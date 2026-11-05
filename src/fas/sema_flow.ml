module State_map = Map.Make (Int)

type binding = Hir.local = { name : string; ty : Hir.ty; id : int }
type selector = Field of string | Element of int
type init_state = Uninit | Full | Unknown | Partial of (selector * init_state) list
type value_fact = { low : int64; high : int64; induction : int option }
type value_state = value_fact option State_map.t

type address_fact =
  | Null_address of int64
  | Function_address of string
  | Object_address of {
      identity : string;
      name : string;
      owner_name : string option;
      writable : bool;
      nullable : bool;
      owner : int option;
      extent : int64;
      offset : int64;
    }
  | Dead_local_address of string

type address_state = address_fact State_map.t
type mask_state = bool list State_map.t
type place_path = Exact of selector list | Dynamic_prefix of selector list
type view_access = Mutable_access | Constant_access | Readonly_access
type deferred_requirement = binding * selector list * Span.t

type checked_defer = {
  requirements : deferred_requirement list;
  effects : (binding * init_state) list;
  value_effects : (binding * value_fact option option) list;
  address_effects : (binding * address_fact option) list;
  falls_through : bool;
}

type loop_init_flow = {
  keep_defer_depth : int;
  label : string option;
  entry_state : snapshot;
  entry_falls_through : bool;
  mutable break_states : snapshot list;
  mutable continue_states : snapshot list;
  induction_binding : int option;
  induction_grid : (int64 * int64) option;
  mutable induction_valid : bool;
  mutable stepping : bool;
}

and snapshot = {
  initialized : init_state State_map.t;
  values : value_state;
  addresses : address_state;
  masks : mask_state;
  value_reachable : bool;
}

type loop = loop_init_flow

type defer_capture = {
  defer_before : snapshot;
  defer_before_falls_through : bool;
  defer_visible_bindings : binding list;
}

type t = {
  structs : Hir.struct_def list;
  locals : (string, binding) Hashtbl.t list ref;
  view_origins : (int, (binding * place_path) option) Hashtbl.t;
  view_accesses : (int, view_access) Hashtbl.t;
  view_readonly_names : (int, string) Hashtbl.t;
  raw_view_types : (int, Hir.ty) Hashtbl.t;
  mutable initialized : init_state State_map.t;
  mutable values : value_state;
  mutable addresses : address_state;
  mutable call_objects : (Span.t * string * int64) list;
  mutable masks : mask_state;
  mutable value_reachable : bool;
  mutable next_binding_id : int;
  mutable loop_depth : int;
  mutable loop_init_flows : loop_init_flow list;
  mutable loop_labels : (string * Span.t) list;
  mutable in_defer : bool;
  mutable collecting_defer : deferred_requirement list option;
  mutable defer_scopes : checked_defer list list;
  mutable falls_through : bool;
  mutable checking_dead : bool;
  mutable recovered : Diag.t list option;
}

let error span message = Error [ Diag.error span message ]

let ( let* ) result continuation =
  match result with
  | Error diagnostics -> Error diagnostics
  | Ok value -> continuation value

let create ?(recover = false) ~initial_scope structs =
  {
    structs;
    locals = ref (if initial_scope then [ Hashtbl.create 8 ] else []);
    view_origins = Hashtbl.create 16;
    view_accesses = Hashtbl.create 16;
    view_readonly_names = Hashtbl.create 16;
    raw_view_types = Hashtbl.create 16;
    initialized = State_map.empty;
    values = State_map.empty;
    addresses = State_map.empty;
    call_objects = [];
    masks = State_map.empty;
    value_reachable = true;
    next_binding_id = 0;
    loop_depth = 0;
    loop_init_flows = [];
    loop_labels = [];
    in_defer = false;
    collecting_defer = None;
    defer_scopes = [];
    falls_through = true;
    checking_dead = false;
    recovered = (if recover then Some [] else None);
  }

let lookup_local name flow =
  let rec find = function
    | [] -> None
    | scope :: rest -> (
        match Hashtbl.find_opt scope name with
        | Some binding -> Some binding
        | None -> find rest)
  in
  find !(flow.locals)

let local_names flow =
  List.concat_map
    (fun scope -> Hashtbl.fold (fun name _ names -> name :: names) scope [])
    !(flow.locals)

let ensure_new_local name flow span =
  let* () =
    if Names.reserved_binding_name name then
      error span (Names.reserved_binding_message name)
    else Ok ()
  in
  let scope =
    match !(flow.locals) with scope :: _ -> scope | [] -> Hashtbl.create 8
  in
  if Hashtbl.mem scope name then error span (Printf.sprintf "duplicate local `%s`" name)
  else Ok ()

let add_local name ty flow span =
  let scope =
    match !(flow.locals) with scope :: _ -> scope | [] -> Hashtbl.create 8
  in
  let* () = ensure_new_local name flow span in
  let id = flow.next_binding_id in
  flow.next_binding_id <- id + 1;
  let binding : binding = { ty; id; name } in
  Hashtbl.replace scope name binding;
  if !(flow.locals) = [] then flow.locals := [ scope ];
  Ok binding

let view_origin flow binding =
  match Hashtbl.find_opt flow.view_origins binding.id with
  | Some origin -> origin
  | None -> Some (binding, Exact [])

let view_access flow binding =
  Option.value ~default:Mutable_access (Hashtbl.find_opt flow.view_accesses binding.id)

let view_readonly_name flow binding =
  Hashtbl.find_opt flow.view_readonly_names binding.id

let is_view flow binding = Hashtbl.mem flow.view_origins binding.id
let raw_view_type flow binding = Hashtbl.find_opt flow.raw_view_types binding.id

let bind_raw_view flow binding element access readonly_name =
  Hashtbl.replace flow.raw_view_types binding.id element;
  Hashtbl.replace flow.view_accesses binding.id access;
  Option.iter
    (fun name -> Hashtbl.replace flow.view_readonly_names binding.id name)
    readonly_name

let invalidate_induction_for_binding flow id =
  if not flow.checking_dead then
    List.iter
      (fun loop ->
        if loop.induction_binding = Some id then loop.induction_valid <- false)
      flow.loop_init_flows

let bind_view flow binding root path access readonly_name =
  let origin =
    match (root, path) with Some root, Some path -> Some (root, path) | _ -> None
  in
  Hashtbl.replace flow.view_origins binding.id origin;
  Hashtbl.replace flow.view_accesses binding.id access;
  Option.iter
    (fun name -> Hashtbl.replace flow.view_readonly_names binding.id name)
    readonly_name;
  Option.iter
    (fun (root, _) ->
      invalidate_induction_for_binding flow root.id;
      flow.values <- State_map.add root.id None flow.values)
    origin

let push flow =
  flow.locals := Hashtbl.create 8 :: !(flow.locals);
  flow.defer_scopes <- [] :: flow.defer_scopes

let pop flow =
  (match !(flow.locals) with
  | scope :: rest ->
      let expired =
        Hashtbl.fold (fun _ binding bindings -> binding :: bindings) scope []
      in
      let expired_ids = List.map (fun binding -> binding.id) expired in
      flow.addresses <-
        State_map.map
          (function
            | Object_address { owner = Some id; owner_name; _ }
              when List.mem id expired_ids ->
                Dead_local_address (Option.value ~default:"" owner_name)
            | address -> address)
          flow.addresses;
      flow.locals := rest;
      Hashtbl.iter
        (fun _ binding ->
          flow.values <- State_map.remove binding.id flow.values;
          flow.addresses <- State_map.remove binding.id flow.addresses;
          flow.masks <- State_map.remove binding.id flow.masks;
          Hashtbl.remove flow.raw_view_types binding.id)
        scope
  | [] -> ());
  match flow.defer_scopes with _ :: rest -> flow.defer_scopes <- rest | [] -> ()

let state_of flow binding =
  Option.value ~default:Uninit (State_map.find_opt binding.id flow.initialized)

let state_usable = function Full | Unknown -> true | Uninit | Partial _ -> false

let child_type flow ty selector =
  match (ty, selector) with
  | Hir.Struct name, Field field ->
      Option.map
        (fun (field : Hir.field) -> field.ty)
        (Sema_types.field_info flow.structs name field)
  | (Hir.Array (_, element) | Hir.Vec (_, element)), Element _ -> Some element
  | _ -> None

let is_vacuous_type flow ty =
  let rec check seen = function
    | Hir.Struct name ->
        if List.mem name seen then false
        else
          Option.value ~default:false
            (Option.map
               (fun (struct_def : Hir.struct_def) ->
                 List.for_all
                   (fun (field : Hir.field) -> check (name :: seen) field.ty)
                   struct_def.fields)
               (List.find_opt
                  (fun (struct_def : Hir.struct_def) -> struct_def.name = name)
                  flow.structs))
    | Hir.Array (length, element) -> length = 0 || check seen element
    | _ -> false
  in
  check [] ty

let rec normalize_state flow ty = function
  | Uninit -> Uninit
  | (Full | Unknown) as state -> state
  | Partial entries ->
      let entries =
        List.filter_map
          (fun (selector, state) ->
            match child_type flow ty selector with
            | None -> None
            | Some child_ty ->
                let state = normalize_state flow child_ty state in
                if state = Uninit then None else Some (selector, state))
          entries
      in
      let complete, any_unknown =
        match ty with
        | Hir.Struct name -> (
            match
              List.find_opt
                (fun (struct_def : Hir.struct_def) -> struct_def.name = name)
                flow.structs
            with
            | None -> (false, false)
            | Some struct_def ->
                if struct_def.is_union then
                  let initialized =
                    List.exists
                      (fun (field : Hir.field) ->
                        match List.assoc_opt (Field field.name) entries with
                        | Some state -> state_usable state
                        | None -> false)
                      struct_def.fields
                  in
                  (initialized, initialized)
                else
                  let all =
                    List.for_all
                      (fun (field : Hir.field) ->
                        match List.assoc_opt (Field field.name) entries with
                        | Some state ->
                            state_usable state || is_vacuous_type flow field.ty
                        | None -> is_vacuous_type flow field.ty)
                      struct_def.fields
                  in
                  let unknown =
                    List.exists
                      (fun (field : Hir.field) ->
                        match List.assoc_opt (Field field.name) entries with
                        | Some Unknown -> true
                        | _ -> false)
                      struct_def.fields
                  in
                  (all, unknown))
        | Hir.Array (length, element_ty) | Hir.Vec (length, element_ty) ->
            let all =
              length >= 0
              &&
              let rec each index =
                if index = length then true
                else
                  match List.assoc_opt (Element index) entries with
                  | Some state when state_usable state -> each (index + 1)
                  | Some _ | None ->
                      if is_vacuous_type flow element_ty then each (index + 1)
                      else false
              in
              each 0
            in
            let unknown =
              List.exists
                (fun (_, state) -> match state with Unknown -> true | _ -> false)
                entries
            in
            (all, unknown)
        | _ -> (false, false)
      in
      if complete then if any_unknown then Unknown else Full
      else if entries = [] then Uninit
      else Partial entries

let rec update_state flow ty state path replacement =
  match path with
  | [] -> replacement
  | selector :: rest -> (
      if state_usable state then state
      else
        let entries =
          match state with Partial entries -> entries | Uninit -> [] | _ -> []
        in
        match child_type flow ty selector with
        | None -> state
        | Some child_ty ->
            let child =
              Option.value ~default:Uninit (List.assoc_opt selector entries)
            in
            let child = update_state flow child_ty child rest replacement in
            let entries =
              List.remove_assoc selector entries |> fun entries ->
              if child = Uninit then entries else (selector, child) :: entries
            in
            normalize_state flow ty (Partial entries))

let set_state flow binding path replacement =
  let state = update_state flow binding.ty (state_of flow binding) path replacement in
  if state = Uninit then
    flow.initialized <- State_map.remove binding.id flow.initialized
  else flow.initialized <- State_map.add binding.id state flow.initialized

let value_of flow binding =
  match binding.ty with
  | Hir.Int _ -> Option.join (State_map.find_opt binding.id flow.values)
  | _ -> None

let value_reachable flow = flow.value_reachable

let proof_checks_enabled flow =
  (not flow.checking_dead) && flow.value_reachable && flow.falls_through

let set_value flow binding value =
  match (binding.ty, State_map.find_opt binding.id flow.values, value) with
  | Hir.Int _, Some None, _ -> ()
  | Hir.Int _, _, Some value ->
      flow.values <- State_map.add binding.id (Some value) flow.values
  | Hir.Int _, _, None -> flow.values <- State_map.remove binding.id flow.values
  | _ -> flow.values <- State_map.remove binding.id flow.values

let forget_value flow binding =
  invalidate_induction_for_binding flow binding.id;
  flow.values <- State_map.add binding.id None flow.values

let address_of flow binding = State_map.find_opt binding.id flow.addresses

let set_address flow binding = function
  | Some address
    when binding.ty = Hir.Addr
         || match binding.ty with Hir.Handle _ -> true | _ -> false ->
      flow.addresses <- State_map.add binding.id address flow.addresses
  | Some _ | None -> flow.addresses <- State_map.remove binding.id flow.addresses

let set_call_object flow span name extent =
  flow.call_objects <-
    (span, name, extent)
    :: List.filter
         (fun (call_span, _, _) -> Span.compare call_span span <> 0)
         flow.call_objects

let call_object flow span =
  List.find_opt
    (fun (call_span, _, _) -> Span.compare call_span span = 0)
    flow.call_objects

let forget_address flow binding =
  flow.addresses <- State_map.remove binding.id flow.addresses

let forget_all_addresses flow = flow.addresses <- State_map.empty

let forget_addresses_on_write flow =
  flow.addresses <- State_map.empty;
  flow.masks <- State_map.empty

let mask_of flow binding = State_map.find_opt binding.id flow.masks

let set_mask flow binding = function
  | Some lanes -> flow.masks <- State_map.add binding.id lanes flow.masks
  | None -> flow.masks <- State_map.remove binding.id flow.masks

let forget_all_masks flow = flow.masks <- State_map.empty
let forget_mask flow binding = flow.masks <- State_map.remove binding.id flow.masks

let forget_all_values flow =
  flow.values <-
    State_map.filter
      (fun _ value ->
        match value with
        | Some { induction = Some id; _ } ->
            List.exists
              (fun loop -> loop.induction_binding = Some id && loop.induction_valid)
              flow.loop_init_flows
        | None -> true
        | Some _ -> false)
      flow.values

let require_state binding path flow span =
  let rec walk ty state = function
    | [] -> if state_usable state || is_vacuous_type flow ty then Ok () else Error ()
    | selector :: rest -> (
        match state with
        | Full | Unknown -> Ok ()
        | Partial entries -> (
            match child_type flow ty selector with
            | Some child_ty ->
                let child =
                  Option.value ~default:Uninit (List.assoc_opt selector entries)
                in
                walk child_ty child rest
            | None -> Error ())
        | Uninit -> (
            match child_type flow ty selector with
            | Some child_ty -> walk child_ty Uninit rest
            | None -> Error ()))
  in
  if walk binding.ty (state_of flow binding) path = Ok () then Ok ()
  else
    match flow.collecting_defer with
    | Some requirements ->
        flow.collecting_defer <- Some ((binding, path, span) :: requirements);
        Ok ()
    | None -> error span (Printf.sprintf "use of uninitialized local `%s`" binding.name)

let state_at_path flow binding path =
  let rec walk ty state = function
    | [] -> state
    | selector :: rest -> (
        match state with
        | Full | Unknown -> state
        | Partial entries -> (
            match child_type flow ty selector with
            | Some child_ty ->
                let child =
                  Option.value ~default:Uninit (List.assoc_opt selector entries)
                in
                walk child_ty child rest
            | None -> Uninit)
        | Uninit -> Uninit)
  in
  walk binding.ty (state_of flow binding) path

let copy_state flow destination destination_path source source_path =
  let source_state = state_at_path flow source source_path in
  set_state flow destination destination_path source_state

let require_place_state binding path flow span =
  match path with
  | Exact path -> require_state binding path flow span
  | Dynamic_prefix path -> (
      match state_at_path flow binding path with
      | Uninit -> require_state binding path flow span
      | Full | Unknown | Partial _ -> Ok ())

let merge_state flow ty left right =
  let rec merge ty left right =
    let result =
      match (left, right) with
      | Uninit, Uninit -> Uninit
      | Uninit, (Full | Unknown) | (Full | Unknown), Uninit -> Unknown
      | Full, Full -> Full
      | Unknown, _ | _, Unknown -> Unknown
      | Full, Partial _ | Partial _, Full -> Unknown
      | Partial entries, Uninit | Uninit, Partial entries ->
          Partial
            (List.filter_map
               (fun (selector, state) ->
                 match child_type flow ty selector with
                 | Some child_ty -> (
                     match merge child_ty Uninit state with
                     | Uninit -> None
                     | state -> Some (selector, state))
                 | None -> None)
               entries)
      | Partial left, Partial right ->
          let selectors = List.map fst left @ List.map fst right in
          let selectors =
            List.fold_left
              (fun accumulated selector ->
                if List.mem selector accumulated then accumulated
                else selector :: accumulated)
              [] selectors
          in
          Partial
            (List.filter_map
               (fun selector ->
                 let left =
                   Option.value ~default:Uninit (List.assoc_opt selector left)
                 in
                 let right =
                   Option.value ~default:Uninit (List.assoc_opt selector right)
                 in
                 match child_type flow ty selector with
                 | Some child_ty -> (
                     match merge child_ty left right with
                     | Uninit -> None
                     | state -> Some (selector, state))
                 | None -> None)
               selectors)
    in
    normalize_state flow ty result
  in
  merge ty left right

let merge_initialized_maps flow left right =
  State_map.merge
    (fun id left right ->
      let rec find = function
        | [] -> None
        | scope :: rest -> (
            match
              Hashtbl.fold
                (fun _ binding result ->
                  match result with
                  | Some _ -> result
                  | None -> if binding.id = id then Some binding else None)
                scope None
            with
            | Some binding -> Some binding
            | None -> find rest)
      in
      match find !(flow.locals) with
      | None -> None
      | Some binding ->
          let state =
            merge_state flow binding.ty
              (Option.value ~default:Uninit left)
              (Option.value ~default:Uninit right)
          in
          if state = Uninit then None else Some state)
    left right

let fact_compare ty left right =
  match ty with
  | Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize) -> Int64.unsigned_compare left right
  | _ -> Int64.compare left right

let find_binding flow id =
  List.find_map
    (fun scope ->
      Hashtbl.fold
        (fun _ binding found -> if binding.id = id then Some binding else found)
        scope None)
    !(flow.locals)

let merge_value_maps flow left right =
  State_map.merge
    (fun id left right ->
      match (find_binding flow id, left, right) with
      | Some binding, Some (Some left), Some (Some right) ->
          Some
            (Some
               {
                 low =
                   (if fact_compare binding.ty left.low right.low <= 0 then left.low
                    else right.low);
                 high =
                   (if fact_compare binding.ty left.high right.high >= 0 then left.high
                    else right.high);
                 induction =
                   (if left.induction = right.induction then left.induction else None);
               })
      | Some _, Some None, _ | Some _, _, Some None -> Some None
      | _ -> None)
    left right

let merge_address_maps left right =
  State_map.merge
    (fun _ left right ->
      match (left, right) with
      | Some left, Some right when left = right -> Some left
      | _ -> None)
    left right

let merge_mask_maps left right =
  State_map.merge (fun _ left right -> if left = right then left else None) left right

let merge_maps (flow : t) (left : snapshot) (right : snapshot) : snapshot =
  let values, value_reachable =
    match (left.value_reachable, right.value_reachable) with
    | true, true -> (merge_value_maps flow left.values right.values, true)
    | true, false -> (left.values, true)
    | false, true -> (right.values, true)
    | false, false -> (State_map.empty, false)
  in
  let addresses =
    match (left.value_reachable, right.value_reachable) with
    | true, true -> merge_address_maps left.addresses right.addresses
    | true, false -> left.addresses
    | false, true -> right.addresses
    | false, false -> State_map.empty
  in
  let masks =
    match (left.value_reachable, right.value_reachable) with
    | true, true -> merge_mask_maps left.masks right.masks
    | true, false -> left.masks
    | false, true -> right.masks
    | false, false -> State_map.empty
  in
  {
    initialized = merge_initialized_maps flow left.initialized right.initialized;
    values;
    addresses;
    masks;
    value_reachable;
  }

let merge_values_into (flow : t) (state : snapshot) (values : snapshot list) : snapshot
    =
  let values = List.filter (fun (state : snapshot) -> state.value_reachable) values in
  let merged =
    match values with
    | [] -> State_map.empty
    | first :: rest ->
        List.fold_left
          (fun facts (next : snapshot) -> merge_value_maps flow facts next.values)
          first.values rest
  in
  let addresses =
    match values with
    | [] -> State_map.empty
    | first :: rest ->
        List.fold_left
          (fun facts (next : snapshot) -> merge_address_maps facts next.addresses)
          first.addresses rest
  in
  let masks =
    match values with
    | [] -> State_map.empty
    | first :: rest ->
        List.fold_left
          (fun facts (next : snapshot) -> merge_mask_maps facts next.masks)
          first.masks rest
  in
  { state with values = merged; addresses; masks; value_reachable = values <> [] }

let widen_values entry result =
  State_map.merge
    (fun _ initial after ->
      match (initial, after) with
      | Some initial, Some after when initial = after -> Some after
      | _ -> None)
    entry result

let widen_loop (entry : snapshot) (result : snapshot) : snapshot =
  let addresses = merge_address_maps entry.addresses result.addresses in
  let masks = merge_mask_maps entry.masks result.masks in
  { result with values = widen_values entry.values result.values; addresses; masks }

let snapshot (flow : t) : snapshot =
  {
    initialized = flow.initialized;
    values = flow.values;
    addresses = flow.addresses;
    masks = flow.masks;
    value_reachable = flow.value_reachable;
  }

let restore (flow : t) (state : snapshot) =
  flow.initialized <- state.initialized;
  flow.values <- state.values;
  flow.addresses <- state.addresses;
  flow.masks <- state.masks;
  flow.value_reachable <- state.value_reachable

let add_init_state flow ty left right =
  let rec add ty left right =
    match (left, right) with
    | Uninit, state | state, Uninit -> state
    | Unknown, _ | _, Unknown -> Unknown
    | Full, _ | _, Full -> Full
    | Partial left, Partial right ->
        let selectors = List.map fst left @ List.map fst right in
        let selectors = List.sort_uniq compare selectors in
        Partial
          (List.filter_map
             (fun selector ->
               match child_type flow ty selector with
               | None -> None
               | Some child_ty ->
                   let left =
                     Option.value ~default:Uninit (List.assoc_opt selector left)
                   in
                   let right =
                     Option.value ~default:Uninit (List.assoc_opt selector right)
                   in
                   let state = add child_ty left right in
                   if state = Uninit then None else Some (selector, state))
             selectors)
  in
  normalize_state flow ty (add ty left right)

let changed_map_effects bindings before after =
  List.filter_map
    (fun binding ->
      let before = State_map.find_opt binding.id before
      and after = State_map.find_opt binding.id after in
      if before = after then None else Some (binding, after))
    bindings

let apply_map_effect map effects =
  List.fold_left
    (fun map (binding, state) ->
      match state with
      | Some state -> State_map.add binding.id state map
      | None -> State_map.remove binding.id map)
    map effects

let apply_defer_effect flow deferred =
  List.iter
    (fun (binding, deferred_state) ->
      let current = state_of flow binding in
      let state = add_init_state flow binding.ty current deferred_state in
      if state = Uninit then
        flow.initialized <- State_map.remove binding.id flow.initialized
      else flow.initialized <- State_map.add binding.id state flow.initialized)
    deferred.effects;
  flow.values <- apply_map_effect flow.values deferred.value_effects;
  flow.addresses <- apply_map_effect flow.addresses deferred.address_effects

let rec validate_defer_list flow = function
  | [] -> Ok true
  | deferred :: rest ->
      let* () =
        Result_list.iter
          (fun (binding, path, span) -> require_state binding path flow span)
          deferred.requirements
      in
      apply_defer_effect flow deferred;
      if deferred.falls_through then validate_defer_list flow rest else Ok false

let validate_defer_scopes flow keep =
  let count = List.length flow.defer_scopes - keep in
  let rec run remaining = function
    | _ when remaining <= 0 -> Ok true
    | [] -> Ok true
    | scope :: rest ->
        let* falls_through = validate_defer_list flow scope in
        if falls_through then run (remaining - 1) rest else Ok false
  in
  run count flow.defer_scopes

let exit_defer_state flow keep =
  let before = snapshot flow in
  let result = validate_defer_scopes flow keep in
  let after = snapshot flow in
  restore flow before;
  let* falls_through = result in
  Ok (if falls_through then Some after else None)

let validate_exit_defers flow keep =
  let* _ = exit_defer_state flow keep in
  Ok ()

let merge_flow_states flow = function
  | [] -> None
  | state :: states -> Some (List.fold_left (merge_maps flow) state states)

let mark_init binding flow = set_state flow binding [] Full

let with_dead_check flow dead check =
  let previous = flow.checking_dead in
  flow.checking_dead <- previous || dead;
  let result = check () in
  flow.checking_dead <- previous;
  result

let checking_dead flow = flow.checking_dead
let set_loop_labels flow labels = flow.loop_labels <- labels
let merge = merge_maps
let falls_through flow = flow.falls_through
let set_falls_through flow value = flow.falls_through <- value

let finish_block_scope flow =
  let result =
    if flow.falls_through then
      match flow.defer_scopes with
      | scope :: _ -> validate_defer_list flow scope
      | [] -> Ok true
    else Ok false
  in
  pop flow;
  let* deferred_falls_through = result in
  flow.falls_through <- flow.falls_through && deferred_falls_through;
  Ok ()

let finish_statement flow ~before ~terminates =
  if not flow.falls_through then
    restore flow { before with initialized = flow.initialized }
  else if terminates then flow.falls_through <- false

let validate_return flow span =
  if flow.in_defer then error span "return is not allowed inside defer" else Ok ()

let check_loop_label flow (name, span) =
  if List.exists (fun loop -> loop.label = Some name) flow.loop_init_flows then
    error span (Printf.sprintf "loop label `%s` shadows an enclosing loop label" name)
  else Ok ()

let begin_loop ?label ?induction_binding ?induction_grid flow =
  let loop =
    {
      keep_defer_depth = List.length flow.defer_scopes;
      label;
      entry_state = snapshot flow;
      entry_falls_through = flow.falls_through;
      break_states = [];
      continue_states = [];
      induction_binding;
      induction_grid;
      induction_valid = Option.is_some induction_binding;
      stepping = false;
    }
  in
  flow.loop_depth <- flow.loop_depth + 1;
  flow.loop_init_flows <- loop :: flow.loop_init_flows;
  flow.falls_through <- loop.entry_falls_through;
  loop

let end_loop flow =
  flow.loop_depth <- flow.loop_depth - 1;
  flow.loop_init_flows <- List.tl flow.loop_init_flows

let finish_while flow loop ~condition_is_true ~condition_is_false =
  let iteration_states =
    (if flow.falls_through then [ snapshot flow ] else []) @ loop.continue_states
  in
  let exit_states =
    if condition_is_true then loop.break_states
    else if condition_is_false then [ loop.entry_state ]
    else loop.entry_state :: (iteration_states @ loop.break_states)
  in
  let result =
    Option.value ~default:loop.entry_state (merge_flow_states flow exit_states)
  in
  restore flow
    (if condition_is_true && iteration_states = [] then result
     else widen_loop loop.entry_state result);
  flow.falls_through <- loop.entry_falls_through && exit_states <> []

let prepare_for_step flow loop ~body_falls_through =
  loop.stepping <- true;
  let step_states =
    if body_falls_through then snapshot flow :: loop.continue_states
    else loop.continue_states
  in
  restore flow
    (Option.value ~default:loop.entry_state (merge_flow_states flow step_states));
  flow.falls_through <- step_states <> []

let finish_for flow loop ~unconditional ~condition_is_false ~single_iteration =
  loop.stepping <- false;
  let exit_states =
    if unconditional then loop.break_states
    else if condition_is_false then [ loop.entry_state ]
    else if single_iteration then
      (if flow.falls_through then [ snapshot flow ] else []) @ loop.break_states
    else loop.entry_state :: snapshot flow :: loop.break_states
  in
  let result =
    Option.value ~default:loop.entry_state (merge_flow_states flow exit_states)
  in
  restore flow (if single_iteration then result else widen_loop loop.entry_state result);
  flow.falls_through <- loop.entry_falls_through && exit_states <> []

let record_loop_exit flow target span kind =
  if flow.in_defer then error span (kind ^ " is not allowed inside defer")
  else
    let target_loop =
      match target with
      | None -> List.nth_opt flow.loop_init_flows 0
      | Some (name, _) ->
          List.find_opt (fun loop -> loop.label = Some name) flow.loop_init_flows
    in
    match (target, target_loop) with
    | Some (name, span), None -> (
        match List.assoc_opt name flow.loop_labels with
        | None -> error span (Printf.sprintf "unknown loop label `%s`" name)
        | Some label_span ->
            Error
              [
                Diag.error
                  ~notes:
                    [
                      Printf.sprintf "loop label `%s` is declared here at %s" name
                        (Span.to_string label_span);
                    ]
                  span
                  (Printf.sprintf "`%s %s` is not inside the loop labeled `%s`" kind
                     name name);
              ])
    | None, None -> error span (kind ^ " outside loop")
    | _, Some _ when not flow.falls_through -> Ok ()
    | _, Some loop ->
        (if not flow.checking_dead then
           let rec invalidate_until_target = function
             | [] -> ()
             | exited :: rest ->
                 exited.induction_valid <- false;
                 if exited == loop then () else invalidate_until_target rest
           in
           invalidate_until_target flow.loop_init_flows);
        let* state = exit_defer_state flow loop.keep_defer_depth in
        Option.iter
          (fun state ->
            if kind = "break" then loop.break_states <- state :: loop.break_states
            else loop.continue_states <- state :: loop.continue_states)
          state;
        Ok ()

let record_break flow target span = record_loop_exit flow target span "break"
let record_continue flow target span = record_loop_exit flow target span "continue"

let invalidate_induction_on_return flow =
  if not flow.checking_dead then
    List.iter (fun loop -> loop.induction_valid <- false) flow.loop_init_flows

let note_binding_write flow id =
  flow.masks <- State_map.remove id flow.masks;
  if
    (not flow.checking_dead)
    && not
         (List.exists
            (fun loop -> loop.induction_binding = Some id && loop.stepping)
            flow.loop_init_flows)
  then invalidate_induction_for_binding flow id

let induction_valid flow id =
  List.exists
    (fun loop -> loop.induction_binding = Some id && loop.induction_valid)
    flow.loop_init_flows

let induction_grid flow id =
  List.find_map
    (fun loop ->
      match loop.induction_grid with
      | Some grid when loop.induction_binding = Some id && loop.induction_valid ->
          Some grid
      | _ -> None)
    flow.loop_init_flows

let begin_defer flow span =
  if flow.in_defer then error span "nested defer is not allowed"
  else
    let visible_bindings =
      List.concat_map
        (fun scope ->
          Hashtbl.fold (fun _ binding bindings -> binding :: bindings) scope [])
        !(flow.locals)
    in
    let capture =
      {
        defer_before = snapshot flow;
        defer_before_falls_through = flow.falls_through;
        defer_visible_bindings = visible_bindings;
      }
    in
    flow.in_defer <- true;
    flow.collecting_defer <- Some [];
    flow.falls_through <- true;
    Ok capture

let finish_defer flow capture checked ~falls_through:body_falls_through =
  let after = snapshot flow in
  let requirements = Option.value ~default:[] flow.collecting_defer |> List.rev in
  flow.in_defer <- false;
  flow.collecting_defer <- None;
  restore flow capture.defer_before;
  flow.falls_through <- capture.defer_before_falls_through;
  let* body = checked in
  let effects =
    List.filter_map
      (fun binding ->
        Option.map
          (fun state -> (binding, state))
          (State_map.find_opt binding.id after.initialized))
      capture.defer_visible_bindings
  in
  let value_effects =
    changed_map_effects capture.defer_visible_bindings capture.defer_before.values
      after.values
  and address_effects =
    changed_map_effects capture.defer_visible_bindings capture.defer_before.addresses
      after.addresses
  in
  (if capture.defer_before_falls_through then
     match flow.defer_scopes with
     | scope :: rest ->
         flow.defer_scopes <-
           ({
              requirements;
              effects;
              value_effects;
              address_effects;
              falls_through = body_falls_through body;
            }
           :: scope)
           :: rest
     | [] -> ());
  Ok body

type checkpoint = {
  state : snapshot;
  locals : (string, binding) Hashtbl.t list;
  view_origins : (int, (binding * place_path) option) Hashtbl.t;
  view_accesses : (int, view_access) Hashtbl.t;
  view_readonly_names : (int, string) Hashtbl.t;
  raw_view_types : (int, Hir.ty) Hashtbl.t;
  call_objects : (Span.t * string * int64) list;
  loop_depth : int;
  loop_init_flows : loop_init_flow list;
  loops : (snapshot list * snapshot list * bool * bool) list;
  in_defer : bool;
  collecting_defer : deferred_requirement list option;
  defer_scopes : checked_defer list list;
  falls_through : bool;
  checking_dead : bool;
}

let recovering flow = Option.is_some flow.recovered

let record flow diagnostics =
  Option.iter
    (fun recorded -> flow.recovered <- Some (List.rev_append diagnostics recorded))
    flow.recovered

let recovered flow = List.rev (Option.value flow.recovered ~default:[])
let recovered_count flow = List.length (Option.value flow.recovered ~default:[])

let map_recovered_since flow count f =
  flow.recovered <-
    Option.map
      (fun recorded ->
        let fresh = List.length recorded - count in
        List.mapi
          (fun index diagnostic -> if index < fresh then f diagnostic else diagnostic)
          recorded)
      flow.recovered

let checkpoint flow =
  {
    state = snapshot flow;
    locals = !(flow.locals);
    view_origins = Hashtbl.copy flow.view_origins;
    view_accesses = Hashtbl.copy flow.view_accesses;
    view_readonly_names = Hashtbl.copy flow.view_readonly_names;
    raw_view_types = Hashtbl.copy flow.raw_view_types;
    call_objects = flow.call_objects;
    loop_depth = flow.loop_depth;
    loop_init_flows = flow.loop_init_flows;
    loops =
      List.map
        (fun loop ->
          (loop.break_states, loop.continue_states, loop.induction_valid, loop.stepping))
        flow.loop_init_flows;
    in_defer = flow.in_defer;
    collecting_defer = flow.collecting_defer;
    defer_scopes = flow.defer_scopes;
    falls_through = flow.falls_through;
    checking_dead = flow.checking_dead;
  }

let rewind flow (saved : checkpoint) =
  let refill table copy =
    Hashtbl.reset table;
    Hashtbl.iter (Hashtbl.replace table) copy
  in
  restore flow saved.state;
  flow.locals := saved.locals;
  refill flow.view_origins saved.view_origins;
  refill flow.view_accesses saved.view_accesses;
  refill flow.view_readonly_names saved.view_readonly_names;
  refill flow.raw_view_types saved.raw_view_types;
  flow.call_objects <- saved.call_objects;
  flow.loop_depth <- saved.loop_depth;
  flow.loop_init_flows <- saved.loop_init_flows;
  List.iter2
    (fun loop (breaks, continues, induction_valid, stepping) ->
      loop.break_states <- breaks;
      loop.continue_states <- continues;
      loop.induction_valid <- induction_valid;
      loop.stepping <- stepping)
    saved.loop_init_flows saved.loops;
  flow.in_defer <- saved.in_defer;
  flow.collecting_defer <- saved.collecting_defer;
  flow.defer_scopes <- saved.defer_scopes;
  flow.falls_through <- saved.falls_through;
  flow.checking_dead <- saved.checking_dead

let forget_facts flow =
  forget_all_values flow;
  forget_all_addresses flow;
  forget_all_masks flow

let recoverable (diagnostic : Diag.t) =
  let mentions needle =
    let size = String.length needle and total = String.length diagnostic.message in
    let rec at index =
      index + size <= total
      && (String.sub diagnostic.message index size = needle || at (index + 1))
    in
    at 0
  in
  not
    (String.starts_with ~prefix:"internal error:" diagnostic.message
    || mentions "(profile ")
