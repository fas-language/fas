(* Function generic arguments (D1, "Fas design survey.md").

   `fn sort[T, LESS fn(T, T) bool](...)` takes a function as a generic
   argument: `sort[Player, by_score](...)`. [desugar] runs before any type is
   resolved and removes every function generic parameter:
   - each function or struct that has one (a template) is copied once per
     distinct list of function arguments (an instance, named after them:
     `sort[by_score]`). In the copy, `LESS(a, b)` calls the argument, `&LESS`
     is its address and `LESS` as a generic argument passes it on;
   - each application names its instance, with the remaining generic
     arguments: `sort[Player, by_score](...)` becomes
     `sort[by_score][Player](...)`, `Map[K, V, h, e]` becomes
     `Map[h, e][K, V]`;
   - a call without generic arguments infers its function arguments from the
     declared types of its arguments (`map_get(m, k)` with `m` declared as a
     `Map[i32, i32, h, e]` or a handle to one); the remaining type arguments
     are inferred as usual;
   - a function argument may mention the caller's generic parameters
     (`sort[T, less[T]]`): the instance takes those as extra hidden
     parameters `$0`, `$1`, ..., so one instance serves every caller;
   - each function instance starts with a hidden check
     (`Names.fn_check_name`), resolved by sema_check: the argument's
     signature must equal the parameter's exactly, `fails` included. A
     struct's function arguments are checked the same way in each function
     that names the struct. *)

exception Bad of Diag.t list

let error span message = raise (Bad [ Diag.error span message ])
let error_help span help message = raise (Bad [ Diag.error ~help span message ])

(* runaway instantiation (`f[LESS]` calling `f[wrap[LESS]]`) stops here *)
let max_instances = 10_000

type template = {
  t_name : string;
  t_params : Ast.generic_param list;
  t_item : Ast.item;
}

type instance = {
  i_name : string;
  i_template : template;
  (* each function parameter's argument, in terms of the instance's own
     generic parameters (the hidden `$i` included) *)
  i_args : (string * Ast.expr) list;
  i_hidden : Ast.generic_param list;
}

let fn_params params =
  List.filter_map (function Ast.Fn_param f -> Some f | _ -> None) params

let has_fn_params params = fn_params params <> []

let param_name = function
  | Ast.Type_param { name; _ } | Ast.Const_param { name; _ } | Ast.Fn_param { name; _ }
    ->
      name

let item_kind = function Ast.Struct _ -> "struct" | _ -> "function"

let generic_signature (t : template) =
  t.t_name ^ "["
  ^ String.concat ", "
      (List.map
         (function
           | Ast.Fn_param f -> f.name ^ " " ^ Ast.fn_param_type_name f
           | p -> param_name p)
         t.t_params)
  ^ "]"

(* prose, not code: the parameter list is not something to paste back *)
let declared_help t =
  Printf.sprintf "write its generic arguments; it is declared as %s"
    (generic_signature t)

(* the function an argument for a function parameter names: `f`, `f[T]` or a
   C function *)
let rec fn_expr span = function
  | Ast.Name_arg (n, s) -> Ast.Ident (n, s)
  | Ast.Type_arg (Ast.Named_type (n, s)) | Ast.Type_or_index (Ast.Named_type (n, s)) ->
      Ast.Ident (n, s)
  | Ast.Type_arg (Ast.Applied_type (n, s, args, s2))
  | Ast.Type_or_index (Ast.Applied_type (n, s, args, s2)) ->
      Ast.Generic_args (Ast.Ident (n, s), args, s2)
  | Ast.Const_arg ((Ast.Ident _ | Ast.Generic_args (Ast.Ident _, _, _)) as e) -> e
  | Ast.Const_arg (Ast.Select (Ast.Ident (n, s), args, s2)) ->
      Ast.Generic_args (Ast.Ident (n, s), args, s2)
  | Ast.Const_arg (Ast.Parenthesized (e, _)) -> fn_expr span (Ast.Const_arg e)
  | Ast.Const_arg e ->
      error (Ast.expr_span e)
        "a function argument names a function: `f`, `f[T]` or a C function"
  | Ast.Type_arg _ | Ast.Type_or_index _ ->
      error span "a function argument names a function: `f`, `f[T]` or a C function"

(* the head name of an argument written for a function parameter *)
let fn_param_reference = function
  | Ast.Name_arg (n, _)
  | Ast.Type_arg (Ast.Named_type (n, _))
  | Ast.Type_or_index (Ast.Named_type (n, _))
  | Ast.Const_arg (Ast.Ident (n, _)) ->
      Some n
  | _ -> None

let rec split_at n = function
  | l when n <= 0 -> ([], l)
  | [] -> ([], [])
  | x :: rest ->
      let a, b = split_at (n - 1) rest in
      (x :: a, b)

(* Replaces each of the owner's generic parameters that [exprs] mention by a
   hidden parameter `$i`, numbered by first mention. Returns the rewritten
   expressions, the hidden parameters and the owner's arguments for them. *)
let abstract owner exprs =
  let declared name =
    List.find_opt (function Ast.Fn_param _ -> false | p -> param_name p = name) owner
  in
  let is_type name =
    match declared name with Some (Ast.Type_param _) -> true | _ -> false
  in
  let is_const name =
    match declared name with Some (Ast.Const_param _) -> true | _ -> false
  in
  let seen = ref [] in
  let hidden name =
    match List.assoc_opt name !seen with
    | Some h -> h
    | None ->
        let h = "$" ^ string_of_int (List.length !seen) in
        seen := !seen @ [ (name, h) ];
        h
  in
  let mapper =
    {
      Ast.identity_mapper with
      on_ty =
        (function
        | Ast.Named_type (n, s) when is_type n -> Some (Ast.Named_type (hidden n, s))
        | _ -> None);
      on_arg =
        (function
        | Ast.Name_arg (n, s) when Option.is_some (declared n) ->
            Some (Ast.Name_arg (hidden n, s))
        | _ -> None);
      on_expr =
        (function
        | Ast.Ident (n, s) when is_const n -> Some (Ast.Ident (hidden n, s))
        | _ -> None);
    }
  in
  let exprs = List.map (Ast.map_expr mapper) exprs in
  let params, args =
    List.split
      (List.map
         (fun (n, h) ->
           match declared n with
           | Some (Ast.Type_param { span; _ }) ->
               ( Ast.Type_param { name = h; span },
                 Ast.Type_arg (Ast.Named_type (n, span)) )
           | Some (Ast.Const_param cp) ->
               ( Ast.Const_param { cp with name = h },
                 Ast.Const_arg (Ast.Ident (n, cp.span)) )
           | Some (Ast.Fn_param _) | None -> assert false)
         !seen)
  in
  (exprs, params, args)

(* [e] with the hidden parameters `$i` replaced by [args] *)
let concretize args e =
  let hidden name =
    if String.length name > 1 && name.[0] = '$' then
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some i -> List.nth_opt args i
      | None -> None
    else None
  in
  let mapper =
    {
      Ast.identity_mapper with
      on_ty =
        (function
        | Ast.Named_type (n, _) -> (
            match hidden n with
            | Some (Ast.Type_arg t | Ast.Type_or_index t) -> Some t
            | Some (Ast.Name_arg (m, s)) -> Some (Ast.Named_type (m, s))
            | _ -> None)
        | _ -> None);
      on_arg = (function Ast.Name_arg (n, _) -> hidden n | _ -> None);
      on_expr =
        (function
        | Ast.Ident (n, _) -> (
            match hidden n with
            | Some (Ast.Const_arg e) -> Some e
            | Some (Ast.Name_arg (m, s)) -> Some (Ast.Ident (m, s))
            | _ -> None)
        | _ -> None);
    }
  in
  Ast.map_expr mapper e

(* `.fn_check NAME[R, P1, ..., fails](&f)` *)
let signature_check (f : Ast.fn_param) ~substitute target span =
  let types = List.map (fun t -> Ast.Type_arg (substitute t)) (f.ret :: f.params) in
  let fails = if f.fails then [ Ast.Name_arg ("fails", span) ] else [] in
  Ast.Expr_stmt
    ( Ast.Call
        ( Ast.Generic_args
            (Ast.Ident (Names.fn_check_name f.name, span), types @ fails, span),
          [ Ast.Addr_of (target, span) ],
          span ),
      span,
      span )

type context = {
  owner : Ast.generic_param list;
  mutable scope : (string * Ast.ty) list;
  (* struct signature checks for this function, newest first, by key *)
  mutable checks : (string * Ast.stmt) list;
  in_function : bool;
}

let desugar (program : Ast.program) =
  let templates = Hashtbl.create 16 in
  List.iter
    (function
      | (Ast.Func { name; generic_params; _ } | Ast.Struct { name; generic_params; _ })
        as item
        when has_fn_params generic_params ->
          Hashtbl.replace templates name
            { t_name = name; t_params = generic_params; t_item = item }
      | _ -> ())
    program.Ast.items;
  if Hashtbl.length templates = 0 then Ok program
  else
    try
      (* the templates' own rules *)
      Hashtbl.iter
        (fun _ t ->
          let names = List.map param_name t.t_params in
          List.iter
            (function
              | Ast.Fn_param f ->
                  if Names.reserved_binding_name f.name then
                    error f.span (Names.reserved_binding_message f.name);
                  if List.length (List.filter (( = ) f.name) names) > 1 then
                    error f.span
                      (Printf.sprintf "duplicate generic parameter `%s`" f.name)
              | _ -> ())
            t.t_params;
          match t.t_item with
          | Ast.Func { linkage = Ast.External_c; name_span; _ } ->
              error name_span "an `extern` function cannot take function arguments"
          | Ast.Func { body = Ast.Declaration; name_span; _ } ->
              error name_span "a function that takes function arguments needs a body"
          | Ast.Func { params; _ } ->
              List.iter
                (fun (p : Ast.param) ->
                  List.iter
                    (fun f ->
                      if f.Ast.name = p.name then
                        error p.span
                          (Printf.sprintf
                             "parameter `%s` conflicts with a generic parameter" p.name))
                    (fn_params t.t_params))
                params
          | _ -> ())
        templates;
      let template name = Hashtbl.find_opt templates name in
      let is_fn_template name =
        match template name with Some { t_item = Ast.Func _; _ } -> true | _ -> false
      in
      let is_struct_template name =
        match template name with
        | Some { t_item = Ast.Struct _; _ } -> true
        | _ -> false
      in
      let instances = Hashtbl.create 16 and by_name = Hashtbl.create 16 in
      let queue = Queue.create () in
      let globals = Hashtbl.create 16 in
      let fn_returns = Hashtbl.create 16 in
      List.iter
        (function
          | Ast.Global { name; ty; _ } -> Hashtbl.replace globals name ty
          | Ast.Func { name; generic_params = []; ret; _ } ->
              Hashtbl.replace fn_returns name ret
          | _ -> ())
        program.Ast.items;
      let needs_arguments name span =
        let t = Option.get (template name) in
        error_help span (declared_help t)
          (Printf.sprintf
             "%s `%s` needs its generic arguments, function arguments included"
             (item_kind t.t_item) name)
      in
      (* the copy of [t] for [args] (in terms of [hidden]) *)
      let instantiate t args hidden span =
        let key =
          t.t_name ^ "("
          ^ String.concat "; " (List.map Ast.expr_name args)
          ^ ")"
          ^ String.concat ""
              (List.map
                 (function
                   | Ast.Const_param cp -> " c:" ^ Ast.type_name cp.ty | _ -> " t")
                 hidden)
        in
        match Hashtbl.find_opt instances key with
        | Some i -> i
        | None ->
            if Hashtbl.length instances >= max_instances then
              error span
                (Printf.sprintf
                   "more than %d copies of functions with function arguments; does a \
                    function argument grow without end?"
                   max_instances);
            let base =
              t.t_name ^ "[" ^ String.concat ", " (List.map Ast.expr_name args) ^ "]"
            in
            let rec unique n =
              let name = if n = 0 then base else base ^ "#" ^ string_of_int n in
              if Hashtbl.mem by_name name || Hashtbl.mem templates name then
                unique (n + 1)
              else name
            in
            let name = unique 0 in
            let bound =
              List.combine (List.map (fun f -> f.Ast.name) (fn_params t.t_params)) args
            in
            let bound_arg n = List.assoc_opt n bound in
            let rec m =
              {
                Ast.identity_mapper with
                on_expr =
                  (function
                  | Ast.Call (Ast.Ident (n, _), xs, s) when Option.is_some (bound_arg n)
                    ->
                      Some
                        (Ast.Call
                           (Option.get (bound_arg n), List.map (Ast.map_expr m) xs, s))
                  | Ast.Addr_of (Ast.Ident (n, _), s) when Option.is_some (bound_arg n)
                    ->
                      Some (Ast.Addr_of (Option.get (bound_arg n), s))
                  | Ast.Ident (n, s) when Option.is_some (bound_arg n) ->
                      error s
                        (Printf.sprintf
                           "function parameter `%s` must be called (`%s(...)`), have \
                            its address taken (`&%s`) or be passed as a generic \
                            argument"
                           n n n)
                  | _ -> None);
                on_arg =
                  (fun a ->
                    match fn_param_reference a with
                    | Some n when Option.is_some (bound_arg n) ->
                        Some (Ast.Const_arg (Option.get (bound_arg n)))
                    | _ -> None);
                on_ty =
                  (function
                  | Ast.Named_type (n, s) when Option.is_some (bound_arg n) ->
                      error s (Printf.sprintf "function parameter `%s` is not a type" n)
                  | _ -> None);
              }
            in
            let generic_params =
              List.filter (function Ast.Fn_param _ -> false | _ -> true) t.t_params
              @ hidden
            in
            let item =
              match Ast.map_item m t.t_item with
              | Ast.Func f ->
                  let checks =
                    List.map2
                      (fun fp arg ->
                        signature_check fp ~substitute:(Ast.map_ty m) arg
                          (Ast.expr_span arg))
                      (fn_params t.t_params) args
                  in
                  let body =
                    match f.body with
                    | Ast.Statements body -> Ast.Statements (checks @ body)
                    | Ast.Declaration -> Ast.Declaration
                  in
                  Ast.Func { f with name; generic_params; body }
              | Ast.Struct s -> Ast.Struct { s with name; generic_params }
              | _ -> assert false
            in
            let i =
              { i_name = name; i_template = t; i_args = bound; i_hidden = hidden }
            in
            Hashtbl.replace instances key i;
            Hashtbl.replace by_name name i;
            Queue.push item queue;
            i
      in
      let local ctx name = List.mem_assoc name ctx.scope in
      (* splits [args] for [t] into its kept arguments and its function
         arguments *)
      let split_arguments ctx t args span =
        let expected = List.length t.t_params and given = List.length args in
        if expected <> given then
          error_help span (declared_help t)
            (Sema_types.generic_arity_message
               ("generic " ^ item_kind t.t_item)
               t.t_name expected given);
        ignore ctx;
        List.fold_right2
          (fun p a (kept, fns) ->
            match p with
            | Ast.Fn_param _ -> (kept, fn_expr span a :: fns)
            | _ -> (a :: kept, fns))
          t.t_params args ([], [])
      in
      let rec mapper ctx =
        {
          Ast.identity_mapper with
          on_ty =
            (function
            | Ast.Applied_type (n, s, args, s2) when is_struct_template n ->
                let args = List.map (rw_arg ctx) args in
                Some (apply_struct ctx (Option.get (template n)) s args s2)
            | Ast.Named_type (n, s) when Option.is_some (template n) ->
                needs_arguments n s
            | _ -> None);
          on_arg =
            (function
            | Ast.Type_arg (Ast.Applied_type (n, s, args, s2))
            | Ast.Type_or_index (Ast.Applied_type (n, s, args, s2))
              when is_fn_template n ->
                Some
                  (Ast.Const_arg
                     (rw_expr ctx (Ast.Generic_args (Ast.Ident (n, s), args, s2))))
            | Ast.Name_arg (n, s) when Option.is_some (template n) && not (local ctx n)
              ->
                needs_arguments n s
            | _ -> None);
          on_expr =
            (function
            | Ast.Call (Ast.Generic_args (Ast.Ident (n, ns), args, gs), xs, cs)
              when is_fn_template n && not (local ctx n) ->
                let callee = apply_fn ctx (Option.get (template n)) ns args gs in
                Some (Ast.Call (callee, List.map (rw_expr ctx) xs, cs))
            | Ast.Generic_args (Ast.Ident (n, ns), args, gs)
            | Ast.Select (Ast.Ident (n, ns), args, gs)
              when is_fn_template n && not (local ctx n) ->
                Some (apply_fn ctx (Option.get (template n)) ns args gs)
            | Ast.Call (Ast.Ident (n, ns), xs, cs)
              when is_fn_template n && not (local ctx n) ->
                let xs = List.map (rw_expr ctx) xs in
                Some (Ast.Call (infer ctx (Option.get (template n)) ns xs cs, xs, cs))
            | Ast.Ident (n, s) when Option.is_some (template n) && not (local ctx n) ->
                needs_arguments n s
            | Ast.Catch (x, Ast.Catch_block (binding, body), s) ->
                let x = rw_expr ctx x in
                let saved = ctx.scope in
                Option.iter
                  (fun (b, _) -> ctx.scope <- (b, Ast.Void) :: ctx.scope)
                  binding;
                let body = rw_block ctx body in
                ctx.scope <- saved;
                Some (Ast.Catch (x, Ast.Catch_block (binding, body), s))
            | _ -> None);
        }
      and rw_ty ctx t = Ast.map_ty (mapper ctx) t
      and rw_arg ctx a = Ast.map_arg (mapper ctx) a
      and rw_expr ctx e = Ast.map_expr (mapper ctx) e
      and apply_fn ctx t ns args gs =
        let args = List.map (rw_arg ctx) args in
        let kept, fns = split_arguments ctx t args gs in
        let fns = List.map (rw_expr ctx) fns in
        let exprs, hidden, hidden_args = abstract ctx.owner fns in
        let i = instantiate t exprs hidden gs in
        match kept @ hidden_args with
        | [] -> Ast.Ident (i.i_name, ns)
        | rest -> Ast.Generic_args (Ast.Ident (i.i_name, ns), rest, gs)
      and apply_struct ctx t s args s2 =
        let kept, fns = split_arguments ctx t args s2 in
        let fns = List.map (rw_expr ctx) fns in
        let exprs, hidden, hidden_args = abstract ctx.owner fns in
        let i = instantiate t exprs hidden s2 in
        if ctx.in_function then struct_checks ctx t i kept fns;
        match kept @ hidden_args with
        | [] -> Ast.Named_type (i.i_name, s)
        | rest -> Ast.Applied_type (i.i_name, s, rest, s2)
      (* the struct's function arguments, checked against its parameters'
         signatures with the struct's other parameters as [kept] *)
      and struct_checks ctx t i kept fns =
        let key =
          i.i_name ^ "["
          ^ String.concat ", " (List.map Ast.generic_arg_name kept)
          ^ "]"
          ^ String.concat "; " (List.map Ast.expr_name fns)
        in
        if not (List.mem_assoc key ctx.checks) then (
          (* registered before rewriting, so a signature naming the struct
             itself stops here *)
          ctx.checks <- (key, Ast.Block ([], Span.synthetic)) :: ctx.checks;
          let others =
            List.filter (function Ast.Fn_param _ -> false | _ -> true) t.t_params
          in
          let bound = List.combine (List.map param_name others) kept in
          let fn_bound =
            List.combine (List.map (fun f -> f.Ast.name) (fn_params t.t_params)) fns
          in
          let m =
            {
              Ast.identity_mapper with
              on_ty =
                (function
                | Ast.Named_type (n, _) -> (
                    match List.assoc_opt n bound with
                    | Some (Ast.Type_arg t | Ast.Type_or_index t) -> Some t
                    | Some (Ast.Name_arg (x, s)) -> Some (Ast.Named_type (x, s))
                    | _ -> None)
                | _ -> None);
              on_arg =
                (fun a ->
                  match fn_param_reference a with
                  | Some n when List.mem_assoc n bound -> Some (List.assoc n bound)
                  | Some n when List.mem_assoc n fn_bound ->
                      Some (Ast.Const_arg (List.assoc n fn_bound))
                  | _ -> None);
              on_expr =
                (function
                | Ast.Ident (n, _) -> (
                    match List.assoc_opt n bound with
                    | Some (Ast.Const_arg e) -> Some e
                    | Some (Ast.Name_arg (x, s)) -> Some (Ast.Ident (x, s))
                    | _ -> None)
                | _ -> None);
            }
          in
          let stmts =
            List.map2
              (fun fp fn ->
                signature_check fp
                  ~substitute:(fun ty -> rw_ty ctx (Ast.map_ty m ty))
                  fn (Ast.expr_span fn))
              (fn_params t.t_params) fns
          in
          ctx.checks <-
            List.map (fun st -> (key, st)) (List.rev stmts)
            @ List.filter (fun (k, _) -> k <> key) ctx.checks)
        else ignore i
      (* the instance a call without generic arguments names, its function
         arguments read from its arguments' declared types *)
      and infer ctx t ns xs cs =
        let params = match t.t_item with Ast.Func f -> f.params | _ -> [] in
        let wanted = List.map (fun f -> f.Ast.name) (fn_params t.t_params) in
        let found = ref [] in
        let bind n e =
          match List.assoc_opt n !found with
          | Some previous when Ast.expr_name previous <> Ast.expr_name e ->
              error cs
                (Printf.sprintf
                   "the arguments of `%s` disagree on its function argument `%s`: `%s` \
                    or `%s`"
                   t.t_name n (Ast.expr_name previous) (Ast.expr_name e))
          | Some _ -> ()
          | None -> found := !found @ [ (n, e) ]
        in
        let rec go p a =
          match (p, a) with
          | Ast.Handle p, Ast.Handle a | Ast.Secret p, Ast.Secret a -> go p a
          | Ast.Handle p, a | Ast.Secret p, a -> go p a
          | Ast.Array (_, p), Ast.Array (_, a) | Ast.Vec (_, p), Ast.Vec (_, a) ->
              go p a
          | ( Ast.Applied_type (n, _, pargs, _),
              (Ast.Applied_type (m, _, _, _) | Ast.Named_type (m, _)) ) -> (
              let aargs = match a with Ast.Applied_type (_, _, x, _) -> x | _ -> [] in
              match Hashtbl.find_opt by_name m with
              | Some i
                when i.i_template.t_name = n
                     && List.length pargs = List.length i.i_template.t_params ->
                  let kept_a, hidden_a =
                    split_at (List.length aargs - List.length i.i_hidden) aargs
                  in
                  let rec walk params pargs kept =
                    match (params, pargs) with
                    | Ast.Fn_param f :: params, parg :: pargs ->
                        (match fn_param_reference parg with
                        | Some r when List.mem r wanted ->
                            bind r
                              (concretize hidden_a (List.assoc f.Ast.name i.i_args))
                        | _ -> ());
                        walk params pargs kept
                    | _ :: params, parg :: pargs -> (
                        match kept with
                        | aarg :: kept ->
                            go_arg parg aarg;
                            walk params pargs kept
                        | [] -> ())
                    | _ -> ()
                  in
                  walk i.i_template.t_params pargs kept_a
              | _ when n = m && List.length pargs = List.length aargs ->
                  List.iter2 go_arg pargs aargs
              | _ -> ())
          | _ -> ()
        and go_arg p a =
          match (p, a) with
          | ( (Ast.Type_arg p | Ast.Type_or_index p),
              (Ast.Type_arg a | Ast.Type_or_index a) ) ->
              go p a
          | _ -> ()
        in
        let arg_ty = expr_ty ctx in
        let rec each params xs =
          match (params, xs) with
          | (p : Ast.param) :: params, x :: xs ->
              Option.iter (go p.ty) (arg_ty x);
              each params xs
          | _ -> ()
        in
        each params xs;
        let fns =
          List.map
            (fun n ->
              match List.assoc_opt n !found with
              | Some e -> e
              | None ->
                  error_help cs (declared_help t)
                    (Printf.sprintf
                       "cannot infer the function argument `%s` of `%s` from its \
                        arguments' declared types"
                       n t.t_name))
            wanted
        in
        let exprs, hidden, _ = abstract ctx.owner fns in
        let i = instantiate t exprs hidden cs in
        Ast.Ident (i.i_name, ns)
      (* the type an argument has on its face: a declared variable, `&x`,
         `handle_from_addr[T](p)` or a cast *)
      and expr_ty ctx = function
        | Ast.Ident (n, _) -> (
            match List.assoc_opt n ctx.scope with
            | Some Ast.Void -> None
            | Some ty -> Some ty
            | None ->
                Option.map
                  (rw_ty { owner = []; scope = []; checks = []; in_function = false })
                  (Hashtbl.find_opt globals n))
        | Ast.Parenthesized (e, _) -> expr_ty ctx e
        | Ast.Addr_of (e, _) -> Option.map (fun t -> Ast.Handle t) (expr_ty ctx e)
        | Ast.Handle_from_addr (t, _, _) -> Some (Ast.Handle t)
        | Ast.Cast (_, t, _, _) -> Some t
        | Ast.Call (Ast.Ident (n, _), _, _) ->
            Option.map
              (rw_ty { owner = []; scope = []; checks = []; in_function = false })
              (Hashtbl.find_opt fn_returns n)
        | _ -> None
      and rw_block ctx body =
        let saved = ctx.scope in
        let body = List.map (rw_stmt ctx) body in
        ctx.scope <- saved;
        body
      and declare ctx name ty = ctx.scope <- (name, ty) :: ctx.scope
      and rw_target ctx = function
        | Ast.Target_ident _ as t -> t
        | Ast.Target_select (a, args) ->
            Ast.Target_select (rw_expr ctx a, List.map (rw_arg ctx) args)
        | Ast.Target_field (a, n, s) -> Ast.Target_field (rw_expr ctx a, n, s)
      and rw_stmt ctx st =
        let ex = rw_expr ctx and block = rw_block ctx in
        match st with
        | Ast.Let l ->
            let ty = rw_ty ctx l.ty and init = Option.map ex l.init in
            declare ctx l.name ty;
            Ast.Let { l with ty; init }
        | Ast.View v ->
            let place = ex v.place in
            declare ctx v.name Ast.Void;
            Ast.View { v with place }
        | Ast.Assign (t, e, s) -> Ast.Assign (rw_target ctx t, ex e, s)
        | Ast.Compound_assign (t, op, e, s1, s2) ->
            Ast.Compound_assign (rw_target ctx t, op, ex e, s1, s2)
        | Ast.Return (e, s) -> Ast.Return (Option.map ex e, s)
        | Ast.If (c, a, b, s) ->
            let c = ex c in
            Ast.If (c, block a, Option.map block b, s)
        | Ast.While (label, c, body, s) ->
            let c = ex c in
            Ast.While (label, c, block body, s)
        | (Ast.Break _ | Ast.Continue _) as st -> st
        | Ast.Defer (body, s) -> Ast.Defer (block body, s)
        | Ast.Expr_stmt (e, s1, s2) -> Ast.Expr_stmt (ex e, s1, s2)
        | Ast.Block (body, s) -> Ast.Block (block body, s)
        | Ast.For (label, init, c, step, body, s) ->
            let saved = ctx.scope in
            let init = Option.map (rw_stmt ctx) init in
            let c = Option.map ex c in
            let step = Option.map (rw_stmt ctx) step in
            let body = block body in
            ctx.scope <- saved;
            Ast.For (label, init, c, step, body, s)
        | Ast.Switch (e, arms, default, s) ->
            let e = ex e in
            Ast.Switch
              ( e,
                List.map (fun (labels, body) -> (List.map ex labels, block body)) arms,
                Option.map block default,
                s )
        | Ast.Const_for c ->
            let ty = rw_ty ctx c.ty in
            let low = ex c.low and high = ex c.high in
            let saved = ctx.scope in
            declare ctx c.name ty;
            let body = block c.body in
            ctx.scope <- saved;
            Ast.Const_for { c with ty; low; high; body }
        | Ast.Const_copy c ->
            let ty = rw_ty ctx c.ty in
            let saved = ctx.scope in
            declare ctx c.name ty;
            let body = block c.body in
            ctx.scope <- saved;
            Ast.Const_copy { c with ty; body }
        | Ast.Fail (e, s) -> Ast.Fail (ex e, s)
        | Ast.Const_assert_stmt c -> Ast.Const_assert_stmt { c with cond = ex c.cond }
        | Ast.Errdefer (binding, body, s) ->
            let saved = ctx.scope in
            Option.iter (fun (b, _) -> declare ctx b Ast.Void) binding;
            let body = block body in
            ctx.scope <- saved;
            Ast.Errdefer (binding, body, s)
        | Ast.Catch_stmt c ->
            let call = ex c.call in
            let saved = ctx.scope in
            Option.iter (fun (b, _) -> declare ctx b Ast.Void) c.binding;
            let body = block c.body in
            ctx.scope <- saved;
            let target =
              match c.target with
              | Ast.Catch_let l ->
                  let ty = rw_ty ctx l.ty in
                  declare ctx l.name ty;
                  Ast.Catch_let { l with ty }
              | Ast.Catch_assign t -> Ast.Catch_assign (rw_target ctx t)
              | (Ast.Catch_discard | Ast.Catch_return) as t -> t
            in
            Ast.Catch_stmt { c with target; call; body }
      in
      let rw_generic_params ctx params =
        List.map
          (function
            | Ast.Const_param cp -> Ast.Const_param { cp with ty = rw_ty ctx cp.ty }
            | p -> p)
          params
      in
      let top () = { owner = []; scope = []; checks = []; in_function = false } in
      let rw_item = function
        | Ast.Func f ->
            let ctx =
              {
                owner = f.generic_params;
                scope = [];
                checks = [];
                in_function =
                  (match f.body with Ast.Statements _ -> true | _ -> false);
              }
            in
            let params =
              List.map (fun (p : Ast.param) -> { p with ty = rw_ty ctx p.ty }) f.params
            in
            List.iter (fun (p : Ast.param) -> declare ctx p.name p.ty) params;
            let ret = rw_ty ctx f.ret in
            let generic_params = rw_generic_params ctx f.generic_params in
            let body =
              match f.body with
              | Ast.Declaration -> Ast.Declaration
              | Ast.Statements body ->
                  let body = rw_block ctx body in
                  let checks =
                    List.rev_map snd ctx.checks
                    |> List.filter (function Ast.Block ([], _) -> false | _ -> true)
                  in
                  Ast.Statements (checks @ body)
            in
            Ast.Func { f with params; ret; generic_params; body }
        | Ast.Struct s ->
            let ctx = { (top ()) with owner = s.generic_params } in
            Ast.Struct
              {
                s with
                generic_params = rw_generic_params ctx s.generic_params;
                fields =
                  List.map
                    (fun (f : Ast.field) -> { f with ty = rw_ty ctx f.ty })
                    s.fields;
              }
        | Ast.Const c ->
            let ctx = top () in
            Ast.Const { c with ty = rw_ty ctx c.ty; value = rw_expr ctx c.value }
        | Ast.Const_assert c -> Ast.Const_assert { c with cond = rw_expr (top ()) c.cond }
        | Ast.Global g ->
            let ctx = top () in
            Ast.Global
              { g with ty = rw_ty ctx g.ty; init = Option.map (rw_expr ctx) g.init }
        | Ast.Error_decl e ->
            let ctx = top () in
            Ast.Error_decl
              {
                e with
                names =
                  List.map (fun (n, s, v) -> (n, s, Option.map (rw_expr ctx) v)) e.names;
              }
        | (Ast.Use _ | Ast.Pkg _ | Ast.Opaque _ | Ast.Type_alias _ | Ast.Enum _) as i -> i
      in
      let items =
        List.filter_map
          (function
            | (Ast.Func { name; _ } | Ast.Struct { name; _ })
              when Hashtbl.mem templates name ->
                None
            | i -> Some (rw_item i))
          program.Ast.items
      in
      let rec drain acc =
        if Queue.is_empty queue then List.rev acc
        else
          let item = Queue.pop queue in
          drain (rw_item item :: acc)
      in
      Ok { Ast.items = items @ drain [] }
    with Bad diagnostics -> Error diagnostics
