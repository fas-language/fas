(* C namespaces, source side (Fas v2 decisions, "C namespaces").

   Every `use "C"` names an alias: `use "C" <stdio.h> as c`. A file reaches C
   only through its own aliases: `c.printf`, `c.EOF`, `handle[c.FILE]`. This
   pass runs on the loaded program, before the C import:

   - it checks the aliases: an alias may not share a name with a Fas top-level
     name, and no local or parameter in that file may reuse it;
   - it rewrites `alias.NAME` (a value, a type, a generic argument or an
     assignment target) to the internal C name `c.NAME`
     (Names.c_prefix; C_import.namespace gives the imported items the same
     names);
   - it records each reference, so the driver imports exactly the C names the
     program uses and checks that each file only uses C its own headers
     declare.

   All aliases of one file share the file's one C scope: the driver builds one
   C unit per file. *)

type c_ref = { file : string; name : string; span : Span.t }

let item_file item = (Ast.item_span item).Span.file

(* the aliases of each file, in order of appearance *)
let file_aliases items =
  let table = Hashtbl.create 16 in
  List.iter
    (function
      | Ast.Use { alias = Some (name, span); _ } ->
          let existing =
            Option.value ~default:[] (Hashtbl.find_opt table span.Span.file)
          in
          if not (List.mem_assoc name existing) then
            Hashtbl.replace table span.Span.file (existing @ [ (name, span) ])
      | _ -> ())
    items;
  table

let top_level_names items =
  List.concat_map
    (function
      | Ast.Const { name; name_span; _ }
      | Ast.Global { name; name_span; _ }
      | Ast.Struct { name; name_span; _ }
      | Ast.Opaque { name; name_span; _ }
      | Ast.Type_alias { name; name_span; _ }
      | Ast.Enum { name; name_span; _ }
      | Ast.Func { name; name_span; _ } ->
          [ (name, name_span) ]
      | Ast.Error_decl { names; _ } -> List.map (fun (n, s, _) -> (n, s)) names
      | Ast.Use _ | Ast.Pkg _ | Ast.Const_assert _ -> [])
    items

(* names a function body binds: parameters, `let`, `view`, `for const`,
   `catch` bindings *)
let rec bound_in_stmts acc stmts = List.fold_left bound_in_stmt acc stmts

and bound_in_stmt acc = function
  | Ast.Let { name; span; _ } -> (name, span) :: acc
  | Ast.View { name; name_span; _ } -> (name, name_span) :: acc
  | Ast.Const_for { name; name_span; body; _ } ->
      bound_in_stmts ((name, name_span) :: acc) body
  | Ast.Const_copy { body; _ } | Ast.Defer (body, _) | Ast.Block (body, _) ->
      bound_in_stmts acc body
  | Ast.While (_, _, body, _) -> bound_in_stmts acc body
  | Ast.If (_, yes, no, _) ->
      bound_in_stmts (bound_in_stmts acc yes) (Option.value ~default:[] no)
  | Ast.For (_, init, _, step, body, _) ->
      let acc = Option.fold ~none:acc ~some:(bound_in_stmt acc) init in
      let acc = Option.fold ~none:acc ~some:(bound_in_stmt acc) step in
      bound_in_stmts acc body
  | Ast.Switch (_, arms, default, _) ->
      let acc =
        List.fold_left (fun acc (_, body) -> bound_in_stmts acc body) acc arms
      in
      bound_in_stmts acc (Option.value ~default:[] default)
  | Ast.Errdefer (binding, body, _) ->
      bound_in_stmts (Option.fold ~none:acc ~some:(fun b -> b :: acc) binding) body
  | Ast.Catch_stmt { target; binding; body; span; _ } ->
      let acc =
        match target with Ast.Catch_let { name; _ } -> (name, span) :: acc | _ -> acc
      in
      bound_in_stmts (Option.fold ~none:acc ~some:(fun b -> b :: acc) binding) body
  | Ast.Assign _ | Ast.Compound_assign _ | Ast.Return _ | Ast.Break _ | Ast.Continue _
  | Ast.Expr_stmt _ | Ast.Fail _ | Ast.Const_assert_stmt _ ->
      acc

let bound_in_item = function
  | Ast.Func { params; body = Ast.Statements body; _ } ->
      bound_in_stmts (List.map (fun (p : Ast.param) -> (p.name, p.span)) params) body
  | Ast.Func { params; _ } -> List.map (fun (p : Ast.param) -> (p.name, p.span)) params
  | _ -> []

let split_dotted name =
  match String.index_opt name '.' with
  | Some dot ->
      Some
        (String.sub name 0 dot, String.sub name (dot + 1) (String.length name - dot - 1))
  | None -> None

let resolve (program : Ast.program) =
  let aliases = file_aliases program.items in
  let top = top_level_names program.items in
  let errors = ref [] in
  let error span message = errors := Diag.error span message :: !errors in
  Hashtbl.iter
    (fun _ file_aliases ->
      List.iter
        (fun (alias, span) ->
          match List.assoc_opt alias top with
          | Some _ ->
              error span
                (Printf.sprintf
                   "C alias `%s` has the name of a Fas top-level name; choose another \
                    alias"
                   alias)
          | None -> ())
        file_aliases)
    aliases;
  let refs = ref [] in
  let rewrite item =
    let file = item_file item in
    match Hashtbl.find_opt aliases file with
    | None ->
        (* no C in this file: a dotted type name can only be a mistake *)
        let mapper =
          {
            Ast.identity_mapper with
            on_ty =
              (function
              | Ast.Named_type (name, span) -> (
                  match split_dotted name with
                  | Some (alias, _) ->
                      error span
                        (Printf.sprintf
                           "`%s` is not a C alias in this file; add `use \"C\" \
                            <header.h> as %s`"
                           alias alias);
                      None
                  | None -> None)
              | _ -> None);
          }
        in
        Ast.map_item mapper item
    | Some file_aliases ->
        let is_alias name = List.mem_assoc name file_aliases in
        List.iter
          (fun (name, span) ->
            if is_alias name then
              error span
                (Printf.sprintf
                   "`%s` is this file's C alias; a local or parameter may not reuse \
                    the name"
                   name))
          (bound_in_item item);
        let c_name name span =
          refs := { file; name; span } :: !refs;
          Names.c_prefix ^ name
        in
        let mapper =
          {
            Ast.identity_mapper with
            on_expr =
              (function
              | Ast.Field (Ast.Ident (alias, _), name, span) when is_alias alias ->
                  Some (Ast.Ident (c_name name span, span))
              | _ -> None);
            on_ty =
              (function
              | Ast.Named_type (dotted, span) -> (
                  match split_dotted dotted with
                  | Some (alias, name) when is_alias alias ->
                      (* the name part only, like a value reference `c.NAME` *)
                      let skip = String.length alias + 1 in
                      let span =
                        if Span.is_synthetic span then span
                        else
                          {
                            span with
                            Span.start_offset = span.Span.start_offset + skip;
                            column = span.column + skip;
                          }
                      in
                      Some (Ast.Named_type (c_name name span, span))
                  | Some (alias, _) ->
                      error span
                        (Printf.sprintf "`%s` is not a C alias in this file" alias);
                      None
                  | None -> None)
              | _ -> None);
            on_arg =
              (function
              | Ast.Const_arg (Ast.Field (Ast.Ident (alias, _), name, span))
                when is_alias alias ->
                  Some (Ast.Name_arg (c_name name span, span))
              | _ -> None);
            on_target =
              (function
              | Ast.Target_field (Ast.Ident (alias, _), name, span) when is_alias alias
                ->
                  Some (Ast.Target_ident (c_name name span, span))
              | _ -> None);
          }
        in
        Ast.map_item mapper item
  in
  let items =
    List.filter_map
      (function Ast.Use _ -> None | item -> Some (rewrite item))
      program.items
  in
  match !errors with
  | [] -> Ok ({ Ast.items }, List.rev !refs)
  | errors -> Error (List.rev errors)

(* the C names the program references through an alias *)
let referenced refs = List.sort_uniq String.compare (List.map (fun r -> r.name) refs)

(* `c.NAME` for a C declaration that a Fas `extern` item provides (the import
   confirmed it, so it is not an imported item): the Fas item is the entity,
   under its plain name. *)
let use_provided ~provided (program : Ast.program) =
  if provided = [] then program
  else
    let rn name =
      if Names.is_c_name name && List.mem (Names.c_link_symbol name) provided then
        Names.c_link_symbol name
      else name
    in
    let mapper =
      {
        Ast.identity_mapper with
        on_expr =
          (function
          | Ast.Ident (name, span) -> Some (Ast.Ident (rn name, span))
          | _ -> None);
        on_arg =
          (function
          | Ast.Name_arg (name, span) -> Some (Ast.Name_arg (rn name, span))
          | _ -> None);
        on_target =
          (function
          | Ast.Target_ident (name, span) -> Some (Ast.Target_ident (rn name, span))
          | _ -> None);
      }
    in
    { Ast.items = List.map (Ast.map_item mapper) program.items }

(* [item] with every internal C name `c.NAME` in its types and values back to
   the plain C spelling, to compare a Fas `extern` item with the C
   declaration before C_import.namespace runs *)
let plain_c_names item =
  let plain name = if Names.is_c_name name then Names.c_link_symbol name else name in
  let mapper =
    {
      Ast.identity_mapper with
      on_expr =
        (function
        | Ast.Ident (name, span) -> Some (Ast.Ident (plain name, span))
        | _ -> None);
      on_ty =
        (function
        | Ast.Named_type (name, span) when Names.is_c_name name ->
            Some (Ast.Named_type (plain name, span))
        | _ -> None);
      on_arg =
        (function
        | Ast.Name_arg (name, span) -> Some (Ast.Name_arg (plain name, span))
        | _ -> None);
    }
  in
  Ast.map_item mapper item

(* A file sees only the C its own `use "C"` lines declare. [visible file]
   lists the C names of that file's unit; [None] when the file has no unit
   (its references were already refused by [resolve]). *)
let check_visibility ~visible ~known refs =
  List.filter_map
    (fun r ->
      match visible r.file with
      | Some names when (not (List.mem r.name names)) && known r.name ->
          Some
            (Diag.error r.span
               (Printf.sprintf
                  "C name `%s` is not declared by this file's C headers; another file \
                   imports it, so add its header here"
                  r.name))
      | _ -> None)
    refs

(* Migration helper (FAS_MIGRATE_C=1, driven by tools/migrate_c/migrate_c.py
   to move old code to C namespaces): every bare reference to a name in
   [c_names] that no local of its function binds, as (span, name). *)
let migration_edits ~c_names (program : Ast.program) =
  let edits = ref [] in
  let fas_names = List.map fst (top_level_names program.items) in
  List.iter
    (fun item ->
      let locals = List.map fst (bound_in_item item) in
      let hit name span =
        if
          Hashtbl.mem c_names name
          && (not (List.mem name locals))
          && (not (List.mem name fas_names))
          && not (Names.reserved_binding_name name)
        then edits := (span, name) :: !edits
      in
      let mapper =
        {
          Ast.identity_mapper with
          on_expr =
            (function
            | Ast.Ident (name, span) ->
                hit name span;
                None
            | _ -> None);
          on_ty =
            (function
            | Ast.Named_type (name, span) | Ast.Applied_type (name, span, _, _) ->
                hit name span;
                None
            | _ -> None);
          on_arg =
            (function
            | Ast.Name_arg (name, span) ->
                hit name span;
                None
            | _ -> None);
          on_target =
            (function
            | Ast.Target_ident (name, span) ->
                hit name span;
                None
            | _ -> None);
        }
      in
      ignore (Ast.map_item mapper item))
    program.items;
  List.rev !edits
