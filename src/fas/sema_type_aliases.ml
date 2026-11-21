module String_set = Set.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let desugar ?index (program : Ast.program) =
  let aliases : (string, Ast.ty * Span.t * Span.t * Span.t) Hashtbl.t = Hashtbl.create 16 in
  let non_alias_names = Hashtbl.create 64 in
  List.iter
    (function
      | Ast.Struct { name; _ }
      | Ast.Opaque { name; _ }
      | Ast.Enum { name; _ }
      | Ast.Const { name; _ }
      | Ast.Global { name; _ }
      | Ast.Func { name; _ } ->
          Hashtbl.replace non_alias_names name ()
      | Ast.Use _ | Ast.Pkg _ | Ast.Error_decl _ | Ast.Const_assert _ | Ast.Type_alias _
        ->
          ())
    program.items;
  let* () =
    Result_list.iter
      (function
        | Ast.Type_alias { name; name_span; ty; ty_span; span; extent } ->
            let* () = Sema_context.validate_binding_name name_span name in
            if Hashtbl.mem aliases name then
              error name_span (Printf.sprintf "duplicate type alias `%s`" name)
            else if Hashtbl.mem non_alias_names name then
              error name_span (Printf.sprintf "duplicate declaration `%s`" name)
            else (
              Hashtbl.replace aliases name (ty, name_span, ty_span, span);
              Option.iter
                (fun idx ->
                  Sema_index.record_type_alias idx name name_span extent
                    (Ast.type_name ty))
                index;
              Ok ())
        | _ -> Ok ())
      program.items
  in
  if Hashtbl.length aliases = 0 then Ok program
  else
    let resolved : (string, Ast.ty) Hashtbl.t = Hashtbl.create 16 in
    let rec expand_alias visiting span name =
      Option.iter (fun idx -> Sema_index.refer_name idx span name) index;
      match Hashtbl.find_opt resolved name with
      | Some ty -> Ok ty
      | None ->
          if List.mem name visiting then
            error span (Printf.sprintf "cyclic type alias `%s`" name)
          else
            let raw_ty, _, _, _ = Hashtbl.find aliases name in
            let* ty = expand_ty (name :: visiting) String_set.empty raw_ty in
            Hashtbl.replace resolved name ty;
            Ok ty
    and expand_ty visiting shadowed = function
      | Ast.Named_type (name, span)
        when (not (String_set.mem name shadowed)) && Hashtbl.mem aliases name ->
          expand_alias visiting span name
      | Ast.Applied_type (name, name_span, _, _)
        when (not (String_set.mem name shadowed)) && Hashtbl.mem aliases name ->
          error name_span
            (Printf.sprintf "type alias `%s` does not take generic arguments" name)
      | Ast.Applied_type (name, name_span, args, app_span) ->
          let* args = Result_list.map (expand_arg visiting shadowed shadowed) args in
          Ok (Ast.Applied_type (name, name_span, args, app_span))
      | Ast.Handle inner ->
          let* inner = expand_ty visiting shadowed inner in
          Ok (Ast.Handle inner)
      | Ast.Secret inner ->
          let* inner = expand_ty visiting shadowed inner in
          Ok (Ast.Secret inner)
      | Ast.Array (len, elem) ->
          let* expression = expand_expr visiting shadowed shadowed len.expression in
          let* elem = expand_ty visiting shadowed elem in
          Ok (Ast.Array ({ len with expression }, elem))
      | Ast.Vec (len, elem) ->
          let* expression = expand_expr visiting shadowed shadowed len.expression in
          let* elem = expand_ty visiting shadowed elem in
          Ok (Ast.Vec ({ len with expression }, elem))
      | (Ast.Bool | Ast.Int _ | Ast.Float _ | Ast.Addr | Ast.Void | Ast.Named_type _) as
        ty ->
          Ok ty
    and expand_arg visiting type_shadowed value_shadowed = function
      | Ast.Type_arg ty ->
          let* ty = expand_ty visiting type_shadowed ty in
          Ok (Ast.Type_arg ty)
      | Ast.Type_or_index (Ast.Named_type (name, span))
        when (not (String_set.mem name type_shadowed))
             && (not (String_set.mem name value_shadowed))
             && Hashtbl.mem aliases name ->
          let* ty = expand_alias visiting span name in
          Ok (Ast.Type_arg ty)
      | Ast.Type_or_index ty ->
          let* ty = expand_ty visiting type_shadowed ty in
          Ok (Ast.Type_or_index ty)
      | Ast.Name_arg (name, span)
        when (not (String_set.mem name type_shadowed))
             && (not (String_set.mem name value_shadowed))
             && Hashtbl.mem aliases name ->
          let* ty = expand_alias visiting span name in
          Ok (Ast.Type_arg ty)
      | Ast.Name_arg _ as arg -> Ok arg
      | Ast.Const_arg expr ->
          let* expr = expand_expr visiting type_shadowed value_shadowed expr in
          Ok (Ast.Const_arg expr)
    and expand_expr visiting ts vs = function
      | ( Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _
        | Ast.String_lit _ | Ast.Byte_string _ | Ast.Ident _ ) as e ->
          Ok e
      | Ast.Unary (op, e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Unary (op, e, s))
      | Ast.C_dereference (e, n, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.C_dereference (e, n, s))
      | Ast.C_dot_star (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.C_dot_star (e, s))
      | Ast.Parenthesized (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Parenthesized (e, s))
      | Ast.Binary (op, l, r, s) ->
          let* l = expand_expr visiting ts vs l in
          let* r = expand_expr visiting ts vs r in
          Ok (Ast.Binary (op, l, r, s))
      | Ast.Call (f, args, s) ->
          let* f = expand_expr visiting ts vs f in
          let* args = Result_list.map (expand_expr visiting ts vs) args in
          Ok (Ast.Call (f, args, s))
      | Ast.Generic_args (f, args, s) ->
          let* f = expand_expr visiting ts vs f in
          let* args = Result_list.map (expand_arg visiting ts vs) args in
          Ok (Ast.Generic_args (f, args, s))
      | Ast.Cast (k, ty, e, s) ->
          let* ty = expand_ty visiting ts ty in
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Cast (k, ty, e, s))
      | Ast.Select (base, args, s) ->
          let* base = expand_expr visiting ts vs base in
          let* args = Result_list.map (expand_arg visiting ts vs) args in
          Ok (Ast.Select (base, args, s))
      | Ast.Field (base, n, s) ->
          let* base = expand_expr visiting ts vs base in
          Ok (Ast.Field (base, n, s))
      | Ast.Arrow_field (base, n, s1, s2) ->
          let* base = expand_expr visiting ts vs base in
          Ok (Ast.Arrow_field (base, n, s1, s2))
      | Ast.Addr_of (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Addr_of (e, s))
      | Ast.Handle_from_addr (ty, e, s) ->
          let* ty = expand_ty visiting ts ty in
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Handle_from_addr (ty, e, s))
      | Ast.Sizeof (ty, s) ->
          let* ty = expand_ty visiting ts ty in
          Ok (Ast.Sizeof (ty, s))
      | Ast.Sizeof_value (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Sizeof_value (e, s))
      | Ast.Alignof (ty, s) ->
          let* ty = expand_ty visiting ts ty in
          Ok (Ast.Alignof (ty, s))
      | Ast.Offsetof (ty, f, fs, s) ->
          let* ty = expand_ty visiting ts ty in
          Ok (Ast.Offsetof (ty, f, fs, s))
      | Ast.Splat (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Splat (e, s))
      | Ast.Ternary (c, a, b, s) ->
          let* c = expand_expr visiting ts vs c in
          let* a = expand_expr visiting ts vs a in
          let* b = expand_expr visiting ts vs b in
          Ok (Ast.Ternary (c, a, b, s))
      | Ast.Array_lit (xs, s) ->
          let* xs = Result_list.map (expand_expr visiting ts vs) xs in
          Ok (Ast.Array_lit (xs, s))
      | Ast.Designated_field (n, ns, e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Designated_field (n, ns, e, s))
      | Ast.Designated_index (i, e, s) ->
          let* i = expand_expr visiting ts vs i in
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Designated_index (i, e, s))
      | Ast.Try (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Try (e, s))
      | Ast.Catch (e, handler, s) ->
          let* e = expand_expr visiting ts vs e in
          let* handler =
            match handler with
            | Ast.Catch_value v ->
                let* v = expand_expr visiting ts vs v in
                Ok (Ast.Catch_value v)
            | Ast.Catch_block (binding, body) ->
                let vs' =
                  match binding with
                  | Some (name, _) -> String_set.add name vs
                  | None -> vs
                in
                let* body = expand_stmts visiting ts vs' body in
                Ok (Ast.Catch_block (binding, body))
          in
          Ok (Ast.Catch (e, handler, s))
    and expand_target visiting ts vs = function
      | Ast.Target_ident _ as t -> Ok t
      | Ast.Target_select (e, args) ->
          let* e = expand_expr visiting ts vs e in
          let* args = Result_list.map (expand_arg visiting ts vs) args in
          Ok (Ast.Target_select (e, args))
      | Ast.Target_field (e, n, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Target_field (e, n, s))
    and expand_stmts visiting ts vs stmts =
      let rec go vs acc = function
        | [] -> Ok (List.rev acc)
        | stmt :: rest ->
            let* stmt, vs = expand_stmt visiting ts vs stmt in
            go vs (stmt :: acc) rest
      in
      go vs [] stmts
    and expand_stmt visiting ts vs = function
      | Ast.Let ({ name; ty; init; _ } as l) ->
          let* ty = expand_ty visiting ts ty in
          let* init =
            match init with
            | None -> Ok None
            | Some e ->
                let* e = expand_expr visiting ts vs e in
                Ok (Some e)
          in
          Ok (Ast.Let { l with ty; init }, String_set.add name vs)
      | Ast.View ({ name; place; _ } as v) ->
          let* place = expand_expr visiting ts vs place in
          Ok (Ast.View { v with place }, String_set.add name vs)
      | Ast.Assign (t, e, s) ->
          let* t = expand_target visiting ts vs t in
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Assign (t, e, s), vs)
      | Ast.Compound_assign (t, op, e, s1, s2) ->
          let* t = expand_target visiting ts vs t in
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Compound_assign (t, op, e, s1, s2), vs)
      | Ast.Return (e, s) ->
          let* e =
            match e with
            | None -> Ok None
            | Some e ->
                let* e = expand_expr visiting ts vs e in
                Ok (Some e)
          in
          Ok (Ast.Return (e, s), vs)
      | Ast.If (c, t, f, s) ->
          let* c = expand_expr visiting ts vs c in
          let* t = expand_stmts visiting ts vs t in
          let* f =
            match f with
            | None -> Ok None
            | Some f ->
                let* f = expand_stmts visiting ts vs f in
                Ok (Some f)
          in
          Ok (Ast.If (c, t, f, s), vs)
      | Ast.While (lbl, c, body, s) ->
          let* c = expand_expr visiting ts vs c in
          let* body = expand_stmts visiting ts vs body in
          Ok (Ast.While (lbl, c, body, s), vs)
      | (Ast.Break _ | Ast.Continue _) as stmt -> Ok (stmt, vs)
      | Ast.Defer (body, s) ->
          let* body = expand_stmts visiting ts vs body in
          Ok (Ast.Defer (body, s), vs)
      | Ast.Errdefer (binding, body, s) ->
          let vs' =
            match binding with Some (name, _) -> String_set.add name vs | None -> vs
          in
          let* body = expand_stmts visiting ts vs' body in
          Ok (Ast.Errdefer (binding, body, s), vs)
      | Ast.Expr_stmt (e, s1, s2) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Expr_stmt (e, s1, s2), vs)
      | Ast.For (lbl, init, cond, step, body, s) ->
          let* init, vs' =
            match init with
            | None -> Ok (None, vs)
            | Some st ->
                let* st, vs' = expand_stmt visiting ts vs st in
                Ok (Some st, vs')
          in
          let* cond =
            match cond with
            | None -> Ok None
            | Some e ->
                let* e = expand_expr visiting ts vs' e in
                Ok (Some e)
          in
          let* step =
            match step with
            | None -> Ok None
            | Some st ->
                let* st, _ = expand_stmt visiting ts vs' st in
                Ok (Some st)
          in
          let* body = expand_stmts visiting ts vs' body in
          Ok (Ast.For (lbl, init, cond, step, body, s), vs)
      | Ast.Const_for ({ name; ty; low; high; body; _ } as f) ->
          let* ty = expand_ty visiting ts ty in
          let* low = expand_expr visiting ts vs low in
          let* high = expand_expr visiting ts vs high in
          let* body = expand_stmts visiting ts (String_set.add name vs) body in
          Ok (Ast.Const_for { f with ty; low; high; body }, vs)
      | Ast.Const_copy ({ name; ty; body; _ } as f) ->
          let* ty = expand_ty visiting ts ty in
          let* body = expand_stmts visiting ts (String_set.add name vs) body in
          Ok (Ast.Const_copy { f with ty; body }, vs)
      | Ast.Switch (e, arms, default, s) ->
          let* e = expand_expr visiting ts vs e in
          let* arms =
            Result_list.map
              (fun (values, body) ->
                let* values = Result_list.map (expand_expr visiting ts vs) values in
                let* body = expand_stmts visiting ts vs body in
                Ok (values, body))
              arms
          in
          let* default =
            match default with
            | None -> Ok None
            | Some body ->
                let* body = expand_stmts visiting ts vs body in
                Ok (Some body)
          in
          Ok (Ast.Switch (e, arms, default, s), vs)
      | Ast.Fail (e, s) ->
          let* e = expand_expr visiting ts vs e in
          Ok (Ast.Fail (e, s), vs)
      | Ast.Catch_stmt { target; call; binding; body; span } ->
          let* target, vs_after =
            match target with
            | Ast.Catch_let ({ name; ty; _ } as l) ->
                let* ty = expand_ty visiting ts ty in
                Ok (Ast.Catch_let { l with ty }, String_set.add name vs)
            | Ast.Catch_assign t ->
                let* t = expand_target visiting ts vs t in
                Ok (Ast.Catch_assign t, vs)
            | (Ast.Catch_discard | Ast.Catch_return) as t -> Ok (t, vs)
          in
          let* call = expand_expr visiting ts vs call in
          let vs_body =
            match binding with Some (name, _) -> String_set.add name vs | None -> vs
          in
          let* body = expand_stmts visiting ts vs_body body in
          Ok (Ast.Catch_stmt { target; call; binding; body; span }, vs_after)
      | Ast.Const_assert_stmt ({ cond; _ } as ca) ->
          let* cond = expand_expr visiting ts vs cond in
          Ok (Ast.Const_assert_stmt { ca with cond }, vs)
      | Ast.Block (body, s) ->
          let* body = expand_stmts visiting ts vs body in
          Ok (Ast.Block (body, s), vs)
    and expand_generic_params visiting ts params =
      Result_list.map
        (function
          | Ast.Type_param _ as p -> Ok p
          | Ast.Const_param cp ->
              let* ty = expand_ty visiting ts cp.ty in
              Ok (Ast.Const_param { cp with ty })
          | Ast.Fn_param fp ->
              let* params = Result_list.map (expand_ty visiting ts) fp.params in
              let* ret = expand_ty visiting ts fp.ret in
              Ok (Ast.Fn_param { fp with params; ret }))
        params
    in
    (* Resolve all alias definitions first so cycles are always caught even if unused *)
    let* () =
      Result_list.iter
        (function
          | Ast.Type_alias { name; name_span; _ } ->
              let* _ = expand_alias [] name_span name in
              Ok ()
          | _ -> Ok ())
        program.items
    in
    let generic_param_names params =
      List.fold_left
        (fun acc -> function
          | Ast.Type_param { name; _ }
          | Ast.Const_param { name; _ }
          | Ast.Fn_param { name; _ } ->
              String_set.add name acc)
        String_set.empty params
    in
    let expand_item = function
      | Ast.Type_alias _ -> Ok None
      | (Ast.Use _ | Ast.Pkg _ | Ast.Error_decl _ | Ast.Opaque _) as item -> Ok (Some item)
      | Ast.Const_assert ({ cond; _ } as ca) ->
          let* cond = expand_expr [] String_set.empty String_set.empty cond in
          Ok (Some (Ast.Const_assert { ca with cond }))
      | Ast.Enum ({ repr; _ } as e) ->
          let* repr = expand_ty [] String_set.empty repr in
          Ok (Some (Ast.Enum { e with repr }))
      | Ast.Const ({ ty; value; fill; _ } as c) ->
          let* ty = expand_ty [] String_set.empty ty in
          let* value = expand_expr [] String_set.empty String_set.empty value in
          let* fill =
            match fill with
            | None -> Ok None
            | Some body ->
                let* body = expand_stmts [] String_set.empty String_set.empty body in
                Ok (Some body)
          in
          Ok (Some (Ast.Const { c with ty; value; fill }))
      | Ast.Global ({ ty; init; _ } as g) ->
          let* ty = expand_ty [] String_set.empty ty in
          let* init =
            match init with
            | None -> Ok None
            | Some e ->
                let* e = expand_expr [] String_set.empty String_set.empty e in
                Ok (Some e)
          in
          Ok (Some (Ast.Global { g with ty; init }))
      | Ast.Struct ({ generic_params; fields; _ } as s) ->
          let ts = generic_param_names generic_params in
          let* generic_params = expand_generic_params [] ts generic_params in
          let* fields =
            Result_list.map
              (fun (f : Ast.field) ->
                let* ty = expand_ty [] ts f.ty in
                Ok { f with ty })
              fields
          in
          Ok (Some (Ast.Struct { s with generic_params; fields }))
      | Ast.Func ({ generic_params; params; ret; body; _ } as f) ->
          let ts = generic_param_names generic_params in
          let* generic_params = expand_generic_params [] ts generic_params in
          let* params =
            Result_list.map
              (fun (p : Ast.param) ->
                let* ty = expand_ty [] ts p.ty in
                Ok { p with ty })
              params
          in
          let* ret = expand_ty [] ts ret in
          let vs =
            List.fold_left
              (fun acc (p : Ast.param) -> String_set.add p.name acc)
              ts params
          in
          let* body =
            match body with
            | Ast.Declaration -> Ok Ast.Declaration
            | Ast.Statements stmts ->
                let* stmts = expand_stmts [] ts vs stmts in
                Ok (Ast.Statements stmts)
          in
          Ok (Some (Ast.Func { f with generic_params; params; ret; body }))
    in
    let* items = Result_list.map expand_item program.items in
    Ok { Ast.items = List.filter_map Fun.id items }
