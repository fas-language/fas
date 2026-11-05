open Hir

type record = { name : string; spelling : string; header : string option }

let includes declarations =
  let tokens =
    String.map
      (function
        | ' ' | '\t' | '\r' | '\n' | '*' | '(' | ')' | ',' | ';' | '[' | ']' -> ' '
        | c -> c)
      declarations
    |> String.split_on_char ' '
  in
  let uses name = List.mem name tokens in
  let any names = List.exists uses names in
  [
    ("stdbool.h", uses "bool");
    ("stdalign.h", any [ "alignas"; "alignof" ]);
    ("assert.h", uses "static_assert");
    ( "stdint.h",
      List.exists
        (fun width ->
          uses ("int" ^ string_of_int width ^ "_t")
          || uses ("uint" ^ string_of_int width ^ "_t"))
        [ 8; 16; 32; 64 ] );
    ("stddef.h", any [ "size_t"; "ptrdiff_t"; "offsetof" ]);
  ]
  |> List.filter_map (fun (header, needed) ->
      if needed then Some ("#include <" ^ header ^ ">\n") else None)
  |> String.concat ""

let reserved_identifier name =
  List.mem name
    (String.split_on_char ' '
       "auto break case char const continue default do double else enum extern float \
        for goto if inline int long register restrict return short signed sizeof \
        static struct switch typedef union unsigned void volatile while _Alignas \
        _Alignof _Atomic _Bool _Complex _Generic _Imaginary _Noreturn _Static_assert \
        _Thread_local alignas alignof and and_eq asm bitand bitor bool catch class \
        compl constexpr const_cast decltype delete dynamic_cast explicit export false \
        friend mutable namespace new noexcept not not_eq nullptr operator or or_eq \
        private protected public reinterpret_cast static_assert static_cast template \
        this thread_local throw true try typeid typename using virtual wchar_t \
        char16_t char32_t xor xor_eq offsetof NULL size_t ptrdiff_t assert \
        __bool_true_false_are_defined __alignas_is_defined __alignof_is_defined int8_t \
        int16_t int32_t int64_t uint8_t uint16_t uint32_t uint64_t int_least8_t \
        int_least16_t int_least32_t int_least64_t uint_least8_t uint_least16_t \
        uint_least32_t uint_least64_t int_fast8_t int_fast16_t int_fast32_t \
        int_fast64_t uint_fast8_t uint_fast16_t uint_fast32_t uint_fast64_t intptr_t \
        uintptr_t intmax_t uintmax_t max_align_t wchar_t SIZE_MAX INT8_C INT16_C \
        INT32_C INT64_C UINT8_C UINT16_C UINT32_C UINT64_C INTMAX_C UINTMAX_C \
        _ASSERT_H _ASSERT_H_DECLS _STDINT_H __ASSERT_FUNCTION __ASSERT_VOID_CAST")
  || List.mem name
       (String.split_on_char ' '
          "INTPTR_MIN INTPTR_MAX UINTPTR_MAX INTMAX_MIN INTMAX_MAX UINTMAX_MAX \
           PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX WCHAR_MIN WCHAR_MAX \
           WINT_MIN WINT_MAX INT8_MIN INT8_MAX UINT8_MAX INT_LEAST8_MIN INT_LEAST8_MAX \
           UINT_LEAST8_MAX INT_FAST8_MIN INT_FAST8_MAX UINT_FAST8_MAX INT16_MIN \
           INT16_MAX UINT16_MAX INT_LEAST16_MIN INT_LEAST16_MAX UINT_LEAST16_MAX \
           INT_FAST16_MIN INT_FAST16_MAX UINT_FAST16_MAX INT32_MIN INT32_MAX \
           UINT32_MAX INT_LEAST32_MIN INT_LEAST32_MAX UINT_LEAST32_MAX INT_FAST32_MIN \
           INT_FAST32_MAX UINT_FAST32_MAX INT64_MIN INT64_MAX UINT64_MAX \
           INT_LEAST64_MIN INT_LEAST64_MAX UINT_LEAST64_MAX INT_FAST64_MIN \
           INT_FAST64_MAX UINT_FAST64_MAX")

let declarations ?(reserved = []) ?specializations records (program : program) =
  let find name = List.find (fun (s : struct_def) -> s.name = name) program.structs in
  let identifier name =
    if reserved_identifier name then
      Some (Printf.sprintf "`%s` is a reserved C or C++ identifier" name)
    else None
  in
  let generic_instance name =
    Option.bind specializations (fun specializations ->
        Sema_specialization.specialization_application_name specializations
          Sema_specialization.Struct_specialization name)
  in
  let rec invalid = function
    | Array (0, _) -> Some "contains a zero-length array"
    | Array (_, ty) -> invalid ty
    | Vec (_, ty) -> invalid ty
    | Struct name -> (
        match generic_instance name with
        | Some instance -> Some (Printf.sprintf "generic struct instance `%s`" instance)
        | None -> (
            match List.find_opt (fun r -> r.name = name) records with
            | Some r when r.header = None ->
                Some (Printf.sprintf "`%s` is declared only in a C container" name)
            | Some _ -> None
            | None ->
                let s = find name in
                if s.fields = [] then Some "contains an empty struct"
                else
                  List.find_map Fun.id
                    (identifier name
                    :: List.map
                         (fun (f : field) ->
                           match identifier f.name with
                           | Some _ as e -> e
                           | None -> invalid f.ty)
                         s.fields)))
    | Handle name -> (
        match generic_instance name with
        | Some instance -> Some (Printf.sprintf "generic struct instance `%s`" instance)
        | None -> (
            match List.find_opt (fun r -> r.name = name) records with
            | Some r when r.header = None && not (String.contains r.spelling ' ') ->
                Some (Printf.sprintf "`%s` is declared only in a C container" name)
            | Some _ -> None
            | None -> identifier name))
    | _ -> None
  in
  let exports =
    List.filter_map
      (fun (g : global) ->
        if g.linkage = Ast.Export_c && Option.is_some g.init_value then
          Some (g.name, [ g.ty ], `Global g)
        else None)
      program.globals
    @ List.filter_map
        (fun (f : func) ->
          if f.body <> Declaration && f.linkage = External_c then
            Some
              (f.name, f.ret :: List.map (fun (p : local) -> p.ty) f.params, `Function f)
          else None)
        program.funcs
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  let error (name, types, _) =
    match identifier name with
    | Some _ as e -> e
    | None ->
        List.find_map
          (fun ty ->
            Option.map
              (fun reason ->
                if String.starts_with ~prefix:"contains " reason then
                  ty_name ty ^ " " ^ reason
                else reason)
              (invalid ty))
          types
  in
  let errors, exports =
    List.partition (fun export -> Option.is_some (error export)) exports
  in
  let errors =
    List.map
      (fun ((name, _, _) as export) ->
        Printf.sprintf "export `%s` has no C declaration: %s" name
          (Option.get (error export)))
      errors
  in
  let lines = ref [] and headers = ref [] and seen = Hashtbl.create 32 in
  let record_headers_seen = Hashtbl.create 32 in
  let add text = lines := text :: !lines in
  let add_header record =
    Option.iter (fun header -> headers := header :: !headers) record.header
  in
  let rec add_type_headers active = function
    | Struct name -> add_record_headers active name
    | Array (_, ty) -> add_type_headers active ty
    | _ -> ()
  and add_record_headers active name =
    if (not (List.mem name active)) && not (Hashtbl.mem record_headers_seen name) then
      match List.find_opt (fun record -> record.name = name) records with
      | None -> ()
      | Some record ->
          Hashtbl.add record_headers_seen name ();
          (match
             List.find_opt
               (fun (structure : struct_def) -> structure.name = name)
               program.structs
           with
          | None -> ()
          | Some structure ->
              List.iter
                (fun (field : field) -> add_type_headers (name :: active) field.ty)
                structure.fields);
          add_header record
  in
  let rec base = function
    | Bool -> "bool"
    | Int Usize -> "size_t"
    | Int Isize -> "ptrdiff_t"
    | Int kind ->
        let name = ty_name (Int kind) in
        (if name.[0] = 'u' then "uint" else "int")
        ^ String.sub name 1 (String.length name - 1)
        ^ "_t"
    | Float Hir.F32 -> "float"
    | Float Hir.F64 -> "double"
    | Void -> "void"
    | Addr -> "void *"
    | Handle name -> (
        match List.find_opt (fun r -> r.name = name) records with
        | Some r ->
            if
              String.contains r.spelling ' '
              && not (Hashtbl.mem seen ("handle:" ^ name))
            then (
              Hashtbl.add seen ("handle:" ^ name) ();
              add (r.spelling ^ ";\n"));
            add_record_headers [] name;
            r.spelling ^ " *"
        | None ->
            if not (Hashtbl.mem seen ("handle:" ^ name)) then (
              Hashtbl.add seen ("handle:" ^ name) ();
              add ("struct " ^ name ^ ";\n"));
            "struct " ^ name ^ " *")
    | Vec (n, ty) ->
        let stem = Printf.sprintf "fas_vec_%d_%s" n (ty_name ty) in
        let rec available suffix =
          let name = stem ^ suffix in
          if
            List.mem name reserved
            || List.exists
                 (fun (structure : struct_def) -> structure.name = name)
                 program.structs
            || List.exists (fun (export, _, _) -> export = name) exports
          then available (suffix ^ "_")
          else name
        in
        let name = available "" in
        if not (Hashtbl.mem seen name) then (
          Hashtbl.add seen name ();
          add
            (Printf.sprintf "typedef %s %s __attribute__((ext_vector_type(%d)));\n"
               (base ty) name n);
          checks name (Vec (n, ty)));
        name
    | Struct name -> (
        if not (Hashtbl.mem seen ("struct:" ^ name)) then (
          Hashtbl.add seen ("struct:" ^ name) ();
          let s = find name in
          match List.find_opt (fun r -> r.name = name) records with
          | Some r ->
              add_record_headers [] name;
              checks r.spelling (Struct name);
              List.iter
                (fun (f : field) ->
                  add
                    (Printf.sprintf
                       "static_assert(offsetof(%s, %s) == %d, \"%s.%s offset\");\n"
                       r.spelling f.name f.offset name f.name))
                s.fields;
              r.spelling
          | None ->
              let fields =
                List.mapi
                  (fun i (f : field) ->
                    Printf.sprintf "  %s%s;\n"
                      (if i = 0 then Printf.sprintf "alignas(%d) " s.align else "")
                      (declarator f.ty f.name))
                  s.fields
              in
              add ("struct " ^ name ^ " {\n" ^ String.concat "" fields ^ "};\n");
              add ("typedef struct " ^ name ^ " " ^ name ^ ";\n");
              checks ("struct " ^ name) (Struct name);
              List.iter
                (fun (f : field) ->
                  add
                    (Printf.sprintf
                       "static_assert(offsetof(struct %s, %s) == %d, \"%s.%s offset\");\n"
                       name f.name f.offset name f.name))
                s.fields;
              "struct " ^ name)
        else
          match List.find_opt (fun r -> r.name = name) records with
          | Some r -> r.spelling
          | None -> "struct " ^ name)
    | Array _ | Opaque _ -> invalid_arg "internal error: C declaration base type"
  and declarator ty name =
    match ty with
    | Array (n, ty) -> declarator ty (Printf.sprintf "%s[%d]" name n)
    | _ -> base ty ^ " " ^ name
  and checks name ty =
    let size, align =
      match layout program.structs ty with
      | Ok x -> x
      | Error e -> failwith ("internal error: " ^ e)
    in
    add
      (Printf.sprintf
         "static_assert(sizeof(%s) == %d, \"%s size\");\n\
          static_assert(alignof(%s) == %d, \"%s alignment\");\n"
         name size name name align name)
  in
  let decls =
    List.map
      (fun (_, _, export) ->
        match export with
        | `Global g -> "extern " ^ declarator g.ty g.name ^ ";\n"
        | `Function f ->
            let params =
              List.map
                (fun (p : local) ->
                  declarator p.ty (if reserved_identifier p.name then "" else p.name))
                f.params
            in
            base f.ret ^ " " ^ f.name ^ "("
            ^ (if params = [] then "void" else String.concat ", " params)
            ^ ");\n")
      exports
  in
  let headers =
    List.fold_left
      (fun headers header ->
        if List.mem header headers then headers else header :: headers)
      [] (List.rev !headers)
    |> List.rev
  in
  (String.concat "" (List.rev !lines @ decls), headers, errors)

let guard name =
  String.uppercase_ascii (Filename.remove_extension (Filename.basename name))
  |> String.map (fun c ->
      if (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_')

let identifiers text =
  let is_start = function 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false in
  let is_continue = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let length = String.length text in
  let rec scan index names =
    if index = length then names
    else if is_start text.[index] then
      let rec finish index =
        if index < length && is_continue text.[index] then finish (index + 1) else index
      in
      let stop = finish (index + 1) in
      scan stop (String.sub text index (stop - index) :: names)
    else scan (index + 1) names
  in
  scan 0 []

let header ~name ~headers declarations =
  let body =
    includes declarations ^ String.concat "" headers
    ^ "#ifdef __cplusplus\nextern \"C\" {\n#endif\n" ^ declarations
    ^ "#ifdef __cplusplus\n}\n#endif\n#endif\n"
  in
  let used = identifiers body in
  let rec available suffix =
    let candidate = "FAS_" ^ guard name ^ "_H" ^ suffix in
    if List.mem candidate used then available (suffix ^ "_FAS") else candidate
  in
  let guard = available "" in
  "#ifndef " ^ guard ^ "\n#define " ^ guard ^ "\n" ^ body

let relative_path directory path =
  let parts path = String.split_on_char '/' (Unix.realpath path) in
  let rec common left right =
    match (left, right) with
    | a :: left, b :: right when a = b -> common left right
    | left, right -> String.concat "/" (List.map (fun _ -> "..") left @ right)
  in
  common (parts directory) (parts path)
