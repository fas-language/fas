(* Runtime helpers for the native backend, written in Fas.

   Some operations have no baseline x86-64 instruction and, in a freestanding
   program, no libm to call: today that is `fma`. Their implementations live
   in runtime/*.fas, embedded as strings by runtime/dune. When a module needs
   one, the source is compiled by the ordinary front end (parse, check,
   lower, fold) once per process, its functions are renamed into the
   `__fas_rt_` namespace with internal linkage, and they are appended to the
   module before the native passes run. They are then promoted, inlined and
   compiled like user code.

   llvm.fma.f32 / llvm.fma.f64 calls are rewritten to __fas_rt_fma32 /
   __fas_rt_fma64; vector fma reaches here already split into scalar lanes by
   Scalarize. *)

open Ir

let prefix = "__fas_rt_"

exception Failed of string

let compile_source ~file text : func list =
  let source = Source.create ~file ~text in
  let fail diagnostics =
    raise (Failed (Diag.render_all ~source:(Some source) diagnostics))
  in
  let hir =
    match Parser.parse source with
    | Error d -> fail d
    | Ok program -> ( match Sema.check program with Error d -> fail d | Ok hir -> hir)
  in
  let m = match Lower.lower hir with Error d -> fail d | Ok m -> Ir_fold.module_ m in
  if m.globals <> [] then raise (Failed (file ^ ": runtime sources must not define globals"));
  let defined = List.filter (fun (f : func) -> f.blocks <> []) m.funcs in
  let rename n =
    if List.exists (fun (f : func) -> f.name = n) defined then prefix ^ n else n
  in
  List.map
    (fun (f : func) ->
      if f.blocks = [] then f
      else
        {
          f with
          name = rename f.name;
          linkage = Internal;
          source = None;
          display = None;
          blocks =
            List.map
              (fun (b : block) ->
                {
                  b with
                  instrs =
                    List.map
                      (function
                        | Call (r, e, t, n, args) -> Call (r, e, t, rename n, args)
                        | i -> i)
                      b.instrs;
                })
              f.blocks;
        })
    m.funcs

let fma_funcs =
  lazy (compile_source ~file:"<fas runtime fma.fas>" Native_runtime_source.fma)

(* llvm.fma.<ty> -> runtime function, for the scalar types the helpers cover *)
let fma_target = function
  | "llvm.fma.f64" -> Some (prefix ^ "fas_rt_fma64")
  | "llvm.fma.f32" -> Some (prefix ^ "fas_rt_fma32")
  | _ -> None

let uses_fma (m : module_) =
  List.exists
    (fun (f : func) ->
      List.exists
        (fun (b : block) ->
          List.exists
            (function
              | Call (_, _, _, n, _) -> String.starts_with ~prefix:"llvm.fma." n
              | _ -> false)
            b.instrs)
        f.blocks)
    m.funcs

(* Before Scalarize (which also lowers the runtime's mul_hi): append the
   runtime functions when any fma, scalar or vector, is used. *)
let add (m : module_) : module_ =
  if not (uses_fma m) then m
  else
    let runtime =
      match Lazy.force fma_funcs with
      | fs -> fs
      | exception Failed msg -> raise (Scalarize.Unsupported ("runtime: " ^ msg))
    in
    let have = Hashtbl.create 64 in
    List.iter (fun (f : func) -> Hashtbl.replace have f.name ()) m.funcs;
    (* Declarations the runtime needs (llvm.ctlz...) join unless present *)
    let extra =
      List.filter
        (fun (f : func) ->
          if Hashtbl.mem have f.name then false
          else (
            Hashtbl.replace have f.name ();
            true))
        runtime
    in
    { m with funcs = m.funcs @ extra }

(* After Scalarize: point the scalar fma calls at the runtime *)
let rewrite (m : module_) : module_ =
  let rewrite_block (b : block) =
    if
      not
        (List.exists
           (function Call (_, _, _, n, _) -> Option.is_some (fma_target n) | _ -> false)
           b.instrs)
    then b
    else
      {
        b with
        instrs =
          List.map
            (function
              | Call (r, e, t, n, args) as i -> (
                  match fma_target n with Some n' -> Call (r, e, t, n', args) | None -> i)
              | i -> i)
            b.instrs;
      }
  in
  { m with funcs = List.map (fun (f : func) -> { f with blocks = List.map rewrite_block f.blocks }) m.funcs }
