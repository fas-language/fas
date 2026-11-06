(* Runtime helpers for the native backend, written in Fas.

   Some operations have no baseline x86-64 instruction and, in a freestanding
   program, no libm to call: today that is `fma`. Their implementations live
   in runtime/*.fas, embedded as strings by runtime/dune. The source is
   compiled by the ordinary front end (parse, check, lower, fold) once per
   process, and its functions are renamed into the `__fas_rt_` namespace with
   internal linkage. Mir_runtime converts them to Fir and appends them to a
   module that uses fma. *)

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
