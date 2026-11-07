(* Runtime helpers for the native backend, written in Fas.

   Some operations have no baseline x86-64 instruction and, in a freestanding
   program, no libm to call: today that is `fma`. Their implementations live
   in runtime/*.fas, embedded as strings by runtime/dune. The source is
   compiled by the ordinary front end (parse, check, lower, fold) to Fir, and
   its functions are renamed into the `__fas_rt_` namespace with internal
   linkage. Mir_runtime appends them to a module that uses fma. *)

let prefix = "__fas_rt_"

exception Failed of string

(* A fresh module each call: Fir functions are mutable records, so modules
   must not share them. *)
let compile_source ~file text : Fir.modul =
  let source = Source.create ~file ~text in
  let fail diagnostics =
    raise (Failed (Diag.render_all ~source:(Some source) diagnostics))
  in
  let hir =
    match Parser.parse source with
    | Error d -> fail d
    | Ok program -> ( match Sema.check program with Error d -> fail d | Ok hir -> hir)
  in
  let m = match Lower.lower hir with Error d -> fail d | Ok m -> m in
  Fir_fold.run m;
  if Fir.Vec.length m.globals > 0 then
    raise (Failed (file ^ ": runtime sources must not define globals"));
  let defined = Hashtbl.create 16 in
  Fir.Vec.iter (fun (f : Fir.func) -> if f.defined then Hashtbl.replace defined f.name ()) m.funcs;
  Fir_rename.apply (fun n -> if Hashtbl.mem defined n then prefix ^ n else n) m;
  Fir.Vec.iter
    (fun (f : Fir.func) ->
      if f.defined then begin
        f.linkage <- Fir.Local;
        (* no source for debug info: like libgcc's *)
        f.display <- None;
        f.decl_loc <- 0
      end)
    m.funcs;
  m

let fma_module () = compile_source ~file:"<fas runtime fma.fas>" Native_runtime_source.fma
