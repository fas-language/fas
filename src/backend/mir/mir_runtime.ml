(* Mir_runtime: runtime helpers the machine-IR backend calls.

   Baseline x86-64 has no FMA instruction and a freestanding program no
   libm, so a scalar [fma] becomes a call to the correctly rounded helpers
   in backend/native/runtime/fma.fas. Native_runtime compiles that Fas
   source to Fir (a fresh copy per use); its functions, local and named
   __fas_rt_*, are appended to a module that uses [fma]. *)

module F = Fir

let fma_name dbl = Native_runtime.prefix ^ if dbl then "fas_rt_fma64" else "fas_rt_fma32"

let uses_fma (m : F.modul) =
  let found = ref false in
  F.Vec.iter
    (fun (fn : F.func) ->
      if fn.defined && not !found then
        F.Vec.iter (fun (ir : F.inst_rec) -> match ir.op with F.Fma _ -> found := true | _ -> ()) fn.insts)
    m.funcs;
  !found

(* appends the helpers [m] needs (once) *)
let add (m : F.modul) =
  if uses_fma m then begin
    let present = Hashtbl.create 64 in
    F.Vec.iter (fun (f : F.func) -> Hashtbl.replace present f.name f.defined) m.funcs;
    F.Vec.iter
      (fun (f : F.func) ->
        (* helpers' own declarations (none today) would duplicate the
           module's; only definitions not already there are added *)
        if f.defined && not (Hashtbl.mem present f.name) then begin
          (* their location ids index the helpers' own module: runtime code
             has no source lines for the program's debug info (like
             libgcc), so they are cleared *)
          F.Vec.iter (fun (ir : F.inst_rec) -> ir.loc <- 0) f.insts;
          F.Vec.iter (fun (b : F.block_rec) -> b.term_loc <- 0) f.blocks;
          F.add_func m f
        end)
      (Native_runtime.fma_module ()).funcs;
    (* the Fas front end may use wider integers (i128 for a 64x64 product):
       legalize again; functions already legal are left alone *)
    Fir_wide.run m
  end
