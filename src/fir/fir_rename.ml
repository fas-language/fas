(* Renames symbols across a Fir module, in place: function, global and alias
   names, and every reference to them (direct callees, [Csym] and
   [Cblockaddr] constants, [Reloc] and [Blockref] data, alias targets).

   The driver uses it for C adapter names (a Fas function that a C header
   declares static inline gets a private name, and its callers are redirected
   to the adapter), for both backends: before v0.6 the LLVM renderer applied
   the mapping while printing and the native backend renamed the old Ir
   (the deleted Native_symbols). [rename] must be injective on the module's names. *)

open Fir

let apply (rename : string -> string) (m : modul) =
  let changes =
    Vec.fold (fun acc f -> acc || rename f.name <> f.name) false m.funcs
    || Vec.fold (fun acc g -> acc || rename g.gname <> g.gname) false m.globals
    || Vec.fold (fun acc a -> acc || rename a.aname <> a.aname || rename a.target <> a.target) false m.aliases
  in
  if changes then begin
    Vec.iteri
      (fun i f ->
        (* the source-level name stays what debuggers show *)
        let display = match f.display with None when rename f.name <> f.name -> Some f.name | d -> d in
        let f = { f with name = rename f.name; display } in
        Vec.set m.funcs i f;
        (* constants: rewrite, then rebuild the interning table *)
        let touched = ref false in
        Vec.iteri
          (fun v d ->
            match d with
            | Const (Csym (n, a)) when rename n <> n ->
                Vec.set f.defs v (Const (Csym (rename n, a)));
                touched := true
            | Const (Cblockaddr (n, b)) when rename n <> n ->
                Vec.set f.defs v (Const (Cblockaddr (rename n, b)));
                touched := true
            | _ -> ())
          f.defs;
        if !touched then begin
          Hashtbl.reset f.consts;
          Vec.iteri
            (fun v d ->
              match d with Const c when not (Hashtbl.mem f.consts c) -> Hashtbl.add f.consts c v | _ -> ())
            f.defs
        end;
        Vec.iter
          (fun (ir : inst_rec) ->
            match ir.op with
            | Call ({ callee = Direct n; _ } as c) when rename n <> n -> ir.op <- Call { c with callee = Direct (rename n) }
            | _ -> ())
          f.insts)
      m.funcs;
    Vec.iteri
      (fun i g ->
        let piece = function
          | Reloc r -> Reloc { r with sym = rename r.sym }
          | Blockref b -> Blockref { b with fn = rename b.fn }
          | p -> p
        in
        Vec.set m.globals i { g with gname = rename g.gname; init = Option.map (List.map piece) g.init })
      m.globals;
    Vec.iteri (fun i a -> Vec.set m.aliases i { a with aname = rename a.aname; target = rename a.target }) m.aliases
  end
