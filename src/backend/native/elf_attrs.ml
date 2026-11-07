(* ELF symbol attributes of a Fir module's symbols, for Mir_emit: binding
   (local, global, weak), visibility and aliases. [of_fir] builds them. *)

type alias = {
  name : string;
  target : string;  (** a function or global defined in the same module *)
  addend : int;
  binding : int;  (** STB_LOCAL 0, STB_GLOBAL 1, STB_WEAK 2 *)
  visibility : int;  (** STV_DEFAULT 0, STV_HIDDEN 2, STV_PROTECTED 3 *)
}

type t = {
  binding : (string, int) Hashtbl.t;
  visibility : (string, int) Hashtbl.t;
  aliases : alias list;
}

let empty () = { binding = Hashtbl.create 1; visibility = Hashtbl.create 1; aliases = [] }
let weak t name = Hashtbl.find_opt t.binding name = Some 2

let stv : Fir.visibility -> int = function Default -> 0 | Hidden -> 2 | Protected -> 3
let stb : Fir.linkage -> int = function Local -> 0 | Global -> 1 | Weak -> 2

let of_fir (m : Fir.modul) =
  let t = empty () in
  let note name linkage (vis : Fir.visibility) =
    Hashtbl.replace t.binding name (stb linkage);
    if vis <> Default then Hashtbl.replace t.visibility name (stv vis)
  in
  Fir.Vec.iter (fun (f : Fir.func) -> note f.name f.linkage f.visibility) m.funcs;
  Fir.Vec.iter (fun (g : Fir.global) -> note g.gname g.glinkage g.gvisibility) m.globals;
  let aliases =
    Fir.Vec.fold
      (fun acc (a : Fir.alias) ->
        note a.aname a.alinkage a.avisibility;
        { name = a.aname; target = a.target; addend = Int64.to_int a.addend;
          binding = stb a.alinkage; visibility = stv a.avisibility }
        :: acc)
      [] m.aliases
  in
  { t with aliases = List.rev aliases }
