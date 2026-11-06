(* ELF symbol attributes that the old Ir (src/backend/llvm/ir.ml) cannot
   carry: weak binding, visibility and aliases. Fir_to_ir.symbols builds this
   from a Fir module for Mir_emit. Names absent from [binding]
   get the binding the Ir's linkage implies. *)

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
