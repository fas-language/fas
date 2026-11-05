(* Native code generation from [Ir]: scalar integers.

   Pipeline ([compile]): Mem2reg (promotion, addr+n as gep, DCE, jump
   threading, pruning), Ir_fold, store forwarding and block merging,
   reassociation, then per function: address analysis ([analyse_addrs] folds
   gep chains and constant index arithmetic into [base + idx*scale + disp]
   forms), linear-scan allocation (Regalloc), RPO block layout with loop
   rotation, and emission.

   Every SSA value has a home: a register from Regalloc or an 8-byte stack
   slot (aggregates get a slot of their size). Scratch registers are rax, rcx
   and rdx. Integer values are kept canonical: zero-extended from their width
   to 64 bits, with `i1` as 0 or 1; signed operations sign-extend operands.
   Instruction selection fuses compare-and-branch, folds address forms and
   immediates into operands, and turns load/op/store of one place into a
   read-modify-write instruction.

   Phis: each edge writes the phi homes of its target as one parallel move
   (cycles broken through a scratch register), so a phi that reads another
   phi of the same block always sees the old value.

   Calls between Fas functions use the SysV integer registers for the first
   six arguments and the stack for the rest, with the result in rax. Floats,
   vectors and `i128` are not supported yet and raise [Unsupported]. *)

open Ir
module Hashtbl = Itbl
module Ptbl = Stdlib.Hashtbl
module A = X64

exception Unsupported = Scalarize.Unsupported

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type ctx = { m : module_; a : A.t; func_labels : (string, A.label) Ptbl.t }

let size_of ctx ty =
  match static_type_bytes ctx.m.structs [] ty with
  | Ok n -> n
  | Error () -> unsupported "type with no static size"

let bits = function
  | I1 -> 1
  | I8 -> 8
  | I16 -> 16
  | I32 -> 32
  | I64 | Pointer _ -> 64
  | I128 -> unsupported "i128 values"
  | F32 -> 32
  | F64 -> 64
  | Vector _ -> unsupported "vector values"
  | Struct _ | Array _ -> unsupported "aggregate value in a scalar position"
  | Void -> unsupported "void value"

let is_aggregate = function Struct _ | Array _ -> true | _ -> false

let norm_const ty v =
  let w = bits ty in
  if w >= 64 then v else Int64.logand v (Int64.pred (Int64.shift_left 1L w))

let field_offset ctx name index =
  match List.find_opt (fun (d : struct_def) -> d.name = name) ctx.m.structs with
  | None -> unsupported "unknown struct %s" name
  | Some d ->
      let rec go n acc = function
        | [] -> unsupported "struct field index out of range"
        | f :: rest -> if n = index then acc else go (n + 1) (acc + size_of ctx f) rest
      in
      go 0 0 d.fields

(* Address forms. A pointer computed by `getelementptr`, `alloca` or a
   global is described as an anchor plus a constant offset plus at most one
   scaled 64-bit index, which is exactly an x86 memory operand. Pointers that
   are only ever used as the address of scalar loads and stores are folded
   into those instructions and never computed; constant ones (a global or a
   stack slot plus an offset) are rebuilt with one `lea` wherever a register
   needs them. *)

type anchor = Reg of value | Sym of string | Frame of int  (** alloca id *)
type form = { anchor : anchor; off : int; idx : (value * int) option }

type addrs = {
  forms : form Hashtbl.t;
  skip : unit Hashtbl.t;
  dead : unit Hashtbl.t;
      (** index arithmetic that folding into addresses left unused *)
  tests : (value * int) Hashtbl.t;
      (** a mask whose only use is a branch on it being zero: [test v, mask] *)
}

let plain v = { anchor = Reg v; off = 0; idx = None }
let is_const_form f = f.idx = None && match f.anchor with Reg _ -> false | _ -> true

let form_operands f =
  (match f.anchor with Reg v -> [ v ] | _ -> [])
  @ match f.idx with Some (v, _) -> [ v ] | None -> []

let scalar_access = function
  | (Load (_, ty, p, _) | Load_volatile (_, ty, p, _)) when not (is_aggregate ty) ->
      Some (None, p)
  | (Store (ty, v, p, _) | Store_volatile (ty, v, p, _)) when not (is_aggregate ty) ->
      Some (Some v, p)
  | _ -> None

let analyse_addrs ctx (f : func) =
  let defs = Hashtbl.create 64 in
  let arith = Hashtbl.create 64 in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match ins with
          | Bin (id, (Mul | Shl | Add), I64, _, Const _) -> Hashtbl.replace arith id ins
          | Gep (id, _, _, _)
          | Global_ptr (id, _, _)
          | String_ptr (id, _, _)
          | Alloca (id, _, _) ->
              Hashtbl.replace defs id ins
          | _ -> ())
        b.instrs)
    f.blocks;
  let memo = Hashtbl.create 64 in
  let rec form_of_id id =
    match Hashtbl.find_opt memo id with
    | Some r -> r
    | None ->
        let r =
          match Hashtbl.find_opt defs id with
          | Some (Global_ptr (_, name, _)) ->
              Some { anchor = Sym name; off = 0; idx = None }
          | Some (String_ptr (_, i, _)) ->
              Some { anchor = Sym (Printf.sprintf ".str.%d" i); off = 0; idx = None }
          | Some (Alloca (id, _, _)) -> Some { anchor = Frame id; off = 0; idx = None }
          | Some (Gep (_, ty, p, indices)) -> gep_form ty p indices
          | _ -> None
        in
        Hashtbl.replace memo id r;
        r
  and value_form v =
    match v with
    | Local (id, _) -> ( match form_of_id id with Some f -> f | None -> plain v)
    | Global (name, _) -> { anchor = Sym name; off = 0; idx = None }
    | v -> plain v
  and gep_form ty p indices =
    let exception No in
    let base = value_form p in
    let f = ref base in
    let add n =
      let o = Int64.add (Int64.of_int !f.off) n in
      if Int64.compare (Int64.abs o) 0x4000_0000L > 0 then raise No;
      f := { !f with off = Int64.to_int o }
    in
    let step size = function
      | Zero -> ()
      | Index (Const (ity, n)) ->
          let w = bits ity in
          let n =
            if w >= 64 then n
            else Int64.shift_right (Int64.shift_left n (64 - w)) (64 - w)
          in
          if Int64.compare (Int64.abs n) 0x4000_0000L > 0 then raise No;
          add (Int64.mul n (Int64.of_int size))
      | Index v ->
          (* A base that already uses the index slot, untouched so far, is
             used from its register instead: [q + v] *)
          (match p with
          | Local _ when !f.idx <> None && !f == base -> f := plain p
          | _ -> ());
          if value_ty v <> I64 || (not (List.mem size [ 1; 2; 4; 8 ])) || !f.idx <> None
          then raise No;
          (* (x + c) * k with k * size a valid scale: x scaled, c * size
             in the displacement *)
          let rec scaled v size =
            match v with
            | Local (id, _) -> (
                let small n = Int64.compare (Int64.abs n) 0x1000_0000L < 0 in
                match Hashtbl.find_opt arith id with
                | Some (Bin (_, Mul, _, x, Const (_, k)))
                  when small k && List.mem (Int64.to_int k * size) [ 1; 2; 4; 8 ] ->
                    scaled x (Int64.to_int k * size)
                | Some (Bin (_, Shl, _, x, Const (_, k)))
                  when Int64.compare k 0L >= 0
                       && Int64.compare k 3L <= 0
                       && List.mem (size lsl Int64.to_int k) [ 1; 2; 4; 8 ] ->
                    scaled x (size lsl Int64.to_int k)
                | Some (Bin (_, Add, _, x, Const (_, c))) when small c ->
                    add (Int64.mul c (Int64.of_int size));
                    scaled x size
                | _ -> (v, size))
            | _ -> (v, size)
          in
          let idx = scaled v size in
          f := { !f with idx = Some idx }
    in
    let rec walk ty = function
      | [] -> ()
      | idx :: rest -> (
          match ty with
          | Struct name ->
              let n =
                match idx with
                | Zero -> 0
                | Index (Const (_, n)) -> Int64.to_int n
                | Index _ -> raise No
              in
              add (Int64.of_int (field_offset ctx name n));
              let d = List.find (fun (d : struct_def) -> d.name = name) ctx.m.structs in
              walk (List.nth d.fields n) rest
          | Array (_, elem) | Vector (_, elem) ->
              step (size_of ctx elem) idx;
              walk elem rest
          | _ -> raise No)
    in
    try
      (match indices with
      | [] -> ()
      | first :: rest ->
          step (size_of ctx ty) first;
          walk ty rest);
      Some !f
    with No | Unsupported _ | Not_found | Failure _ | Invalid_argument _ -> None
  in
  let forms = Hashtbl.create 64 in
  Hashtbl.iter
    (fun id _ ->
      match form_of_id id with Some f -> Hashtbl.replace forms id f | None -> ())
    defs;
  (* Which form-carrying values are used other than as a scalar address? *)
  let escapes = Hashtbl.create 32 in
  let escape = function Local (id, _) -> Hashtbl.replace escapes id () | _ -> () in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match (ins, scalar_access ins) with
          | Gep (id, _, _, _), _ when Hashtbl.mem forms id ->
              List.iter escape (form_operands (Hashtbl.find forms id))
          | _, Some (v, _) -> Option.iter escape v
          | _ -> Regalloc.iter_operands escape ins)
        b.instrs;
      Regalloc.iter_term_operands escape b.terminator)
    f.blocks;
  let skip = Hashtbl.create 32 in
  Hashtbl.iter
    (fun id f ->
      if is_const_form f || not (Hashtbl.mem escapes id) then Hashtbl.replace skip id ())
    forms;
  (* Index arithmetic whose only readers now read through the folded form *)
  let dead = Hashtbl.create 16 in
  (* `br (x & m) != 0`, also through `(x >> k) & m`, as `test x, m'`: the
     mask and shift, used only there, are left out *)
  let tests = Hashtbl.create 8 in
  let uses = Hashtbl.create 64 and masks = Hashtbl.create 16 in
  let count = function
    | Local (id, _) ->
        Hashtbl.replace uses id (1 + Option.value ~default:0 (Hashtbl.find_opt uses id))
    | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          (match ins with
          | Bin (id, (And | Lshr), _, (Local _ | Param _), Const _) ->
              Hashtbl.replace masks id ins
          | _ -> ());
          Regalloc.iter_operands count ins)
        b.instrs;
      Regalloc.iter_term_operands count b.terminator)
    f.blocks;
  if Hashtbl.length masks > 0 then begin
    let once id = Hashtbl.find_opt uses id = Some 1 in
    List.iter
      (fun (b : block) ->
        match (List.rev b.instrs, b.terminator) with
        | ( Cmp (id, (Eq | Ne), (I8 | I16 | I32 | I64), Local (x, _), Const (_, 0L)) :: _,
            CondBr (Local (c, _), _, _) )
          when c = id && once id && once x -> (
            match Hashtbl.find_opt masks x with
            | Some (Bin (_, And, ty, v, Const (_, m))) ->
                let w = bits ty in
                let wmask =
                  if w = 64 then -1L else Int64.pred (Int64.shift_left 1L w)
                in
                let v, m, shift =
                  match v with
                  | Local (sid, _) when once sid -> (
                      match Hashtbl.find_opt masks sid with
                      | Some (Bin (_, Lshr, _, v', Const (_, k)))
                        when Int64.compare k 0L >= 0
                             && Int64.compare k (Int64.of_int w) < 0 ->
                          ( v',
                            Int64.logand (Int64.shift_left m (Int64.to_int k)) wmask,
                            Some sid )
                      | _ -> (v, m, None))
                  | _ -> (v, m, None)
                in
                if Int64.compare m 0L > 0 && Int64.compare m 0x7fffffffL <= 0 then begin
                  Hashtbl.replace tests x (v, Int64.to_int m);
                  Hashtbl.replace dead x ();
                  Option.iter (fun sid -> Hashtbl.replace dead sid ()) shift
                end
            | _ -> ())
        | _ -> ())
      f.blocks
  end;
  let addrs0 = { forms; skip; dead; tests } in
  let changed = ref true in
  while !changed do
    changed := false;
    let used = Hashtbl.create 64 in
    let use = function Local (id, _) -> Hashtbl.replace used id () | _ -> () in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match instruction_result ins with
            | Some (id, _) when Hashtbl.mem dead id -> ()
            | _ -> (
                match (ins, scalar_access ins) with
                | Gep (id, _, _, _), _ when Hashtbl.mem forms id ->
                    if not (Hashtbl.mem skip id) then
                      List.iter use (form_operands (Hashtbl.find forms id))
                | _, Some (v, p) -> (
                    Option.iter use v;
                    match p with
                    | Local (id, _) when Hashtbl.mem skip id ->
                        List.iter use (form_operands (Hashtbl.find forms id))
                    | p -> use p)
                | Cmp (_, _, _, Local (x, _), _), _ when Hashtbl.mem tests x ->
                    use (fst (Hashtbl.find tests x))
                | _ -> Regalloc.iter_operands use ins))
          b.instrs;
        Regalloc.iter_term_operands use b.terminator)
      f.blocks;
    Hashtbl.iter
      (fun id _ ->
        if (not (Hashtbl.mem used id)) && not (Hashtbl.mem dead id) then begin
          Hashtbl.replace dead id ();
          changed := true
        end)
      arith
  done;
  addrs0

(* What an instruction reads once folded addresses are expanded *)
let operands_view addrs ins =
  let ptr p =
    match p with
    | Local (id, _) when Hashtbl.mem addrs.skip id ->
        form_operands (Hashtbl.find addrs.forms id)
    | p -> [ p ]
  in
  match (ins, scalar_access ins) with
  | Bin (id, _, _, _, _), _ when Hashtbl.mem addrs.dead id -> []
  | Cmp (_, _, _, Local (x, _), _), _ when Hashtbl.mem addrs.tests x ->
      [ fst (Hashtbl.find addrs.tests x) ]
  | Gep (id, _, _, _), _ when Hashtbl.mem addrs.forms id ->
      if Hashtbl.mem addrs.skip id then []
      else form_operands (Hashtbl.find addrs.forms id)
  | _, Some (v, p) -> Option.to_list v @ ptr p
  | _ -> Regalloc.default_operands ins

(* Store-to-load forwarding and dead-store elimination inside each block.
   Two accesses are the same location when their address forms have the same
   anchor, index and offset; they are disjoint when the anchors are distinct
   globals or stack slots, or when the anchor and index match and the byte
   ranges do not overlap. Anything else may alias. A load of a location whose
   value is known (stored or loaded earlier in the block, nothing that may
   alias written since) takes that value; a store that a later store to the
   same bytes overwrites, with no read of them in between, is dropped. Calls,
   system calls, volatile accesses and traps forget everything. This is what
   merges `for const` read-modify-write chains on one word into one store. *)
(* Locations known in a block, indexed so that each access only looks at the
   entries it may alias: groups by (anchor, index), entries by offset. *)
module Locs = struct
  type key = anchor * (value * int) option

  type 'a group = {
    at : (int * 'a) list Hashtbl.t;  (** offset -> (size, payload) *)
    mutable widest : int;
  }

  type 'a t = {
    fixed : (anchor, ((value * int) option, 'a group) Ptbl.t) Ptbl.t;
        (** groups anchored on a global or a stack slot, by anchor *)
    regs : (key, 'a group) Ptbl.t;  (** groups anchored on a register value *)
  }

  let create () = { fixed = Ptbl.create 8; regs = Ptbl.create 8 }

  let clear t =
    Ptbl.reset t.fixed;
    Ptbl.reset t.regs

  let find_group t (fm : form) =
    match fm.anchor with
    | Reg _ -> Ptbl.find_opt t.regs (fm.anchor, fm.idx)
    | a -> (
        match Ptbl.find_opt t.fixed a with
        | Some g -> Ptbl.find_opt g fm.idx
        | None -> None)

  let group t (fm : form) =
    match find_group t fm with
    | Some g -> g
    | None ->
        let g = { at = Hashtbl.create 8; widest = 0 } in
        (match fm.anchor with
        | Reg _ -> Ptbl.replace t.regs (fm.anchor, fm.idx) g
        | a ->
            let tbl =
              match Ptbl.find_opt t.fixed a with
              | Some tbl -> tbl
              | None ->
                  let tbl = Ptbl.create 4 in
                  Ptbl.replace t.fixed a tbl;
                  tbl
            in
            Ptbl.replace tbl fm.idx g);
        g

  let add t (fm, n) x =
    let g = group t fm in
    g.widest <- max g.widest n;
    Hashtbl.replace g.at fm.off
      ((n, x) :: Option.value ~default:[] (Hashtbl.find_opt g.at fm.off))

  (* The entries of [g] whose bytes overlap [off, off + n) *)
  let overlapping g off n =
    let hit o (m, _) = o < off + n && off < o + m in
    if Hashtbl.length g.at <= n + g.widest then
      Hashtbl.fold
        (fun o es acc -> if List.exists (hit o) es then o :: acc else acc)
        g.at []
    else
      let acc = ref [] in
      for o = off - g.widest + 1 to off + n - 1 do
        match Hashtbl.find_opt g.at o with
        | Some es when List.exists (hit o) es -> acc := o :: !acc
        | _ -> ()
      done;
      !acc

  (* Remove every entry that may alias [loc] *)
  let kill t ((fm : form), n) =
    let own = find_group t fm in
    (match fm.anchor with
    | Reg _ ->
        Ptbl.reset t.fixed;
        Ptbl.reset t.regs
    | a ->
        Ptbl.reset t.regs;
        Option.iter Ptbl.reset (Ptbl.find_opt t.fixed a));
    match own with
    | None -> ()
    | Some g -> (
        List.iter
          (fun o ->
            match
              List.filter
                (fun (m, _) -> not (o < fm.off + n && fm.off < o + m))
                (Hashtbl.find g.at o)
            with
            | [] -> Hashtbl.remove g.at o
            | es -> Hashtbl.replace g.at o es)
          (overlapping g fm.off n);
        match fm.anchor with
        | Reg _ -> Ptbl.replace t.regs (fm.anchor, fm.idx) g
        | a -> (
            match Ptbl.find_opt t.fixed a with
            | Some tbl -> Ptbl.replace tbl fm.idx g
            | None ->
                let tbl = Ptbl.create 4 in
                Ptbl.replace tbl fm.idx g;
                Ptbl.replace t.fixed a tbl))

  (* Remove the entries whose bytes lie inside [loc], passing each to [f] *)
  let remove_covered t ((fm : form), n) f =
    match find_group t fm with
    | None -> ()
    | Some g ->
        List.iter
          (fun o ->
            let inside, rest =
              List.partition
                (fun (m, _) -> fm.off <= o && o + m <= fm.off + n)
                (Hashtbl.find g.at o)
            in
            List.iter (fun (_, x) -> f x) inside;
            match rest with
            | [] -> Hashtbl.remove g.at o
            | _ -> Hashtbl.replace g.at o rest)
          (overlapping g fm.off n)

  let find t ((fm : form), n) p =
    match find_group t fm with
    | None -> None
    | Some g -> (
        match Hashtbl.find_opt g.at fm.off with
        | Some es ->
            List.find_map (fun (m, x) -> if m = n && p x then Some x else None) es
        | None -> None)
end

let forward_memory ctx (f : func) =
  let addrs = analyse_addrs ctx f in
  let form_of = function
    | Local (id, _) as v -> (
        match Hashtbl.find_opt addrs.forms id with Some fm -> fm | None -> plain v)
    | Global (name, _) -> { anchor = Sym name; off = 0; idx = None }
    | v -> plain v
  in
  let env = Hashtbl.create 64 in
  let rec resolve = function
    | Local (i, _) as v -> (
        match Hashtbl.find_opt env i with Some w -> resolve w | None -> v)
    | v -> v
  in
  let size ty = try Some (size_of ctx ty) with Unsupported _ -> None in
  (* (type, value) known to be in memory at a location *)
  let known = Locs.create () in
  (* stores not read since, by their position in the block *)
  let pending = Locs.create () in
  let blocks =
    List.map
      (fun (b : block) ->
        Locs.clear known;
        Locs.clear pending;
        let dead = Hashtbl.create 8 in
        let forget () =
          Locs.clear known;
          Locs.clear pending
        in
        let instrs =
          List.mapi
            (fun pos ins ->
              let ins = Ir_fold.map_instr resolve ins in
              match ins with
              | Load (id, ty, p, _) -> (
                  match size ty with
                  | Some n when not (is_aggregate ty) -> (
                      let loc = (form_of p, n) in
                      match Locs.find known loc (fun (t, _) -> t = ty) with
                      | Some (_, v) ->
                          Hashtbl.replace env id v;
                          None
                      | None ->
                          Locs.kill pending loc;
                          Locs.add known loc (ty, Local (id, ty));
                          Some ins)
                  | Some n ->
                      Locs.kill pending (form_of p, n);
                      Some ins
                  | None ->
                      Locs.clear pending;
                      Some ins)
              | Store (ty, v, p, _) -> (
                  match size ty with
                  | Some n ->
                      let loc = (form_of p, n) in
                      Locs.remove_covered pending loc (fun at ->
                          Hashtbl.replace dead at ());
                      Locs.kill known loc;
                      if not (is_aggregate ty) then Locs.add known loc (ty, v);
                      Locs.add pending loc pos;
                      Some ins
                  | None ->
                      forget ();
                      Some ins)
              | Call (_, _, _, name, _) when Mem2reg.pure_call name -> Some ins
              | Load_volatile _ | Store_volatile _ | Call _ | Call_indirect _
              | Syscall _ | Trap ->
                  forget ();
                  Some ins
              | _ -> Some ins)
            b.instrs
        in
        let instrs =
          List.filteri (fun pos _ -> not (Hashtbl.mem dead pos)) instrs
          |> List.filter_map Fun.id
        in
        { b with instrs })
      f.blocks
  in
  {
    f with
    blocks =
      List.map
        (fun (b : block) ->
          {
            b with
            instrs = List.map (Ir_fold.map_instr resolve) b.instrs;
            terminator = Ir_fold.map_term resolve b.terminator;
          })
        blocks;
  }

(* Per-function frame *)

type frame = {
  addrs : addrs;
  slots : int Hashtbl.t;  (** SSA id -> rbp offset *)
  allocas : int Hashtbl.t;  (** alloca id -> rbp offset of its memory *)
  params : (string, int) Ptbl.t;
  mutable size : int;
  homes : A.reg Hashtbl.t;  (** values that live in a register *)
  xhomes : int Hashtbl.t;  (** 128-bit vectors that live in an xmm register *)
  xshared : int Hashtbl.t;  (** spilled vector -> phi-web slot representative *)
  xtmp : int;  (** rbp offset of a 16-byte scratch slot for vector lanes *)
  saved : (A.reg * int) list;  (** callee-saved registers used, and their save slots *)
  call_saves : (instr * (int * bool) list) list;
      (** per call: allocation keys in caller-saved registers to store before it, each
          with whether it is reloaded after *)
  loose : unit Hashtbl.t;
      (** narrow results whose every use reads only their low bits, so they may skip the
          zero extension to canonical form *)
}

(* Narrow add/sub/mul/shl and trunc results that only feed narrow stores,
   truncs and further low-bit arithmetic never need their upper bits cleared. *)
let loose_values addrs (f : func) =
  let narrow = function I8 | I16 | I32 -> true | _ -> false in
  let cand = Hashtbl.create 16 and strict = Hashtbl.create 64 in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          (match ins with
          | Bin (id, (Add | Sub | Mul | Shl), ty, _, _)
            when narrow ty && not (Hashtbl.mem addrs.dead id) ->
              Hashtbl.replace cand id ()
          | Cast (id, "trunc", _, _, ty) when narrow ty -> Hashtbl.replace cand id ()
          | _ -> ());
          let ok_use v =
            match (ins, v) with
            | ( ( Store (ty, Local (x, _), p, _)
                | Store_volatile (ty, Local (x, _), p, _) ),
                Local (y, _) )
              when x = y && narrow ty && p <> v ->
                true
            | Cast (_, "trunc", _, _, _), _ -> true
            | Bin (id, (Add | Sub | Mul), ty, _, _), _ ->
                narrow ty && not (Hashtbl.mem addrs.dead id)
            | Bin (id, Shl, ty, x, y), _ ->
                narrow ty && (not (Hashtbl.mem addrs.dead id)) && x = v && y <> v
            | _ -> false
          in
          Regalloc.iter_operands
            (fun v ->
              match v with
              | Local (id, _) when not (ok_use v) -> Hashtbl.replace strict id ()
              | _ -> ())
            ins)
        b.instrs;
      Regalloc.iter_term_operands
        (function Local (id, _) -> Hashtbl.replace strict id () | _ -> ())
        b.terminator)
    f.blocks;
  Hashtbl.filter_map_inplace
    (fun id () -> if Hashtbl.mem strict id then None else Some ())
    cand;
  cand

(* load; op; store back to the same place, with the loaded and computed
   values used nowhere else: code generation emits one `op [mem], y`, so the
   two values never need a register. Gives the load and op ids, the op, the
   type, the other operand, the place and the instructions after the store. *)
let same_place addrs p q =
  p = q
  ||
  match (p, q) with
  | Local (i, _), Local (j, _) -> (
      match (Hashtbl.find_opt addrs.forms i, Hashtbl.find_opt addrs.forms j) with
      | Some f, Some g -> Hashtbl.mem addrs.skip i && Hashtbl.mem addrs.skip j && f = g
      | _ -> false)
  | _ -> false

let rmw_match addrs once ins rest =
  let rec skip = function
    | Location _ :: r -> skip r
    | (Gep (id, _, _, _) | Global_ptr (id, _, _) | String_ptr (id, _, _)) :: r
      when Hashtbl.mem addrs.skip id ->
        skip r
    | r -> r
  in
  match ins with
  | Load (l, ty, p, _)
    when once l && match ty with I8 | I16 | I32 | I64 -> true | _ -> false -> (
      match skip rest with
      | Bin (r, op, ty', x, y) :: rest
        when ty' = ty && once r
             && match op with Add | Sub | And | Or | Xor -> true | _ -> false -> (
          let operand =
            match (x, y) with
            | Local (i, _), y when i = l -> Some y
            | x, Local (i, _) when i = l && op <> Sub -> Some x
            | _ -> None
          in
          match (operand, skip rest) with
          | Some y, Store (ty'', Local (s, _), p', _) :: rest
            when ty'' = ty && s = r && same_place addrs p p'
                 && match y with Local (i, _) -> i <> l | _ -> true ->
              Some (l, r, op, ty, y, p, rest)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* The values that [rmw_match] folds away *)
let rmw_temps addrs (f : func) =
  let uses = Hashtbl.create 64 in
  let count = function
    | Local (id, _) ->
        Hashtbl.replace uses id (1 + Option.value ~default:0 (Hashtbl.find_opt uses id))
    | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter (Regalloc.iter_operands count) b.instrs;
      Regalloc.iter_term_operands count b.terminator)
    f.blocks;
  let once id = Hashtbl.find_opt uses id = Some 1 in
  let temps = Hashtbl.create 8 in
  List.iter
    (fun (b : block) ->
      let rec walk = function
        | [] -> ()
        | ins :: rest -> (
            match rmw_match addrs once ins rest with
            | Some (l, r, _, _, _, _, rest) ->
                Hashtbl.replace temps l ();
                Hashtbl.replace temps r ();
                walk rest
            | None -> walk rest)
      in
      walk b.instrs)
    f.blocks;
  temps

let reserve fr bytes align =
  fr.size <- (fr.size + bytes + align - 1) / align * align;
  -fr.size

let build_frame ?xmm ctx (f : func) =
  let addrs = analyse_addrs ctx f in
  let rmw = rmw_temps addrs f in
  let skip id =
    Hashtbl.mem addrs.skip id || Hashtbl.mem addrs.dead id || Hashtbl.mem rmw id
  in
  let shared = Hashtbl.create 16 in
  let call_saves = ref [] in
  let xshared = match xmm with Some (_, x) -> x | None -> Hashtbl.create 16 in
  let homes =
    Regalloc.allocate ~skip ~operands:(operands_view addrs) ~slot_of:shared
      ~saves:call_saves f
  in
  let fr =
    {
      addrs;
      slots = Hashtbl.create 64;
      allocas = Hashtbl.create 8;
      params = Ptbl.create 8;
      size = 0;
      homes;
      xhomes =
        (match xmm with
        | Some (homes, _) -> homes
        | None ->
            Regalloc.allocate_xmm ~skip ~operands:(operands_view addrs) ~slot_of:xshared
              f);
      xshared;
      xtmp = 0;
      saved = [];
      call_saves = !call_saves;
      loose =
        (if Sys.getenv_opt "FAS_NATIVE_NOLOOSE" = None then loose_values addrs f
         else Hashtbl.create 1);
    }
  in
  let has_v128 = ref false in
  List.iter (fun (p : param) -> Ptbl.replace fr.params p.name (reserve fr 8 8)) f.params;
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          (match ins with
          | Alloca (id, ty, align) ->
              Hashtbl.replace fr.allocas id
                (reserve fr (max 1 (size_of ctx ty)) (max 1 align))
          | _ -> ());
          match instruction_result ins with
          | Some (id, ty) when Regalloc.v128 ty ->
              has_v128 := true;
              if not (Hashtbl.mem xshared id) then
                Hashtbl.replace fr.slots id (reserve fr 16 16)
          | Some (id, ty) when is_aggregate ty ->
              Hashtbl.replace fr.slots id (reserve fr (max 1 (size_of ctx ty)) 16)
          | Some (id, _) when Hashtbl.mem shared id -> ()
          | Some (id, _) -> Hashtbl.replace fr.slots id (reserve fr 8 8)
          | None -> ())
        b.instrs)
    f.blocks;
  (* phi webs sharing a slot: members take their representative's *)
  let share bytes =
    Hashtbl.iter (fun id r ->
        if id <> r then
          match Hashtbl.find_opt fr.slots r with
          | Some o -> Hashtbl.replace fr.slots id o
          | None ->
              let o = reserve fr bytes bytes in
              Hashtbl.replace fr.slots r o;
              Hashtbl.replace fr.slots id o)
  in
  share 8 shared;
  share 16 xshared;
  let used =
    Hashtbl.fold (fun _ r acc -> if List.mem r acc then acc else r :: acc) fr.homes []
  in
  let saved =
    List.filter_map
      (fun r -> if List.mem r used then Some (r, reserve fr 8 8) else None)
      Regalloc.callee_saved
  in
  let xtmp = if !has_v128 then reserve fr 16 16 else 0 in
  let fr = { fr with saved; xtmp } in
  fr.size <- (fr.size + 15) / 16 * 16;
  fr

let slot fr id =
  match Hashtbl.find_opt fr.slots id with
  | Some o -> A.Base (A.Rbp, o)
  | None -> unsupported "use of %%v%d before any definition" id

(* Position independence. A static image from [compile] / [link_files] has a
   fixed address, so symbols are reached directly: [rip + sym], or a 32-bit
   absolute displacement in an indexed operand. An object for an external
   linker ([compile_object]: `fas -c` and hosted links) may land in a PIE or
   a shared library, where neither is allowed for every symbol:
   - an indexed operand cannot carry an absolute address; the symbol's
     address goes to a base register first (lea base, [rip + sym]);
   - a symbol another module may define or interpose (exported and imported
     data, External functions and declarations) is addressed through its GOT
     entry, as LLVM's pic model does without dso_local. The linker relaxes
     the load back to a lea when the symbol resolves locally.
   Calls to functions keep Call32 (PLT32) either way. *)
let pic = ref false
let got_syms : (string, unit) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 64
let via_got s = !pic && Stdlib.Hashtbl.mem got_syms s

(* [r] <- address of [s] + [off] *)
let sym_addr a r s off =
  if via_got s then begin
    A.load_got a r s;
    if off <> 0 then A.lea a r (A.Base (r, off))
  end
  else A.lea a r (A.Rip (s, off))

(* Values *)

(* Load a scalar value into [r], canonical (zero-extended). *)
let load ctx fr r v =
  let a = ctx.a in
  match v with
  | Const (ty, n) -> A.mov_imm a r (norm_const ty n)
  | Null _ | Zero _ | Undef _ -> A.mov_imm a r 0L
  | Local (id, ty) when is_aggregate ty -> A.lea a r (slot fr id)
  | Local (id, _) when Hashtbl.mem fr.addrs.skip id -> (
      let f = Hashtbl.find fr.addrs.forms id in
      match f with
      | { anchor = Sym s; idx = None; off } -> sym_addr a r s off
      | { anchor = Frame al; idx = None; off } ->
          A.lea a r (A.Base (A.Rbp, Hashtbl.find fr.allocas al + off))
      | _ -> invalid_arg "folded address used as a value")
  | Local (id, _) -> (
      match Hashtbl.find_opt fr.homes id with
      | Some home -> A.mov_rr a r home
      | None -> A.load_zx a 8 r (slot fr id))
  | Param (name, _) -> (
      match Hashtbl.find_opt fr.homes (Regalloc.pkey name) with
      | Some home -> A.mov_rr a r home
      | None -> (
          match Ptbl.find_opt fr.params name with
          | Some o -> A.load_zx a 8 r (A.Base (A.Rbp, o))
          | None -> unsupported "unknown parameter %s" name))
  | Global (name, _) -> sym_addr a r name 0
  | Const_vector _ -> unsupported "vector constants"

let load_signed ctx fr r v =
  load ctx fr r v;
  A.sign_extend ctx.a (bits (value_ty v)) r

(* Store [r] as the result [id] of type [ty], normalizing to canonical form. *)
let rec store_result ctx fr id ty r =
  if not (Hashtbl.mem fr.loose id) then A.zero_extend ctx.a (bits ty) r;
  put ctx fr id r

(* Write a 64-bit register that is already canonical to [id]'s home. *)
and put ctx fr id r =
  match Hashtbl.find_opt fr.homes id with
  | Some home -> A.mov_rr ctx.a home r
  | None -> A.store ctx.a 8 (slot fr id) r

let home fr = function
  | Local (id, _) -> Hashtbl.find_opt fr.homes id
  | Param (name, _) -> Hashtbl.find_opt fr.homes (Regalloc.pkey name)
  | _ -> None

(* [v] as a sign-extended 32-bit immediate equal to its canonical value *)
(* 64-bit constants that do not fit an immediate, read from .rodata by the
   instructions that use them instead of going through a register *)
let const_pool : (int64, string) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 16

let pooled = function
  | Const (I64, n) when not (A.fits_i32_64 n) ->
      let name =
        match Stdlib.Hashtbl.find_opt const_pool n with
        | Some s -> s
        | None ->
            let s = Printf.sprintf "__fas_k%d" (Stdlib.Hashtbl.length const_pool) in
            Stdlib.Hashtbl.replace const_pool n s;
            s
      in
      Some (A.Rip (name, 0))
  | _ -> None

let imm32 = function
  | Const (ty, n) ->
      let c = norm_const ty n in
      if A.fits_i32_64 c then Some (Int64.to_int c) else None
  | Null _ | Zero _ | Undef _ -> Some 0
  | _ -> None

(* A register that holds [v]: its home, or [scratch] after a load. *)
let in_reg ctx fr v scratch =
  match home fr v with
  | Some r -> r
  | None ->
      load ctx fr scratch v;
      scratch

(* The memory operand for a form, loading its registers into the scratch
   registers [sb] and [si] when they have no home. *)
let mem_of_form ctx fr f ~sb ~si =
  let index () = Option.map (fun (v, sc) -> (in_reg ctx fr v si, sc)) f.idx in
  match f.anchor with
  | Sym s -> (
      let idx = index () in
      if via_got s || (!pic && idx <> None) then begin
        (* see [pic]: the address goes to [sb] (after the index, which is
           in a home or in [si]) *)
        sym_addr ctx.a sb s 0;
        match idx with
        | None -> A.Base (sb, f.off)
        | Some (index, scale) ->
            A.Sib { base = Some sb; index; scale; disp = f.off; sym = None }
      end
      else
        match idx with
        | None -> A.Rip (s, f.off)
        | Some (index, scale) ->
            A.Sib { base = None; index; scale; disp = f.off; sym = Some s })
  | Frame al -> (
      let off = Hashtbl.find fr.allocas al + f.off in
      match index () with
      | None -> A.Base (A.Rbp, off)
      | Some (index, scale) ->
          A.Sib { base = Some A.Rbp; index; scale; disp = off; sym = None })
  | Reg v -> (
      let base = in_reg ctx fr v sb in
      match index () with
      | None -> A.Base (base, f.off)
      | Some (index, scale) ->
          A.Sib { base = Some base; index; scale; disp = f.off; sym = None })

(* The memory operand addressed by pointer [p] *)
let mem_at ctx fr p ~sb ~si =
  match p with
  | Local (id, _) when Hashtbl.mem fr.addrs.skip id ->
      mem_of_form ctx fr (Hashtbl.find fr.addrs.forms id) ~sb ~si
  | p -> A.Base (in_reg ctx fr p sb, 0)

(* Where to compute [id]: its home register unless that is in [avoid]. *)
let dest_reg fr id avoid =
  match Hashtbl.find_opt fr.homes id with
  | Some r when not (List.mem r avoid) -> r
  | _ -> A.Rax

let avoid_of fr v = match home fr v with Some r -> [ r ] | None -> []

(* The stack slot of a scalar value that has no register home *)
let slot_of fr v =
  match v with
  | Local (id, ty) when (not (is_aggregate ty)) && not (Hashtbl.mem fr.addrs.skip id)
    -> (
      match (home fr v, Hashtbl.find_opt fr.slots id) with
      | None, Some o -> Some (A.Base (A.Rbp, o))
      | _ -> None)
  | Param (name, _) -> (
      match (home fr v, Ptbl.find_opt fr.params name) with
      | None, Some o -> Some (A.Base (A.Rbp, o))
      | _ -> None)
  | _ -> None

(* Floats travel as bit patterns in general registers and slots; xmm0..2
   are scratch for the instructions that need them. *)

(* Load float [v] into xmm [x], from its slot or through [scratch]. *)
let to_xmm ctx fr x v scratch =
  let double = value_ty v = F64 in
  match slot_of fr v with
  | Some m -> A.mov_xm ctx.a ~double x m
  | None -> A.mov_xr ctx.a ~double x (in_reg ctx fr v scratch)

(* Write xmm [x] as the float result [id] of type [ty]. *)
let from_xmm ctx fr id ty x =
  let d = dest_reg fr id [] in
  A.mov_rx ctx.a ~double:(ty = F64) d x;
  put ctx fr id d

(* 128-bit vectors (SSE2). They live in xmm3..xmm15 or in 16-byte aligned
   slots; xmm0..xmm2 are scratch. Constants come from a 16-byte .rodata pool,
   so every operand without a register is aligned memory. *)

let vpool : (string, string) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 16
let lane_w = function Vector (_, t) -> bits t | _ -> 64

let vec_bytes ty xs =
  let w = lane_w ty / 8 in
  let b = Buffer.create 16 in
  List.iter
    (fun k ->
      for i = 0 to w - 1 do
        Buffer.add_char b
          (Char.chr
             (Int64.to_int (Int64.logand (Int64.shift_right_logical k (8 * i)) 0xffL)))
      done)
    xs;
  Buffer.contents b

let vconst ty xs =
  let key = vec_bytes ty xs in
  let name =
    match Stdlib.Hashtbl.find_opt vpool key with
    | Some s -> s
    | None ->
        let s = Printf.sprintf "__fas_v%d" (Stdlib.Hashtbl.length vpool) in
        Stdlib.Hashtbl.replace vpool key s;
        s
  in
  A.Rip (name, 0)

(* Zero and undef as constants, so every operand has a register or memory *)
let vnorm : value -> value = function
  | Zero (Vector (n, _) as t) | Undef (Vector (n, _) as t) ->
      Const_vector (t, List.init n (fun _ -> 0L))
  | v -> v

let xhome fr = function Local (id, _) -> Hashtbl.find_opt fr.xhomes id | _ -> None

let xmem fr v =
  match vnorm v with
  | Local (id, _) when not (Hashtbl.mem fr.xhomes id) -> Some (slot fr id)
  | Const_vector (ty, xs) -> Some (vconst ty xs)
  | _ -> None

let xload ctx fr (x : int) v =
  match (v, xhome fr v) with
  | (Zero _ | Undef _), _ -> A.pop_xx ctx.a A.Pxor 64 x x
  | Const_vector (_, xs), _ when List.for_all (fun k -> k = 0L) xs ->
      A.pop_xx ctx.a A.Pxor 64 x x
  | _, Some h -> A.movdqa ctx.a x h
  | _ -> (
      match xmem fr v with
      | Some m -> A.movdqu_load ctx.a x m
      | None -> unsupported "vector operand")

let xin ctx fr v scratch =
  match xhome fr v with
  | Some h -> h
  | None ->
      xload ctx fr scratch v;
      scratch

let xdest fr id avoid =
  match Hashtbl.find_opt fr.xhomes id with
  | Some r when not (List.mem r avoid) -> r
  | _ -> 0

let xput ctx fr id x =
  match Hashtbl.find_opt fr.xhomes id with
  | Some h -> A.movdqa ctx.a h x
  | None -> A.movdqu_store ctx.a (slot fr id) x

(* [d] op= [v] *)
let xop ctx fr op w d v =
  match xhome fr v with
  | Some h -> A.pop_xx ctx.a op w d h
  | None -> (
      match xmem fr v with
      | Some m -> A.pop_xm ctx.a op w d m
      | None -> unsupported "vector operand")

let is_v128_instr = function
  | Bin (_, _, ty, _, _) | Load (_, ty, _, _) | Store (ty, _, _, _) -> Regalloc.v128 ty
  | Extract _ | Insert _ | Shuffle_zero _ -> true
  | Cast (_, "bitcast", from, _, target) -> Regalloc.v128 from || Regalloc.v128 target
  | _ -> false

let gen_v128 ctx fr ins =
  let a = ctx.a in
  let avoid v = match xhome fr v with Some h -> [ h ] | None -> [] in
  match ins with
  | Bin (id, op, ty, x, y) -> (
      let w = lane_w ty in
      let x = vnorm x and y = vnorm y in
      let simple pop commutes =
        let d0 = xdest fr id [] in
        let x, y =
          if commutes && xhome fr y = Some d0 && xhome fr x <> Some d0 then (y, x)
          else (x, y)
        in
        let d = xdest fr id (avoid y) in
        xload ctx fr d x;
        xop ctx fr pop w d y;
        xput ctx fr id d
      in
      match op with
      | Add -> simple A.Padd true
      | Sub -> simple A.Psub false
      | And -> simple A.Pand true
      | Or -> simple A.Por true
      | Xor -> simple A.Pxor true
      | Mul when w = 16 -> simple A.Pmullw true
      | Mul when w = 64 ->
          let hi_zero = function
            | Const_vector (_, xs) ->
                List.for_all (fun k -> Int64.shift_right_logical k 32 = 0L) xs
            | _ -> false
          in
          let x, y = if hi_zero x && not (hi_zero y) then (y, x) else (x, y) in
          let d = xdest fr id (avoid y) in
          (* lo(x) lo(y) + (hi(x) lo(y) + lo(x) hi(y)) << 32 *)
          xload ctx fr 1 x;
          A.pshift_imm a A.Psrl 64 1 32;
          xop ctx fr A.Pmuludq 64 1 y;
          (match y with
          | _ when hi_zero y -> ()
          | Const_vector (t, ys) ->
              (* hi(y) straight from the pool *)
              xload ctx fr 2 x;
              xop ctx fr A.Pmuludq 64 2
                (Const_vector (t, List.map (fun k -> Int64.shift_right_logical k 32) ys));
              A.pop_xx a A.Padd 64 1 2
          | _ ->
              xload ctx fr 2 y;
              A.pshift_imm a A.Psrl 64 2 32;
              xop ctx fr A.Pmuludq 64 2 x;
              A.pop_xx a A.Padd 64 1 2);
          A.pshift_imm a A.Psll 64 1 32;
          xload ctx fr d x;
          xop ctx fr A.Pmuludq 64 d y;
          A.pop_xx a A.Padd 64 d 1;
          xput ctx fr id d
      | Shl | Lshr | Ashr -> (
          let sh = match op with Shl -> A.Psll | Lshr -> A.Psrl | _ -> A.Psra in
          let d = xdest fr id (avoid y) in
          match y with
          | Const_vector (_, k :: _) ->
              xload ctx fr d x;
              A.pshift_imm a sh w d (Int64.to_int (Int64.logand k 0xffL));
              xput ctx fr id d
          | _ ->
              (* a splat: the count is lane 0 *)
              let c = xin ctx fr y 2 in
              let c =
                if w = 64 then c
                else begin
                  A.mov_rx a ~double:true A.Rax c;
                  A.zero_extend a w A.Rax;
                  A.mov_xr a ~double:true 2 A.Rax;
                  2
                end
              in
              xload ctx fr d x;
              A.pshift_xx a sh w d c;
              xput ctx fr id d)
      | _ -> unsupported "vector operation")
  | Load (id, _, p, _) ->
      let d = xdest fr id [] in
      A.movdqu_load a d (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx);
      xput ctx fr id d
  | Store (_, v, p, _) ->
      let m = mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx in
      A.movdqu_store a m (xin ctx fr v 0)
  | Insert (id, ty, v, idx, x) -> (
      let w = lane_w ty in
      let k =
        match idx with Const (_, k) -> Int64.to_int k | _ -> unsupported "lane"
      in
      let d = xdest fr id [] in
      load ctx fr A.Rax x;
      match (v, w, k) with
      | (Undef _ | Zero _), _, 0 ->
          A.mov_xr a ~double:true d A.Rax;
          xput ctx fr id d
      | _, 64, 1 ->
          A.mov_xr a ~double:true 1 A.Rax;
          xload ctx fr d v;
          A.punpcklqdq a d 1;
          xput ctx fr id d
      | _, 16, _ ->
          xload ctx fr d v;
          A.sse_rr a 0x66 0xc4 ~reg:d ~rm:(A.reg_num A.Rax);
          A.byte a k;
          xput ctx fr id d
      | _ ->
          let t = A.Base (A.Rbp, fr.xtmp) in
          A.movdqu_store a t (xin ctx fr v 1);
          A.store a (w / 8) (A.Base (A.Rbp, fr.xtmp + (k * w / 8))) A.Rax;
          A.movdqu_load a d t;
          xput ctx fr id d)
  | Extract (id, ty, v, idx) ->
      let w = lane_w ty in
      let k =
        match idx with Const (_, k) -> Int64.to_int k | _ -> unsupported "lane"
      in
      let x = xin ctx fr v 1 in
      let d = dest_reg fr id [] in
      (match w with
      | 64 ->
          if k = 0 then A.mov_rx a ~double:true d x
          else begin
            A.pshufd a 1 x 0xee;
            A.mov_rx a ~double:true d 1
          end
      | 32 ->
          if k = 0 then A.mov_rx a ~double:false d x
          else begin
            A.pshufd a 1 x k;
            A.mov_rx a ~double:false d 1
          end
      | 16 -> A.pextrw a d x k
      | _ ->
          A.pextrw a d x (k / 2);
          if k mod 2 = 1 then A.shift_imm a A.Shr d 8);
      let t = match ty with Vector (_, t) -> t | t -> t in
      store_result ctx fr id t d
  | Shuffle_zero (id, ty, v) ->
      let x = xin ctx fr v 1 in
      let d = xdest fr id [] in
      (match lane_w ty with
      | 64 -> A.pshufd a d x 0x44
      | 32 -> A.pshufd a d x 0
      | 16 ->
          A.pshuflw a d x 0;
          A.pshufd a d d 0
      | _ ->
          A.movdqa a d x;
          A.punpcklbw a d d;
          A.pshuflw a d d 0;
          A.pshufd a d d 0);
      xput ctx fr id d
  | Cast (id, _, from, v, target) when Regalloc.v128 from && Regalloc.v128 target ->
      xput ctx fr id (xin ctx fr v 0)
  | _ -> unsupported "vector operation"

let float_cmp = function Foeq | Fune | Folt | Fole | Fogt | Foge -> true | _ -> false

(* Set the flags for [cmp c x y]. A float compare sets them as ucomis does
   (an unsigned compare, PF on unordered), with the operands of < and <=
   swapped so that every ordered compare reads `a` or `ae`. *)
let compare_flags ctx fr c x y =
  let a = ctx.a in
  let w = bits (value_ty x) in
  if float_cmp c then begin
    let x, y = match c with Folt | Fole -> (y, x) | _ -> (x, y) in
    to_xmm ctx fr 0 x A.Rax;
    to_xmm ctx fr 1 y A.Rcx;
    A.ucomi a ~double:(value_ty x = F64) 0 1
  end
  else if (match c with Slt | Sle | Sgt | Sge -> true | _ -> false) && w < 64 then begin
    load_signed ctx fr A.Rax x;
    load_signed ctx fr A.Rcx y;
    A.alu_rr a A.Cmp A.Rax A.Rcx
  end
  else
    match x with
    | Local (t, _) when Hashtbl.mem fr.addrs.tests t ->
        let v, m = Hashtbl.find fr.addrs.tests t in
        A.test_ri a (in_reg ctx fr v A.Rax) m
    | _ -> (
        let rx = in_reg ctx fr x A.Rax in
        match imm32 y with
        | Some 0 -> A.test_rr a rx rx
        | Some k -> A.alu_ri a A.Cmp rx k
        | None -> (
            (* slots hold the canonical 64-bit value, so compare in memory *)
            match slot_of fr y with
            | Some m -> A.alu_rm a A.Cmp rx m
            | None -> A.alu_rr a A.Cmp rx (in_reg ctx fr y A.Rcx)))

let mem_size _ctx ty =
  match ty with
  | I1 | I8 -> 1
  | I16 -> 2
  | I32 | F32 -> 4
  | I64 | F64 | Pointer _ -> 8
  | ty ->
      ignore (bits ty);
      unsupported "memory access of this type"

(* Copy [n] bytes from the address in rsi to the address in rdi. *)
let copy_bytes ctx n =
  A.mov_imm ctx.a A.Rcx (Int64.of_int n);
  A.rep_movsb ctx.a

(* Instructions *)

let signed_cmp = function Slt | Sle | Sgt | Sge -> true | _ -> false

let cond_of = function
  | Eq -> A.E
  | Ne -> A.Ne
  | Slt -> A.L
  | Sle -> A.Le
  | Sgt -> A.G
  | Sge -> A.Ge
  | Ult -> A.B
  | Ule -> A.Be
  | Ugt -> A.A
  | Uge -> A.Ae
  | Fogt | Folt -> A.A
  | Foge | Fole -> A.Ae
  | Foeq | Fune -> invalid_arg "cond_of: needs two flags"

let negate = function
  | A.E -> A.Ne
  | A.Ne -> A.E
  | A.L -> A.Ge
  | A.Ge -> A.L
  | A.Le -> A.G
  | A.G -> A.Le
  | A.B -> A.Ae
  | A.Ae -> A.B
  | A.Be -> A.A
  | A.A -> A.Be
  | _ -> invalid_arg "negate"

let arg_regs = [| A.Rdi; A.Rsi; A.Rdx; A.Rcx; A.R8; A.R9 |]

(* What may cross an object boundary: every scalar, passed by the SysV rules
   in [classify_args]/[gen_call] and the prologue (integers and pointers in
   rdi..r9, floats in xmm0..7, the rest on the stack; narrow integers are
   extended per [ret_extension]/param extension; variadic calls set al).
   Vectors still use this backend's own representation, and aggregates by
   value reach C through clang-built adapters, so neither is exposed.
   Variadic functions can be called but not defined here. *)
let external_abi (f : func) =
  let scalar = function
    | I1 | I8 | I16 | I32 | I64 | F32 | F64 | Pointer _ -> true
    | _ -> false
  in
  (f.ret = Void || scalar f.ret) && List.for_all (fun (p : param) -> scalar p.ty) f.params

(* Values in caller-saved registers that [ins] (a call or syscall) would
   clobber: stored to their slots first, read from there while the arguments
   are set up, and reloaded afterwards when still live. *)
let around_call ctx fr ins body =
  match List.assq_opt ins fr.call_saves with
  | None -> body ()
  | Some ks ->
      let a = ctx.a in
      let regs =
        List.map
          (fun (k, reload) ->
            let m =
              if k >= 0 then slot fr k
              else A.Base (A.Rbp, Ptbl.find fr.params (Ptbl.find Regalloc.param_names k))
            in
            (k, Hashtbl.find fr.homes k, reload, m))
          ks
      in
      List.iter
        (fun (k, r, _, m) ->
          A.store a 8 m r;
          Hashtbl.remove fr.homes k)
        regs;
      body ();
      List.iter
        (fun (k, r, reload, m) ->
          Hashtbl.replace fr.homes k r;
          if reload then A.load_zx a 8 r m)
        regs

(* The SysV x86-64 calling convention, used for every call so that Fas
   functions, C functions and function pointers all agree:
   - integer and pointer arguments take rdi, rsi, rdx, rcx, r8, r9;
   - float and double arguments take xmm0..xmm7;
   - the rest go on the stack, 8 bytes each, first argument lowest;
   - results come back in rax, or xmm0 for a float or double.
   Values are canonical (zero-extended) in our registers; C callers and
   callees only guarantee the low 32 bits of a narrow integer, extended as
   its `signext`/`zeroext` attribute says, so both edges normalize. *)
type arg_class = Gpr of int | Xmm of int | Stack of int

let is_float_ty = function F32 | F64 -> true | _ -> false

(* The width of a narrow integer; 64 for anything held whole in a register
   (or passed by address) *)
let narrow_bits = function I1 -> 1 | I8 -> 8 | I16 -> 16 | I32 -> 32 | _ -> 64

(* Where each argument of the given types goes, and how many stack words
   the call needs. *)
let classify_args tys =
  let gi = ref 0 and xi = ref 0 and si = ref 0 in
  let classes =
    List.map
      (fun ty ->
        if is_float_ty ty && !xi < 8 then (incr xi; Xmm (!xi - 1))
        else if (not (is_float_ty ty)) && !gi < 6 then (incr gi; Gpr (!gi - 1))
        else (incr si; Stack (!si - 1)))
      tys
  in
  (classes, !xi, !si)

(* [variadic]: %al must carry an upper bound on the number of vector
   registers used, for the callee's va_start. *)
let gen_call ctx fr ~variadic result ret_ty target args =
  let a = ctx.a in
  let classes, nxmm, stack_args =
    classify_args (List.map (fun (_, _, v) -> value_ty v) args)
  in
  let args = List.combine classes args in
  let pad = if stack_args mod 2 = 1 then 8 else 0 in
  if pad > 0 then A.alu_ri a A.Sub A.Rsp pad;
  (* Stack arguments, last first *)
  List.iter
    (function
      | Stack _, (_, _, v) ->
          load ctx fr A.Rax v;
          A.push a A.Rax
      | _ -> ())
    (List.rev args);
  (* Float arguments go through rax, which is no argument register. *)
  List.iter
    (function Xmm x, (_, _, v) -> to_xmm ctx fr x v A.Rax | _ -> ())
    args;
  List.iter
    (function
      | Gpr i, (_, ext, v) ->
          load ctx fr arg_regs.(i) v;
          let b = narrow_bits (value_ty v) in
          if ext = Sign_extension && b < 32 then A.sign_extend a b arg_regs.(i)
      | _ -> ())
    args;
  if variadic then A.mov_imm a A.Rax (Int64.of_int nxmm);
  (match target with
  | `Direct name -> (
      match Ptbl.find_opt ctx.func_labels name with
      | Some l -> A.call_label a l
      | None
        when List.exists
               (fun (f : func) ->
                 f.name = name && f.linkage = External && external_abi f)
               ctx.m.funcs ->
          A.call_symbol a name
      | None ->
          unsupported
            "call to `%s` requires an unsupported external ABI (only scalar \
             parameters and results are supported)"
            name)
  | `Indirect v ->
      load ctx fr A.R11 v;
      A.call_r a A.R11);
  let cleanup = (8 * stack_args) + pad in
  if cleanup > 0 then A.alu_ri a A.Add A.Rsp cleanup;
  match result with
  | Some id when is_float_ty ret_ty -> from_xmm ctx fr id ret_ty 0
  | Some id when ret_ty <> Void -> store_result ctx fr id ret_ty A.Rax
  | _ -> ()

let gen_intrinsic ctx fr result ret_ty name args =
  let a = ctx.a in
  let arg i =
    let _, _, v = List.nth args i in
    v
  in
  let prefix p = String.starts_with ~prefix:p name in
  let result_bits () = bits ret_ty in
  let store r =
    match result with Some id -> store_result ctx fr id ret_ty r | None -> ()
  in
  let fstore x =
    match result with Some id -> from_xmm ctx fr id ret_ty x | None -> ()
  in
  if prefix "llvm.lifetime." then ()
  else if prefix "llvm.fas.store_if" then begin
    (* store_if(cond, value, pointer) *)
    let _, _, v = List.nth args 1 in
    let size = bits (value_ty v) / 8 in
    let skip = A.new_label a in
    load ctx fr A.Rax (arg 0);
    A.test_rr a A.Rax A.Rax;
    A.jcc a A.E skip;
    load ctx fr A.Rcx v;
    load ctx fr A.Rdx (arg 2);
    A.store a size (A.Base (A.Rdx, 0)) A.Rcx;
    A.place a skip
  end
  else if prefix "llvm.fas.load_if" then begin
    (* load_if(cond, pointer, passthru) *)
    let size = bits ret_ty / 8 in
    let skip = A.new_label a in
    load ctx fr A.Rcx (arg 0);
    load ctx fr A.Rax (arg 2);
    A.test_rr a A.Rcx A.Rcx;
    A.jcc a A.E skip;
    load ctx fr A.Rdx (arg 1);
    A.load_zx a size A.Rax (A.Base (A.Rdx, 0));
    A.place a skip;
    store A.Rax
  end
  else if prefix "llvm.fas.umulh" || prefix "llvm.fas.smulh" then begin
    load ctx fr A.Rax (arg 0);
    load ctx fr A.Rcx (arg 1);
    if prefix "llvm.fas.umulh" then A.mul1 a A.Rcx else A.imul1 a A.Rcx;
    store A.Rdx
  end
  else if prefix "llvm.ctpop." then begin
    (* Bit-by-bit population count; M2 favours simplicity. *)
    load ctx fr A.Rcx (arg 0);
    A.mov_imm a A.Rax 0L;
    let top = A.new_label a and done_ = A.new_label a in
    A.place a top;
    A.test_rr a A.Rcx A.Rcx;
    A.jcc a A.E done_;
    A.mov_rr a A.Rdx A.Rcx;
    A.alu_ri a A.And A.Rdx 1;
    A.alu_rr a A.Add A.Rax A.Rdx;
    (* shr rcx, 1 *)
    A.ext_r a [ 0xd1 ] 5 A.Rcx;
    A.jmp a top;
    A.place a done_;
    store A.Rax
  end
  else if
    (prefix "llvm.cttz." || prefix "llvm.ctlz.")
    && match args with [ _; (_, _, Const (_, 1L)) ] -> true | _ -> false
  then begin
    (* zero is poison: a bare bsf, or bsr turned into a count from the top *)
    let w = result_bits () in
    let src = in_reg ctx fr (arg 0) A.Rax in
    let d =
      match result with
      | Some id -> (
          match Hashtbl.find_opt fr.homes id with Some r -> r | None -> A.Rdx)
      | None -> A.Rdx
    in
    if prefix "llvm.cttz." then A.rr a [ 0x0f; 0xbc ] ~reg:d ~rm:src
    else begin
      A.rr a [ 0x0f; 0xbd ] ~reg:d ~rm:src;
      A.alu_ri a A.Xor d (w - 1)
    end;
    store d
  end
  else if prefix "llvm.cttz." || prefix "llvm.ctlz." then begin
    let w = result_bits () in
    load ctx fr A.Rax (arg 0);
    A.mov_imm a A.Rdx (Int64.of_int w);
    let zero = A.new_label a in
    A.test_rr a A.Rax A.Rax;
    A.jcc a A.E zero;
    if prefix "llvm.cttz." then A.rr a [ 0x0f; 0xbc ] ~reg:A.Rdx ~rm:A.Rax (* bsf *)
    else begin
      A.rr a [ 0x0f; 0xbd ] ~reg:A.Rdx ~rm:A.Rax (* bsr: index of the top bit *);
      A.mov_imm a A.Rcx (Int64.of_int (w - 1));
      A.alu_rr a A.Sub A.Rcx A.Rdx;
      A.mov_rr a A.Rdx A.Rcx
    end;
    A.place a zero;
    store A.Rdx
  end
  else if prefix "llvm.fshl." || prefix "llvm.fshr." then begin
    (* General funnel shift, k = arg2 mod w:
         fshl a b k = (a << k) | (b >> (w - k))   (a when k = 0)
         fshr a b k = (b >> k) | (a << (w - k))   (b when k = 0)
       Fas's rotl/rotr pass a = b, but Fir allows any pair. Both cases put
       the "kept" input in rax (shifted by k) and the other one in rdx
       (shifted the other way by w - k). k = 0 is a branch, since an x86
       shift by w = 64 is a no-op instead of giving 0. *)
    let w = result_bits () in
    let left = prefix "llvm.fshl." in
    load ctx fr A.Rax (arg (if left then 0 else 1));
    load ctx fr A.Rdx (arg (if left then 1 else 0));
    load ctx fr A.Rcx (arg 2);
    A.alu_ri a A.And A.Rcx (w - 1);
    let skip = A.new_label a in
    A.jcc a A.E skip;
    A.shift_cl a (if left then A.Shl else A.Shr) A.Rax;
    A.mov_rr a A.Rsi A.Rcx;
    A.mov_imm a A.Rcx (Int64.of_int w);
    A.alu_rr a A.Sub A.Rcx A.Rsi;
    A.shift_cl a (if left then A.Shr else A.Shl) A.Rdx;
    A.alu_rr a A.Or A.Rax A.Rdx;
    (* the left shift can carry bits past w: keep the value canonical *)
    if w < 64 then A.zero_extend a w A.Rax;
    A.place a skip;
    store A.Rax
  end
  else if prefix "llvm.sqrt." then begin
    to_xmm ctx fr 0 (arg 0) A.Rax;
    A.fop_xx a ~double:(ret_ty = F64) A.Fsqrt 0 0;
    fstore 0
  end
  else if prefix "llvm.fabs." then begin
    load ctx fr A.Rax (arg 0);
    A.bit_ri a `Btr A.Rax (result_bits () - 1);
    store A.Rax
  end
  else if prefix "llvm.roundeven." || prefix "llvm.floor." || prefix "llvm.ceil." then begin
    (* SSE2 only (roundsd is SSE4.1): for |x| < 2^52 (2^23 for f32),
       (|x| + 2^52) - 2^52 rounds to the nearest even integer. floor and ceil
       then step by 1 towards x's side, and the result takes x's sign so
       that zeros keep it. Larger values, infinities and NaN are integral
       already and pass through. *)
    let double = ret_ty = F64 in
    let w = result_bits () in
    let fbits x =
      if double then Int64.bits_of_float x
      else Int64.logand (Int64.of_int32 (Int32.bits_of_float x)) 0xffff_ffffL
    in
    let pass = A.new_label a and done_ = A.new_label a in
    load ctx fr A.Rax (arg 0);
    (* rsi := sign of x *)
    A.mov_rr a A.Rsi A.Rax;
    A.shift_imm a A.Shr A.Rsi (w - 1);
    A.shift_imm a A.Shl A.Rsi (w - 1);
    A.mov_rr a A.Rdx A.Rax;
    A.bit_ri a `Btr A.Rdx (w - 1);
    A.mov_xr a ~double 0 A.Rdx;
    A.mov_imm a A.Rcx (fbits (Float.ldexp 1.0 (if double then 52 else 23)));
    A.mov_xr a ~double 1 A.Rcx;
    A.ucomi a ~double 1 0;
    A.jcc a A.Be pass;
    A.fop_xx a ~double A.Fadd 0 1;
    A.fop_xx a ~double A.Fsub 0 1;
    A.mov_rx a ~double A.Rdx 0;
    A.alu_rr a A.Or A.Rdx A.Rsi;
    if not (prefix "llvm.roundeven.") then begin
      let floor = prefix "llvm.floor." in
      let exact = A.new_label a in
      A.mov_xr a ~double 0 A.Rdx;
      A.mov_xr a ~double 2 A.Rax;
      (* floor: r > x needs r - 1; ceil: x > r needs r + 1 *)
      if floor then A.ucomi a ~double 0 2 else A.ucomi a ~double 2 0;
      A.jcc a A.Be exact;
      A.mov_imm a A.Rcx (fbits 1.0);
      A.mov_xr a ~double 1 A.Rcx;
      A.fop_xx a ~double (if floor then A.Fsub else A.Fadd) 0 1;
      A.mov_rx a ~double A.Rdx 0;
      A.alu_rr a A.Or A.Rdx A.Rsi;
      A.place a exact
    end;
    A.jmp a done_;
    A.place a pass;
    A.mov_rr a A.Rdx A.Rax;
    A.place a done_;
    store A.Rdx
  end
  else if prefix "llvm.fptosi.sat." || prefix "llvm.fptoui.sat." then begin
    (* Saturating float -> integer: NaN gives 0, out-of-range values clamp. *)
    let w = result_bits () in
    let double = String.ends_with ~suffix:".f64" name in
    let fbits x =
      if double then Int64.bits_of_float x
      else Int64.logand (Int64.of_int32 (Int32.bits_of_float x)) 0xffff_ffffL
    in
    (* xmm1 := 2^k *)
    let bound k =
      A.mov_imm a A.Rcx (fbits (Float.ldexp 1.0 k));
      A.mov_xr a ~double 1 A.Rcx
    in
    to_xmm ctx fr 0 (arg 0) A.Rax;
    A.cvt_f2si a ~double A.Rax 0;
    if prefix "llvm.fptosi." then begin
      (* below the range (and NaN) cvtt gives a value under min, or 2^63 *)
      if w < 64 then begin
        A.mov_imm a A.Rcx (Int64.neg (Int64.shift_left 1L (w - 1)));
        A.alu_rr a A.Cmp A.Rax A.Rcx;
        A.cmov a A.L A.Rax A.Rcx
      end;
      bound (w - 1);
      A.mov_imm a A.Rcx (Int64.pred (Int64.shift_left 1L (w - 1)));
      A.ucomi a ~double 0 1;
      A.cmov a A.Ae A.Rax A.Rcx;
      A.mov_imm a A.Rdx 0L;
      A.ucomi a ~double 0 0;
      A.cmov a A.P A.Rax A.Rdx
    end
    else begin
      (* negative values and NaN convert to a negative number: make it 0 *)
      A.mov_imm a A.Rdx 0L;
      A.test_rr a A.Rax A.Rax;
      A.cmov a A.S A.Rax A.Rdx;
      if w = 64 then begin
        (* [2^63, 2^64): convert x - 2^63 and set the top bit back *)
        bound 63;
        A.mov_xx a 2 0;
        A.fop_xx a ~double A.Fsub 2 1;
        A.cvt_f2si a ~double A.Rcx 2;
        A.bit_ri a `Btc A.Rcx 63;
        A.ucomi a ~double 0 1;
        A.cmov a A.Ae A.Rax A.Rcx
      end;
      bound w;
      A.mov_imm a A.Rcx (if w = 64 then -1L else Int64.pred (Int64.shift_left 1L w));
      A.ucomi a ~double 0 1;
      A.cmov a A.Ae A.Rax A.Rcx
    end;
    store A.Rax
  end
  else if
    prefix "llvm.uadd.sat." || prefix "llvm.usub.sat." || prefix "llvm.sadd.sat."
    || prefix "llvm.ssub.sat."
  then begin
    let w = result_bits () in
    let signed = prefix "llvm.s" in
    let add = prefix "llvm.uadd" || prefix "llvm.sadd" in
    if w >= 64 then begin
      (* No wider register to compute in, so use the flags: CF is the
         unsigned overflow, OF the signed one. The saturated value goes in
         rdx before the add/sub, since mov_imm may emit a flag-clobbering
         xor. *)
      load ctx fr A.Rax (arg 0);
      load ctx fr A.Rcx (arg 1);
      if signed then begin
        (* signed overflow always saturates toward x's sign:
           not (x sar 63) with bit 63 flipped is MIN for x < 0, MAX else *)
        A.mov_rr a A.Rdx A.Rax;
        A.shift_imm a A.Sar A.Rdx 63;
        A.not_ a A.Rdx;
        A.bit_ri a `Btc A.Rdx 63
      end
      else A.mov_imm a A.Rdx (if add then -1L else 0L);
      A.alu_rr a (if add then A.Add else A.Sub) A.Rax A.Rcx;
      A.cmov a (if signed then A.O else A.B) A.Rax A.Rdx;
      store A.Rax
    end
    else
    let ld r v = if signed then load_signed ctx fr r v else load ctx fr r v in
    ld A.Rax (arg 0);
    ld A.Rcx (arg 1);
    A.alu_rr a (if add then A.Add else A.Sub) A.Rax A.Rcx;
    let lo, hi =
      if signed then
        ( Int64.neg (Int64.shift_left 1L (w - 1)),
          Int64.pred (Int64.shift_left 1L (w - 1)) )
      else (0L, Int64.pred (Int64.shift_left 1L w))
    in
    A.mov_imm a A.Rcx lo;
    A.alu_rr a A.Cmp A.Rax A.Rcx;
    A.cmov a A.L A.Rax A.Rcx;
    A.mov_imm a A.Rcx hi;
    A.alu_rr a A.Cmp A.Rax A.Rcx;
    A.cmov a A.G A.Rax A.Rcx;
    store A.Rax
  end
  else unsupported "intrinsic %s" name

(* Atomics (calls to `__fas_atomic_<op>_<order>.<ty>`, see Ir.atomic_name).
   x86-64 is TSO: every naturally aligned load is an acquire load and every
   store a release store, so only seq_cst stores need xchg and only seq_cst
   fences need mfence. The read-modify-writes use lock xadd / xchg / lock
   cmpxchg, which are full barriers whatever the ordering. and/or/xor have
   no fetch form, so they loop on cmpxchg. Only rax/rcx/rdx are used (see
   Regalloc.clobbers). Results are re-canonicalised after 1- and 2-byte
   forms, which leave the register's upper bits alone. *)
let gen_atomic ctx fr result ret name args =
  let a = ctx.a in
  let op, order =
    match Ir.parse_atomic_name name with
    | Some x -> x
    | None -> unsupported "malformed atomic %s" name
  in
  let arg i =
    let _, _, v = List.nth args i in
    v
  in
  let size_of ty = match ty with Pointer _ -> 8 | t -> bits t / 8 in
  let finish r size =
    match result with
    | Some id ->
        if size < 4 then A.zero_extend a (size * 8) r;
        store_result ctx fr id ret r
    | None -> ()
  in
  let mem = A.Base (A.Rdx, 0) in
  match op with
  | Ir.A_fence -> if order = Ir.Seq_cst then A.mfence a
  | Ir.A_load ->
      let size = size_of ret in
      load ctx fr A.Rdx (arg 0);
      A.load_zx a size A.Rax mem;
      finish A.Rax 8
  | Ir.A_store ->
      let size = size_of (value_ty (arg 1)) in
      load ctx fr A.Rax (arg 1);
      load ctx fr A.Rdx (arg 0);
      if order = Ir.Seq_cst then A.xchg_mr a size mem A.Rax else A.store a size mem A.Rax
  | Ir.A_xchg ->
      let size = size_of ret in
      load ctx fr A.Rax (arg 1);
      load ctx fr A.Rdx (arg 0);
      A.xchg_mr a size mem A.Rax;
      finish A.Rax size
  | Ir.A_add | Ir.A_sub ->
      let size = size_of ret in
      load ctx fr A.Rax (arg 1);
      load ctx fr A.Rdx (arg 0);
      if op = Ir.A_sub then A.neg a A.Rax;
      A.lock_xadd a size mem A.Rax;
      finish A.Rax size
  | Ir.A_cas ->
      let size = size_of ret in
      load ctx fr A.Rcx (arg 2);
      load ctx fr A.Rax (arg 1);
      load ctx fr A.Rdx (arg 0);
      A.lock_cmpxchg a size mem A.Rcx;
      finish A.Rax size
  | Ir.A_and | Ir.A_or | Ir.A_xor ->
      (* rax = old; loop: rcx = rax op v; lock cmpxchg [p], rcx; jne loop.
         v is reloaded each round since rcx is the scratch for the new value
         and there is no fourth scratch register. *)
      let size = size_of ret in
      load ctx fr A.Rdx (arg 0);
      A.load_zx a size A.Rax mem;
      let top = A.new_label a in
      A.place a top;
      load ctx fr A.Rcx (arg 1);
      A.alu_rr a (match op with Ir.A_and -> A.And | Ir.A_or -> A.Or | _ -> A.Xor) A.Rcx A.Rax;
      load ctx fr A.Rdx (arg 0);
      A.lock_cmpxchg a size mem A.Rcx;
      A.jcc a A.Ne top;
      finish A.Rax size

let gen_instr ctx fr ins =
  let a = ctx.a in
  match ins with
  | Location _ -> ()
  | Bin (id, _, _, _, _) when Hashtbl.mem fr.addrs.dead id -> ()
  | ins when is_v128_instr ins -> gen_v128 ctx fr ins
  | Bin (id, op, ty, x, y) -> (
      match op with
      | Sub when imm32 x = Some 0 ->
          let d = dest_reg fr id [] in
          load ctx fr d y;
          A.neg a d;
          store_result ctx fr id ty d
      | Xor
        when (ty = I64 || ty = I32)
             && List.exists
                  (function
                    | Const (_, n) -> norm_const ty n = norm_const ty (-1L) | _ -> false)
                  [ x; y ] ->
          (* xor with all ones: not (the 32-bit form zero-extends) *)
          let v = match y with Const _ -> x | _ -> y in
          let d = dest_reg fr id [] in
          load ctx fr d v;
          A.not_ a ~w:(ty = I64) d;
          put ctx fr id d
      | (Add | Sub | And | Or | Xor)
        when Hashtbl.find_opt fr.homes id = None
             && (ty = I64 || match op with And | Or | Xor -> true | _ -> false)
             && (slot_of fr x = Some (slot fr id)
                || (op <> Sub && slot_of fr y = Some (slot fr id))) -> (
          (* The result shares its stack slot with an operand (a spilled
             phi web): update the slot in place. *)
          let y = if slot_of fr x = Some (slot fr id) then y else x in
          let alu =
            match op with
            | Add -> A.Add
            | Sub -> A.Sub
            | And -> A.And
            | Or -> A.Or
            | _ -> A.Xor
          in
          match imm32 y with
          | Some k -> A.alu_mi a alu 8 (slot fr id) k
          | None -> A.alu_mr a alu 8 (slot fr id) (in_reg ctx fr y A.Rcx))
      | Add | Sub | And | Or | Xor | Mul ->
          let commutes = op <> Sub in
          (* When the result's home already holds [y], compute [y op x]. *)
          let x, y =
            match (Hashtbl.find_opt fr.homes id, home fr y) with
            | Some r, Some ry when commutes && r = ry && home fr x <> Some r -> (y, x)
            | _ -> (x, y)
          in
          let x, y =
            if commutes && imm32 x <> None && imm32 y = None then (y, x) else (x, y)
          in
          let d = dest_reg fr id (avoid_of fr y) in
          let alu =
            match op with
            | Add -> A.Add
            | Sub -> A.Sub
            | And -> A.And
            | Or -> A.Or
            | Xor -> A.Xor
            | _ -> A.Cmp
          in
          (match (op, imm32 y) with
          (* Three-operand add: lea leaves both inputs in their registers *)
          | Add, k
            when home fr x <> None
                 && home fr x <> Some d
                 && (k <> None || (home fr y <> None && home fr y <> Some d)) -> (
              let hx = Option.get (home fr x) in
              let index, disp =
                match k with
                | Some k -> (None, k)
                | None -> (Some (Option.get (home fr y), 1), 0)
              in
              match index with
              | None -> A.lea a d (A.Base (hx, disp))
              | Some (index, scale) ->
                  A.lea a d (A.Sib { base = Some hx; index; scale; disp; sym = None }))
          | Mul, Some k -> A.imul_rri a d (in_reg ctx fr x d) k
          | Mul, None -> (
              load ctx fr d x;
              match (pooled y, slot_of fr y) with
              | Some m, _ | None, Some m -> A.imul_rm a d m
              | None, None -> A.imul_rr a d (in_reg ctx fr y A.Rcx))
          | _, Some k ->
              load ctx fr d x;
              A.alu_ri a alu d k
          | _, None -> (
              load ctx fr d x;
              match (pooled y, slot_of fr y) with
              | Some m, _ | None, Some m -> A.alu_rm a alu d m
              | None, None -> A.alu_rr a alu d (in_reg ctx fr y A.Rcx)));
          (* Bitwise operations on canonical (zero-extended) inputs stay
             canonical. *)
          if match op with And | Or | Xor -> true | _ -> false then put ctx fr id d
          else store_result ctx fr id ty d
      | Udiv | Urem ->
          load ctx fr A.Rax x;
          load ctx fr A.Rcx y;
          A.mov_imm a A.Rdx 0L;
          A.div a A.Rcx;
          put ctx fr id (if op = Udiv then A.Rax else A.Rdx)
      | Sdiv | Srem ->
          load_signed ctx fr A.Rax x;
          load_signed ctx fr A.Rcx y;
          A.cqo a;
          A.idiv a A.Rcx;
          store_result ctx fr id ty (if op = Sdiv then A.Rax else A.Rdx)
      | Shl | Lshr | Ashr ->
          let d = dest_reg fr id (avoid_of fr y) in
          if op = Ashr then load_signed ctx fr d x else load ctx fr d x;
          let sh = match op with Shl -> A.Shl | Lshr -> A.Shr | _ -> A.Sar in
          (match y with
          | Const (_, k) -> A.shift_imm a sh d (Int64.to_int k)
          | _ ->
              load ctx fr A.Rcx y;
              A.shift_cl a sh d);
          if op = Lshr then put ctx fr id d else store_result ctx fr id ty d
      | Fadd | Fsub | Fmul | Fdiv ->
          let double = ty = F64 in
          to_xmm ctx fr 0 x A.Rax;
          let fop =
            match op with
            | Fadd -> A.Fadd
            | Fsub -> A.Fsub
            | Fmul -> A.Fmul
            | _ -> A.Fdiv
          in
          (match slot_of fr y with
          | Some m -> A.fop_xm a ~double fop 0 m
          | None ->
              to_xmm ctx fr 1 y A.Rcx;
              A.fop_xx a ~double fop 0 1);
          from_xmm ctx fr id ty 0)
  | Fneg (id, ty, x) ->
      let d = dest_reg fr id [] in
      load ctx fr d x;
      A.bit_ri a `Btc d (bits ty - 1);
      put ctx fr id d
  | Cmp (id, ((Foeq | Fune) as c), _, x, y) ->
      compare_flags ctx fr c x y;
      let c1, c2, op = if c = Foeq then (A.E, A.Np, A.And) else (A.Ne, A.P, A.Or) in
      A.setcc a c1 A.Rax;
      A.setcc a c2 A.Rcx;
      A.alu_rr a op A.Rax A.Rcx;
      put ctx fr id A.Rax
  | Cmp (id, c, _, x, y) ->
      compare_flags ctx fr c x y;
      (* setcc zero-extends: the result is canonical *)
      let d = dest_reg fr id [] in
      A.setcc a (cond_of c) d;
      put ctx fr id d
  | Alloca (id, _, _) when Hashtbl.mem fr.addrs.skip id -> ()
  | Alloca (id, _, _) ->
      A.lea a A.Rax (A.Base (A.Rbp, Hashtbl.find fr.allocas id));
      put ctx fr id A.Rax
  | Load (id, ty, p, _) | Load_volatile (id, ty, p, _) ->
      if is_aggregate ty then begin
        load ctx fr A.Rsi p;
        A.lea a A.Rdi (slot fr id);
        copy_bytes ctx (size_of ctx ty)
      end
      else begin
        let mem = mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx in
        let d = dest_reg fr id [] in
        A.load_zx a (mem_size ctx ty) d mem;
        put ctx fr id d
      end
  | Store (ty, v, p, _) | Store_volatile (ty, v, p, _) ->
      if is_aggregate ty then begin
        load ctx fr A.Rdi p;
        match v with
        | Zero _ | Undef _ | Null _ ->
            A.mov_imm a A.Rax 0L;
            A.mov_imm a A.Rcx (Int64.of_int (size_of ctx ty));
            A.rep_stosb a
        | Local _ ->
            load ctx fr A.Rsi v;
            copy_bytes ctx (size_of ctx ty)
        | _ -> unsupported "aggregate store of a constant"
      end
      else begin
        let size = mem_size ctx ty in
        let imm =
          match v with
          (* compare as Int64: Int64.to_int drops bit 63, so 0x8000...0
             would look like 0 *)
          | Const (_, c) when size < 8 || A.fits_i32_64 c ->
              Some (Int64.to_int c)
          | Null _ -> Some 0
          | _ -> None
        in
        match imm with
        | Some c -> A.mov_mi a size (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) c
        | None ->
            let src = in_reg ctx fr v A.Rcx in
            A.store a size (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) src
      end
  | (Gep (id, _, _, _) | Global_ptr (id, _, _) | String_ptr (id, _, _))
    when Hashtbl.mem fr.addrs.skip id ->
      ()
  | Gep (id, _, _, _) when Hashtbl.mem fr.addrs.forms id ->
      let mem =
        mem_of_form ctx fr (Hashtbl.find fr.addrs.forms id) ~sb:A.Rax ~si:A.Rdx
      in
      let d = dest_reg fr id [] in
      A.lea a d mem;
      put ctx fr id d
  | Gep (id, ty, p, indices) ->
      load ctx fr A.Rax p;
      let step size v =
        match v with
        | Const (ity, n) ->
            let w = bits ity in
            let n =
              if w >= 64 then n
              else Int64.shift_right (Int64.shift_left n (64 - w)) (64 - w)
            in
            let off = Int64.mul n (Int64.of_int size) in
            if A.fits_i32_64 off then (
              if off <> 0L then A.alu_ri a A.Add A.Rax (Int64.to_int off))
            else begin
              A.mov_imm a A.Rcx off;
              A.alu_rr a A.Add A.Rax A.Rcx
            end
        | _ ->
            load_signed ctx fr A.Rcx v;
            if size <> 1 then begin
              A.mov_imm a A.Rdx (Int64.of_int size);
              A.imul_rr a A.Rcx A.Rdx
            end;
            A.alu_rr a A.Add A.Rax A.Rcx
      in
      let rec walk ty = function
        | [] -> ()
        | idx :: rest -> (
            match ty with
            | Struct name ->
                let n =
                  match idx with
                  | Zero -> 0
                  | Index (Const (_, n)) -> Int64.to_int n
                  | Index _ -> unsupported "struct field index is not constant"
                in
                let off = field_offset ctx name n in
                if off <> 0 then A.alu_ri a A.Add A.Rax off;
                let d =
                  List.find (fun (d : struct_def) -> d.name = name) ctx.m.structs
                in
                walk (List.nth d.fields n) rest
            | Array (_, elem) | Vector (_, elem) ->
                (match idx with Zero -> () | Index v -> step (size_of ctx elem) v);
                walk elem rest
            | _ -> unsupported "gep into a scalar")
      in
      (match indices with
      | [] -> ()
      | first :: rest ->
          (match first with Zero -> () | Index v -> step (size_of ctx ty) v);
          walk ty rest);
      put ctx fr id A.Rax
  | Cast (id, kind, from, v, target) -> (
      match kind with
      | "zext" | "bitcast" | "ptrtoint" | "inttoptr" | "trunc" ->
          if kind = "bitcast" && (is_aggregate target || is_aggregate from) then
            unsupported "aggregate bitcast";
          (* straight into the result's register; a canonical source is
             already its zero extension *)
          let d = dest_reg fr id [] in
          (match (kind, home fr v) with
          | "trunc", Some r when Hashtbl.mem fr.loose id ->
              if d <> r then A.mov_rr a d r
          | "trunc", _ when Hashtbl.mem fr.loose id -> load ctx fr d v
          | "trunc", Some r -> A.zero_extend_rr a (bits target) d r
          | "zext", _ -> load ctx fr d v
          | _ ->
              load ctx fr d v;
              A.zero_extend a (bits target) d);
          put ctx fr id d
      | "sext" ->
          load ctx fr A.Rax v;
          A.sign_extend a (bits from) A.Rax;
          store_result ctx fr id target A.Rax
      | "sitofp" ->
          load_signed ctx fr A.Rax v;
          A.cvt_si2f a ~double:(target = F64) 0 A.Rax;
          from_xmm ctx fr id target 0
      | "uitofp" ->
          let double = target = F64 in
          load ctx fr A.Rax v;
          if bits from < 64 then A.cvt_si2f a ~double 0 A.Rax
          else begin
            (* Above 2^63: halve keeping the low bit sticky, convert, double. *)
            let big = A.new_label a and done_ = A.new_label a in
            A.test_rr a A.Rax A.Rax;
            A.jcc a A.S big;
            A.cvt_si2f a ~double 0 A.Rax;
            A.jmp a done_;
            A.place a big;
            A.mov_rr a A.Rcx A.Rax;
            A.shift_imm a A.Shr A.Rcx 1;
            A.alu_ri a A.And A.Rax 1;
            A.alu_rr a A.Or A.Rcx A.Rax;
            A.cvt_si2f a ~double 0 A.Rcx;
            A.fop_xx a ~double A.Fadd 0 0;
            A.place a done_
          end;
          from_xmm ctx fr id target 0
      | "fpext" | "fptrunc" ->
          to_xmm ctx fr 0 v A.Rax;
          A.cvt_ff a ~from_double:(kind = "fptrunc") 0 0;
          from_xmm ctx fr id target 0
      | k -> unsupported "cast %s" k)
  | Call (result, _, ret, name, args) when Ir.is_atomic_name name ->
      gen_atomic ctx fr result ret name args
  | Call (result, _, ret, name, args) ->
      if String.starts_with ~prefix:"llvm." name then
        gen_intrinsic ctx fr result ret name args
      else
        let variadic =
          List.exists (fun (g : func) -> g.name = name && g.variadic) ctx.m.funcs
        in
        around_call ctx fr ins (fun () ->
            gen_call ctx fr ~variadic result ret (`Direct name) args)
  | Call_indirect (result, _, ret, callee, args) ->
      around_call ctx fr ins (fun () ->
          (* the callee might be variadic C: setting %al costs one mov *)
          gen_call ctx fr ~variadic:true result ret (`Indirect callee) args)
  | Phi _ -> () (* copied in from the incoming slot at block entry *)
  | Select (id, c, x, y) ->
      let ty = value_ty x in
      if is_aggregate ty then unsupported "aggregate select";
      (* d = the false arm, then cmovne the true arm in; both arms are
         canonical, so the result is too *)
      let d =
        match Hashtbl.find_opt fr.homes id with
        | Some r when home fr c <> Some r && home fr x <> Some r -> r
        | _ -> A.Rax
      in
      let src =
        match (home fr x, slot_of fr x) with
        | Some r, _ -> `R r
        | None, Some m -> `M m
        | None, None ->
            load ctx fr A.Rcx x;
            `R A.Rcx
      in
      let cond =
        match (home fr c, slot_of fr c) with
        | Some r, _ -> `R r
        | None, Some m -> `M m
        | None, None ->
            load ctx fr A.Rdx c;
            `R A.Rdx
      in
      (* a mov never touches the flags, but a zero constant loads as xor *)
      load ctx fr d y;
      (match cond with `R r -> A.test_rr a r r | `M m -> A.alu_mi a A.Cmp 8 m 0);
      (match src with `R r -> A.cmov a A.Ne d r | `M m -> A.cmov_rm a A.Ne d m);
      if Hashtbl.find_opt fr.homes id <> Some d then put ctx fr id d
  | Extract _ | Insert _ | Shuffle_zero _ | Shufflevector _ ->
      unsupported "vector operations"
  | String_ptr (id, index, _) ->
      A.lea a A.Rax (A.Rip (Printf.sprintf ".str.%d" index, 0));
      put ctx fr id A.Rax
  | Global_ptr (id, name, _) ->
      sym_addr a A.Rax name 0;
      put ctx fr id A.Rax
  | Trap -> A.ud2 a
  | Syscall (id, args) ->
      around_call ctx fr ins @@ fun () ->
      let regs = [| A.Rax; A.Rdi; A.Rsi; A.Rdx; A.R10; A.R8; A.R9 |] in
      (* Load the number last: rax is also a scratch register for nothing
         here, but keep the order obvious. *)
      List.iteri (fun i v -> if i > 0 then load ctx fr regs.(i) v) args;
      load ctx fr A.Rax (List.hd args);
      A.syscall a;
      put ctx fr id A.Rax

(* Edge copies: the phis of [target] take their values for the edge from
   [from_id], as one parallel move straight into each phi's home. rax breaks
   cycles and rcx carries memory-to-memory moves. *)
type loc = R of A.reg | M of int | C of value | XR of int | XM of int | XC of value

let edge_moves ctx fr blocks from_id target =
  ignore ctx;
  let b : block = Hashtbl.find blocks target in
  let loc_of v =
    match (home fr v, v) with
    | Some r, _ -> R r
    | None, Local (id, _) when Hashtbl.mem fr.addrs.skip id -> C v
    | None, Local (id, ty) when not (is_aggregate ty) -> (
        match slot fr id with A.Base (_, o) -> M o | _ -> C v)
    | None, Param (name, _) -> M (Ptbl.find fr.params name)
    | _ -> C v
  in
  let xloc_of v =
    match (xhome fr v, v) with
    | Some r, _ -> XR r
    | None, Local (id, _) -> (
        match slot fr id with A.Base (_, o) -> XM o | _ -> XC v)
    | _ -> XC v
  in
  let moves =
    List.filter_map
      (function
        | Phi (id, ty, incoming) when Regalloc.v128 ty -> (
            match List.find_opt (fun (_, p) -> p = from_id) incoming with
            | Some (v, _) ->
                let dst = xloc_of (Local (id, ty)) and src = xloc_of v in
                if dst = src then None else Some (dst, src)
            | None ->
                unsupported "phi in b%d has no value for edge from b%d" target from_id)
        | Phi (id, ty, incoming) -> (
            if is_aggregate ty then unsupported "aggregate phi";
            match List.find_opt (fun (_, p) -> p = from_id) incoming with
            | Some (v, _) ->
                let dst = loc_of (Local (id, ty)) and src = loc_of v in
                if dst = src then None else Some (dst, src)
            | None ->
                unsupported "phi in b%d has no value for edge from b%d" target from_id)
        | _ -> None)
      b.instrs
  in
  moves

let edge_copies ctx fr blocks from_id target =
  let a = ctx.a in
  let moves = edge_moves ctx fr blocks from_id target in
  let emit dst src =
    match (dst, src) with
    | R r, R q -> A.mov_rr a r q
    | R r, M o -> A.load_zx a 8 r (A.Base (A.Rbp, o))
    | R r, C v -> load ctx fr r v
    | M o, R q -> A.store a 8 (A.Base (A.Rbp, o)) q
    | M o, M o2 ->
        A.load_zx a 8 A.Rcx (A.Base (A.Rbp, o2));
        A.store a 8 (A.Base (A.Rbp, o)) A.Rcx
    | M o, C v ->
        load ctx fr A.Rcx v;
        A.store a 8 (A.Base (A.Rbp, o)) A.Rcx
    | XR r, XR q -> A.movdqa a r q
    | XR r, XM o -> A.movdqu_load a r (A.Base (A.Rbp, o))
    | XR r, XC v -> xload ctx fr r v
    | XM o, XR q -> A.movdqu_store a (A.Base (A.Rbp, o)) q
    | XM o, XM o2 ->
        A.movdqu_load a 1 (A.Base (A.Rbp, o2));
        A.movdqu_store a (A.Base (A.Rbp, o)) 1
    | XM o, XC v ->
        xload ctx fr 1 v;
        A.movdqu_store a (A.Base (A.Rbp, o)) 1
    | _ -> invalid_arg "edge_copies"
  in
  let resolve moves temp =
    let pending = ref moves in
    while !pending <> [] do
      let is_source l = List.exists (fun (_, s) -> s = l) !pending in
      match List.find_opt (fun (d, _) -> not (is_source d)) !pending with
      | Some ((d, s) as m) ->
          emit d s;
          pending := List.filter (fun x -> x != m) !pending
      | None ->
          let d, _ = List.hd !pending in
          emit temp d;
          pending := List.map (fun (d', s) -> (d', if s = d then temp else s)) !pending
    done
  in
  let xmoves, moves =
    List.partition (function (XR _ | XM _), _ -> true | _ -> false) moves
  in
  resolve moves (R A.Rax);
  resolve xmoves (XR 0)

(* [sid] = select ([x] c [y]), [tv], [fv] as cmp; cmovCC, without the
   condition as a value. [d] takes one arm before the compare (moves keep
   the flags, but a zero constant loads with xor, so all loads come first)
   and the other arm, in a register or memory, moves in on the condition. *)
let fused_select ctx fr sid c x y tv fv =
  let a = ctx.a in
  let d =
    match Hashtbl.find_opt fr.homes sid with
    | Some r when not (List.mem r (List.concat_map (avoid_of fr) [ x; y ])) -> r
    | _ -> A.Rdx
  in
  let float = float_cmp c in
  (* the arm that moves in on the condition must be a register or memory *)
  let src v =
    match home fr v with
    | Some r when r <> d -> Some (`R r)
    | Some _ -> None
    | None -> (
        match slot_of fr v with
        | Some m -> Some (`M m)
        | None -> if d <> A.Rdx then Some `Rdx else None)
  in
  let plan =
    match (src tv, src fv) with
    | Some s, _ -> Some (fv, tv, s, cond_of c)
    | None, Some s when not float -> Some (tv, fv, s, negate (cond_of c))
    | _ -> None
  in
  match plan with
  | None -> false
  | Some (first, second, s, cc) ->
      (match s with `Rdx -> load ctx fr A.Rdx second | _ -> ());
      load ctx fr d first;
      compare_flags ctx fr c x y;
      (match s with
      | `R r -> A.cmov a cc d r
      | `Rdx -> A.cmov a cc d A.Rdx
      | `M m -> A.cmov_rm a cc d m);
      if Hashtbl.find_opt fr.homes sid <> Some d then put ctx fr sid d;
      true

let gen_function ctx (f : func) =
  let a = ctx.a in
  let fr = build_frame ctx f in
  (* values left in memory that an inner loop uses get a copy before the
     loop, and allocation runs again *)
  let f, fr =
    if not Licm.split_on then (f, fr)
    else
      let want fr = function
        | Local (id, _) ->
            not
              (Hashtbl.mem fr.addrs.skip id || Hashtbl.mem fr.addrs.dead id
             || Hashtbl.mem fr.homes id)
        | Param (name, _) -> not (Hashtbl.mem fr.homes (Regalloc.pkey name))
        | _ -> false
      in
      (* the copies are scalar and call nothing: vector homes still hold *)
      let round ?inner (f, fr) =
        match Licm.split ?inner ~want:(want fr) f with
        | None -> (f, fr)
        | Some f -> (f, build_frame ~xmm:(fr.xhomes, fr.xshared) ctx f)
      in
      round ~inner:true (round (f, fr))
  in
  let blocks = Hashtbl.create 16 in
  List.iter (fun (b : block) -> Hashtbl.replace blocks b.id b) f.blocks;
  let labels = Hashtbl.create 16 in
  List.iter (fun (b : block) -> Hashtbl.replace labels b.id (A.new_label a)) f.blocks;
  let block_label id = Hashtbl.find labels id in
  A.align a 16;
  A.place a (Ptbl.find ctx.func_labels f.name);
  let start = A.pos a in
  A.push a A.Rbp;
  A.mov_rr a A.Rbp A.Rsp;
  if fr.size > 0 then A.alu_ri a A.Sub A.Rsp fr.size;
  (* Parameters to their slots, canonical (see [classify_args]) *)
  let classes, _, _ = classify_args (List.map (fun (p : param) -> p.ty) f.params) in
  List.iter2
    (fun (p : param) cls ->
      let dst = A.Base (A.Rbp, Ptbl.find fr.params p.name) in
      let narrow = narrow_bits p.ty in
      match cls with
      | Gpr i ->
          A.zero_extend a narrow arg_regs.(i);
          A.store a 8 dst arg_regs.(i)
      | Xmm x ->
          A.mov_rx a ~double:(p.ty = F64) A.Rax x;
          A.store a 8 dst A.Rax
      | Stack k ->
          A.load_zx a 8 A.Rax (A.Base (A.Rbp, 16 + (8 * k)));
          A.zero_extend a narrow A.Rax;
          A.store a 8 dst A.Rax)
    f.params classes;
  List.iter (fun (r, o) -> A.store a 8 (A.Base (A.Rbp, o)) r) fr.saved;
  List.iter
    (fun (p : param) ->
      match Hashtbl.find_opt fr.homes (Regalloc.pkey p.name) with
      | Some home -> A.load_zx a 8 home (A.Base (A.Rbp, Ptbl.find fr.params p.name))
      | None -> ())
    f.params;
  let epilogue () =
    List.iter (fun (r, o) -> A.load_zx a 8 r (A.Base (A.Rbp, o))) fr.saved;
    A.leave a;
    A.ret a
  in
  (* A compare used only by its block's branch becomes cmp + jcc. *)
  let uses = Hashtbl.create 64 in
  let count v =
    match v with
    | Local (id, _) ->
        Hashtbl.replace uses id (1 + Option.value ~default:0 (Hashtbl.find_opt uses id))
    | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter (Regalloc.iter_operands count) b.instrs;
      Regalloc.iter_term_operands count b.terminator)
    f.blocks;
  let fused (b : block) =
    match (List.rev b.instrs, b.terminator) with
    | (Cmp (id, c, ty, x, y) as cmp) :: _, CondBr (Local (cid, _), _, _)
      when cid = id
           && Hashtbl.find_opt uses id = Some 1
           && (match c with
             | Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge -> true
             | Folt | Fole | Fogt | Foge -> true
             | Foeq | Fune -> false)
           && match ty with Vector _ -> false | _ -> true ->
        Some (cmp, c, x, y)
    | _ -> None
  in
  let order = Array.of_list (List.map (fun (b : block) -> b.id) f.blocks) in
  let next_block = ref (-1) in
  let has_phis id =
    List.exists (function Phi _ -> true | _ -> false) (Hashtbl.find blocks id).instrs
  in
  let once id = Hashtbl.find_opt uses id = Some 1 in
  (* load; op; store back to the same place becomes one `op [mem], y` *)
  let rmw ins rest =
    match rmw_match fr.addrs once ins rest with
    | Some (_, _, op, ty, y, p, rest) ->
        let alu =
          match op with
          | Add -> A.Add
          | Sub -> A.Sub
          | And -> A.And
          | Or -> A.Or
          | _ -> A.Xor
        in
        let size = mem_size ctx ty in
        let imm =
          match y with
          | Const (t, n) when size < 8 -> Some (Int64.to_int (norm_const t n))
          | _ -> imm32 y
        in
        (match imm with
        | Some k -> A.alu_mi a alu size (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) k
        | None ->
            let src = in_reg ctx fr y A.Rcx in
            A.alu_mr a alu size (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) src);
        Some rest
    | None -> None
  in
  let position = Hashtbl.create 16 in
  Array.iteri (fun i id -> Hashtbl.replace position id i) order;
  let fusions = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      match fused b with Some f -> Hashtbl.replace fusions b.id f | None -> ())
    f.blocks;
  (* Instructions that emit no code of their own *)
  let silent ins =
    match ins with
    | Phi _ | Location _ -> true
    | Bin (id, _, _, _, _) -> Hashtbl.mem fr.addrs.dead id
    | Gep (id, _, _, _)
    | Global_ptr (id, _, _)
    | String_ptr (id, _, _)
    | Alloca (id, _, _) ->
        Hashtbl.mem fr.addrs.skip id
    | _ -> false
  in
  let plain from_id target = edge_moves ctx fr blocks from_id target = [] in
  (* A block with no code and no copies on to its jump target can be jumped
     over: jumps to it go straight on. *)
  let rec forward seen id =
    let b : block = Hashtbl.find blocks id in
    match b.terminator with
    | Br next
      when (not (List.mem id seen)) && List.for_all silent b.instrs && plain id next ->
        forward (id :: seen) next
    | _ -> id
  in
  let forward id = forward [] id in
  let rotating = ref false in
  (* A loop header that only compares and branches, possibly after a little
     cheap arithmetic, is copied to the end of each back edge, so the loop runs
     one branch per iteration. Recomputing a header value there is safe: its
     register conflicts with everything live into the header. *)
  let cheap = function
    | Bin
        ( id,
          (Add | Sub | And | Or | Xor | Shl | Lshr | Ashr),
          (I8 | I16 | I32 | I64),
          _,
          _ ) ->
        not (Hashtbl.mem fr.addrs.dead id)
    | _ -> false
  in
  let rec jump_to from_id target =
    edge_copies ctx fr blocks from_id target;
    let t = forward target in
    let header = Hashtbl.find blocks t in
    match (Hashtbl.find_opt fusions t, header.terminator) with
    | Some ((cmp, c, x, y) : instr * cmp * value * value), CondBr (_, yes, no)
      when (not !rotating) && t <> !next_block
           && Hashtbl.find position t <= Hashtbl.find position from_id
           && List.for_all
                (fun ins -> ins == cmp || silent ins || cheap ins)
                header.instrs
           && List.length (List.filter cheap header.instrs) <= 2 ->
        rotating := true;
        List.iter (fun ins -> if cheap ins then gen_instr ctx fr ins) header.instrs;
        compare_flags ctx fr c x y;
        branch t (cond_of c) yes no;
        rotating := false
    | _ -> if target <> !next_block && t <> !next_block then A.jmp a (block_label t)
  (* Branch on condition [c] (already in the flags) to [yes] or [no]. *)
  and branch from_id c yes no =
    let py = plain from_id yes and pn = plain from_id no in
    if py && pn then begin
      let y = forward yes and n = forward no in
      if y = !next_block || yes = !next_block then A.jcc a (negate c) (block_label n)
      else begin
        A.jcc a c (block_label y);
        if n <> !next_block && no <> !next_block then A.jmp a (block_label n)
      end
    end
    else if py then begin
      A.jcc a c (block_label (forward yes));
      jump_to from_id no
    end
    else if pn then begin
      A.jcc a (negate c) (block_label (forward no));
      jump_to from_id yes
    end
    else begin
      let other = A.new_label a in
      A.jcc a (negate c) other;
      edge_copies ctx fr blocks from_id yes;
      A.jmp a (block_label yes);
      A.place a other;
      jump_to from_id no
    end
  in
  List.iteri
    (fun i (b : block) ->
      next_block := if i + 1 < Array.length order then order.(i + 1) else -1;
      A.place a (block_label b.id);
      let fusion = fused b in
      let rec emit = function
        | [] -> ()
        | Cmp (id, c, cty, x, y) :: Select (sid, Local (cid, _), tv, fv) :: rest
          when cid = id
               && Hashtbl.find_opt uses id = Some 1
               && (match c with Foeq | Fune -> false | _ -> true)
               && (match cty with Vector _ -> false | _ -> true)
               && (not (is_aggregate (value_ty tv)))
               && fused_select ctx fr sid c x y tv fv ->
            emit rest
        | ins :: rest -> (
            match rmw ins rest with
            | Some rest -> emit rest
            | None ->
                (match fusion with
                | Some (cmp, _, _, _) when ins == cmp -> ()
                | _ -> gen_instr ctx fr ins);
                emit rest)
      in
      emit b.instrs;
      match b.terminator with
      | Ret None -> epilogue ()
      | Ret (Some (_, v)) when is_float_ty (value_ty v) ->
          to_xmm ctx fr 0 v A.Rax;
          epilogue ()
      | Ret (Some (_, v)) ->
          load ctx fr A.Rax v;
          let b = narrow_bits (value_ty v) in
          if f.ret_extension = Sign_extension && b < 32 then A.sign_extend a b A.Rax;
          epilogue ()
      | Br target -> jump_to b.id target
      | CondBr (_, yes, no) when fusion <> None ->
          let _, c, x, y = Option.get fusion in
          compare_flags ctx fr c x y;
          branch b.id (cond_of c) yes no
      | CondBr (c, yes, no) ->
          load ctx fr A.Rax c;
          A.test_rr a A.Rax A.Rax;
          branch b.id A.Ne yes no
      | Switch (ty, v, cases, default) ->
          load ctx fr A.Rax v;
          List.iter
            (fun (k, target) ->
              A.mov_imm a A.Rcx (norm_const ty k);
              A.alu_rr a A.Cmp A.Rax A.Rcx;
              if has_phis target then begin
                (* not the last code of the block: never elide this jump *)
                let next = A.new_label a in
                A.jcc a A.Ne next;
                edge_copies ctx fr blocks b.id target;
                A.jmp a (block_label target);
                A.place a next
              end
              else A.jcc a A.E (block_label target))
            cases;
          jump_to b.id default
      | Unreachable -> A.ud2 a)
    f.blocks;
  A.pos a - start

(* Data *)

type data_out = {
  buf : Buffer.t;
  mutable alignment : int;
  mutable relocs : A.reloc list;
  mutable syms : Elf64.symbol list;
}

let place_data (d : data_out) section ~name ~align ~size write =
  d.alignment <- max d.alignment align;
  while Buffer.length d.buf mod max 1 align <> 0 do
    Buffer.add_char d.buf '\000'
  done;
  let offset = Buffer.length d.buf in
  write offset;
  d.syms <- { Elf64.name; section; offset; size; func = false } :: d.syms

let int_le buf size v =
  for i = 0 to size - 1 do
    Buffer.add_char buf
      (Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical v (8 * i)) land 0xff))
  done

let timing = Sys.getenv_opt "FAS_NATIVE_TIME" = Some "1"

(* [t name f x] runs [f x], reporting its time when FAS_NATIVE_TIME=1 *)
let t name f x =
  if not timing then f x
  else
    let t0 = Unix.gettimeofday () in
    let r = f x in
    Printf.eprintf "%-14s %8.2f ms\n%!" name ((Unix.gettimeofday () -. t0) *. 1000.);
    r

let generate ~startup (m : module_) =
  if timing then begin
    Printf.eprintf "front end      %8.2f ms (cpu)\n%!" (Sys.time () *. 1000.);
    let n = ref 0 and nb = ref 0 and nl = ref 0 in
    List.iter
      (fun (f : func) ->
        List.iter
          (fun (b : block) ->
            incr nb;
            n := !n + List.length b.instrs;
            nl :=
              !nl
              + List.length
                  (List.filter (function Location _ -> true | _ -> false) b.instrs))
          f.blocks)
      m.funcs;
    Printf.eprintf "input          %d instrs (%d locations), %d blocks, %d funcs\n%!" !n
      !nl !nb (List.length m.funcs)
  end;
  try
    if m.target_triple <> Target_layout.x86_64_linux.triple then
      unsupported "only Linux x86-64 object emission is supported";
    pic := not startup;
    Stdlib.Hashtbl.reset got_syms;
    if !pic then begin
      List.iter
        (fun (f : func) ->
          if f.linkage = External then Stdlib.Hashtbl.replace got_syms f.name ())
        m.funcs;
      List.iter
        (function
          | Storage_global { name; linkage = Ast.Export_c | Import_c | Import_const_c; _ }
            ->
              Stdlib.Hashtbl.replace got_syms name ()
          | _ -> ())
        m.globals
    end;
    (* No debug info yet: source locations only slow every pass down *)
    let m =
      {
        m with
        funcs =
          List.map
            (fun (f : func) ->
              {
                f with
                blocks =
                  List.map
                    (fun (b : block) ->
                      {
                        b with
                        instrs =
                          List.filter
                            (function Location _ -> false | _ -> true)
                            b.instrs;
                      })
                    f.blocks;
              })
            m.funcs;
      }
    in
    (* The inliner judges a vector function by its vector instructions, not
       by the lanes scalarization turns them into *)
    let vector_sizes = Ptbl.create 8 in
    List.iter
      (fun (f : func) ->
        if Scalarize.has_vectors f then
          Ptbl.replace vector_sizes f.name (Scalarize.vector_size f))
      m.funcs;
    (* Vector functions inline before scalarization, so their lanes join the
       caller's lane-major runs instead of going through memory *)
    let m =
      if Sys.getenv_opt "FAS_NATIVE_NOINLINE" = Some "1" || Ptbl.length vector_sizes = 0
      then m
      else
        match
          t "vinline"
            (Inline.module_ ~only:(Ptbl.mem vector_sizes) ~sizes:vector_sizes)
            m
        with
        | m', true ->
            {
              m' with
              funcs =
                List.map2
                  (fun (f : func) g -> if f == g then g else Mem2reg.merge g)
                  m.funcs m'.funcs;
            }
        | m, false -> m
    in
    let m = t "runtime" Native_runtime.add m in
    let m = t "scalarize" Scalarize.module_ m in
    (* After scalarization, so vector fma arrives as scalar lanes *)
    let m = t "runtime_calls" Native_runtime.rewrite m in
    let m = t "promote" Mem2reg.module_ m in
    let m = t "fold" Ir_fold.module_ m in
    let m =
      t "demanded"
        (fun m -> { m with funcs = List.map Mem2reg.demanded_bits m.funcs })
        m
    in
    let m = t "cleanup" Mem2reg.cleanup m in
    let m =
      t "unroll"
        (fun m ->
          {
            m with
            funcs =
              List.map
                (fun f ->
                  match Unroll.func f with
                  | f, true -> Mem2reg.cleanup_func (Ir_fold.fold_function f)
                  | f, false -> f)
                m.funcs;
          })
        m
    in
    let m =
      if Sys.getenv_opt "FAS_NATIVE_NOINLINE" = Some "1" then m
      else
        match t "inline" (Inline.module_ ~sizes:vector_sizes) m with
        | m', true ->
            (* Only the functions that changed need promoting again *)
            let live = Inline.reachable m' in
            let again (fs, gs) =
              List.concat
                (List.map2
                   (fun (f : func) (g : func) ->
                     if not (Ptbl.mem live g.name) then []
                     else if f == g then [ g ]
                     else
                       [
                         Mem2reg.cleanup_func
                           (Ir_fold.fold_function
                              (Mem2reg.func (Ir_fold.fold_function g)));
                       ])
                   fs gs)
            in
            { m' with funcs = t "reclean" again (m.funcs, m'.funcs) }
        | m, false -> m
    in
    let m =
      let ctx = { m; a = A.create (); func_labels = Ptbl.create 1 } in
      let merge = t "merge" (List.map Mem2reg.merge) m.funcs in
      { m with funcs = t "forward_mem" (List.map (forward_memory ctx)) merge }
    in
    let m = t "reassociate" Mem2reg.reassociate m in
    let m = t "split_addr" Mem2reg.split_addresses m in
    let m = t "fold3" Ir_fold.module_ m in
    let m = t "licm" Licm.module_ m in
    let m = t "cleanup3" Mem2reg.cleanup m in
    let m = t "sink_cmp" Mem2reg.sink_compares_module m in
    let m =
      t "nonzero"
        (fun m -> { m with funcs = List.map Mem2reg.nonzero_counts m.funcs })
        m
    in
    if Sys.getenv_opt "FAS_NATIVE_IR" = Some "1" then prerr_string (Ir.render_debug m);
    Stdlib.Hashtbl.reset const_pool;
    Stdlib.Hashtbl.reset vpool;
    let a = A.create () in
    let ctx = { m; a; func_labels = Ptbl.create 64 } in
    let defined = List.filter (fun (f : func) -> f.blocks <> []) m.funcs in
    List.iter
      (fun (f : func) ->
        if f.linkage = External && (f.variadic || not (external_abi f)) then
          unsupported "exported function `%s` requires an unsupported external ABI"
            f.name)
      defined;
    List.iter
      (fun (f : func) -> Ptbl.replace ctx.func_labels f.name (A.new_label a))
      defined;
    if startup then begin
      (* _start: argc/argv from the initial stack, call main, exit_group(result) *)
      A.alu_rr a A.Xor A.Rbp A.Rbp;
      A.load_zx a 8 A.Rdi (A.Base (A.Rsp, 0));
      A.lea a A.Rsi (A.Base (A.Rsp, 8));
      A.alu_ri a A.And A.Rsp (-16);
      (match Ptbl.find_opt ctx.func_labels "main" with
      | Some label -> A.call_label a label
      | None -> A.call_symbol a "main");
      A.rr a ~w:false [ 0x89 ] ~reg:A.Rax ~rm:A.Rdi;
      A.mov_imm a A.Rax 231L;
      A.syscall a;
      A.ud2 a
    end;
    let start_size = A.pos a in
    let sizes =
      t "codegen" (List.map (fun (f : func) -> (f, gen_function ctx f))) defined
    in
    let text, text_relocs = A.finish a in
    let func_syms =
      (if startup then
         [
           {
             Elf64.name = "_start";
             section = Elf64.Text;
             offset = 0;
             size = start_size;
             func = true;
           };
         ]
       else [])
      @ List.map
          (fun ((f : func), size) ->
            let offset =
              Option.get (A.label_offset a (Ptbl.find ctx.func_labels f.name))
            in
            { Elf64.name = f.name; section = Elf64.Text; offset; size; func = true })
          sizes
    in
    let rodata = { buf = Buffer.create 256; alignment = 16; relocs = []; syms = [] } in
    let data = { buf = Buffer.create 256; alignment = 1; relocs = []; syms = [] } in
    let sorted_pool pool =
      Stdlib.Hashtbl.to_seq pool |> List.of_seq
      |> List.sort (fun (_, a) (_, b) -> String.compare a b)
    in
    List.iter
      (fun (k, name) ->
        place_data rodata Elf64.Rodata ~name ~align:8 ~size:8 (fun _ ->
            int_le rodata.buf 8 k))
      (sorted_pool const_pool);
    List.iter
      (fun (bytes, name) ->
        place_data rodata Elf64.Rodata ~name ~align:16 ~size:16 (fun _ ->
            Buffer.add_string rodata.buf bytes))
      (sorted_pool vpool);
    let bss = ref 0 and bss_alignment = ref 1 and bss_syms = ref [] in
    List.iter
      (function
        | String_global { name; bytes } ->
            place_data rodata Elf64.Rodata ~name ~align:1 ~size:(String.length bytes)
              (fun _ -> Buffer.add_string rodata.buf bytes)
        | Array_global { name; elem_ty; elems; align } ->
            let es = size_of ctx elem_ty in
            place_data rodata Elf64.Rodata ~name ~align
              ~size:(es * List.length elems)
              (fun _ -> List.iter (int_le rodata.buf es) elems)
        | Storage_global { name; size; bytes; pointers; readonly; align; linkage; _ }
          -> (
            match linkage with
            | Ast.Import_c | Ast.Import_const_c -> ()
            | _ ->
                let zero =
                  match bytes with
                  | None -> true
                  | Some b -> String.for_all (( = ) '\000') b
                in
                if zero && pointers = [] && not readonly then begin
                  bss_alignment := max !bss_alignment align;
                  bss := (!bss + align - 1) / max 1 align * max 1 align;
                  bss_syms :=
                    {
                      Elf64.name;
                      section = Elf64.Bss;
                      offset = !bss;
                      size;
                      func = false;
                    }
                    :: !bss_syms;
                  bss := !bss + size
                end
                else
                  (* A pointer in a position-independent object needs a
                     load-time relocation, which read-only pages cannot take
                     (TEXTREL), so such a constant goes to .data *)
                  let d, section =
                    if readonly && not (!pic && pointers <> []) then
                      (rodata, Elf64.Rodata)
                    else (data, Elf64.Data)
                  in
                  place_data d section ~name ~align ~size (fun offset ->
                      (match bytes with
                      | Some b -> Buffer.add_string d.buf b
                      | None -> Buffer.add_string d.buf (String.make size '\000'));
                      List.iter
                        (fun (o, symbol, addend) ->
                          d.relocs <-
                            A.Abs64 { offset = offset + o; symbol; addend } :: d.relocs)
                        pointers)))
      m.globals;
    let referenced = Ptbl.create 64 in
    List.iter
      (List.iter (function
          | A.Rel32 { symbol; _ }
          | A.Call32 { symbol; _ }
          | A.Abs32s { symbol; _ }
          | A.Gotpcrel { symbol; _ }
          | A.Abs64 { symbol; _ }
          -> Ptbl.replace referenced symbol ()))
      [ text_relocs; rodata.relocs; data.relocs ];
    List.iter
      (fun (f : func) ->
        if f.linkage = External && Ptbl.mem referenced f.name && not (external_abi f)
        then
          unsupported
            "reference to external function `%s` requires an unsupported external ABI"
            f.name)
      m.funcs;
    let globals = Ptbl.create 64 in
    if startup then Ptbl.add globals "_start" ();
    List.iter
      (fun (f : func) -> if f.linkage = External then Ptbl.replace globals f.name ())
      m.funcs;
    List.iter
      (function
        | Storage_global { name; linkage = Ast.Export_c; _ } ->
            Ptbl.replace globals name ()
        | _ -> ())
      m.globals;
    let image =
      Native_object.emit ~global:(Ptbl.mem globals)
        ~alignments:[| 16; rodata.alignment; data.alignment; !bss_alignment |]
        {
          text;
          text_relocs;
          rodata = Buffer.to_bytes rodata.buf;
          rodata_relocs = rodata.relocs;
          data = Buffer.to_bytes data.buf;
          data_relocs = data.relocs;
          bss_size = !bss;
          symbols = func_syms @ rodata.syms @ data.syms @ !bss_syms;
          entry = "_start";
        }
    in
    Ok image
  with
  | Unsupported message -> Error ("native backend: " ^ message)
  | Fas_linker.Error.Error message -> Error ("native backend: " ^ message)
  | Invalid_argument message -> Error ("native backend internal error: " ^ message)

let compile_object m = generate ~startup:false m

let compile ?(inputs = []) m =
  match generate ~startup:true m with
  | Error _ as error -> error
  | Ok object_ -> (
      try
        Ok
          (Fas_linker.Static_link.link_inputs
             (Fas_linker.Input.Object
                { path = "<native module>"; data = Bytes.to_string object_ }
             :: inputs))
      with Fas_linker.Error.Error message ->
        Error ("native backend: linker: " ^ message))

let link_files ?(inputs = []) ?options ~output m =
  match generate ~startup:true m with
  | Error _ as error -> error
  | Ok object_ -> (
      try
        Fas_linker.Static_link.link_input_files ?options ~output
          (Fas_linker.Input.Object
             { path = "<native module>"; data = Bytes.to_string object_ }
          :: inputs);
        Ok ()
      with Fas_linker.Error.Error message ->
        Error ("native backend: linker: " ^ message))
