(* Mir_ct: the constant-time taint check on allocated machine code
   (roadmap v0.7, SPEC §8 "Constant time").

   Fir_ct has already checked the Fir module just before instruction
   selection. This check runs on what is actually emitted, after register
   allocation, so it also covers instruction selection's own expansions and
   the allocator's moves, spills and reloads. It does not trust either.

   Taint lives in physical registers, in the flags and in spill slots:

   - A secret starts at a mark: instruction selection turns each Fir
     `secret` or `declassify` into a copy into a fresh virtual register and
     records it in [func.marks]. That copy's destination is tainted
     (`secret`) or untainted (`declassify`), whatever its source.
   - Every other instruction taints the registers it writes when it reads a
     tainted register, and sets the flags' taint when it sets the flags.
     Exceptions: [xor r, r] and [sub r, r] give a constant; loads, [Movi],
     address and TLS forms give public values; a call's results are public
     (as in Fir) and it clobbers the caller-saved registers.
   - A spill taints its vreg's slot (an imported allocation's [St] the frame
     slot), and a reload carries that taint back. Other memory is public, as
     in Fir: secrecy lives in values and typed accesses, and the front end
     marks each secret read again.
   - Block entries join (or) their predecessors' exits, to a fixpoint.

   Refused with a tainted operand: a memory address (any load or store,
   x87 and atomic forms included; [lea] computes, it does not access), a
   conditional jump on tainted flags, an indirect jump or call target,
   [div]/[idiv], [bsf]/[bsr], scalar float arithmetic, comparisons and
   conversions, atomics, [rep movsb]/[rep stosb] pointers and counts,
   [syscall] arguments, dynamic stack sizes and inline asm inputs. Flags
   from a secret therefore feed only [setcc] and [cmov].

   A violation means instruction selection or the allocator introduced a
   timing leak that the source and the Fir check had excluded, so it is an
   internal error. *)

open Mir

exception Failure of string

let nphys = 32

let pidx = function
  | G r -> X64.reg_num r
  | X i -> 16 + i
  | V _ -> invalid_arg "Mir_ct.pidx"

type state = {
  regs : Bytes.t;  (** one byte per physical register: '\001' tainted *)
  mutable flags : bool;
  vslots : Bytes.t;  (** spilled vregs' slots, one bit per vreg *)
  fslots : (int, unit) Hashtbl.t;  (** tainted frame slots (imported allocations only) *)
}

let copy_state s =
  {
    regs = Bytes.copy s.regs;
    flags = s.flags;
    vslots = Bytes.copy s.vslots;
    fslots = Hashtbl.copy s.fslots;
  }

let bit_get b v = Char.code (Bytes.unsafe_get b (v lsr 3)) land (1 lsl (v land 7)) <> 0

let bit_set b v on =
  let i = v lsr 3 and m = 1 lsl (v land 7) in
  let c = Char.code (Bytes.get b i) in
  Bytes.set b i (Char.unsafe_chr (if on then c lor m else c land lnot m))

(* the join; true when [into] changed *)
let join into s =
  let changed = ref false in
  Bytes.iteri
    (fun i c ->
      let o = Bytes.get into.regs i in
      if c <> '\000' && o = '\000' then begin
        Bytes.set into.regs i '\001';
        changed := true
      end)
    s.regs;
  if s.flags && not into.flags then begin
    into.flags <- true;
    changed := true
  end;
  Bytes.iteri
    (fun i c ->
      let o = Char.code (Bytes.get into.vslots i) in
      let n = o lor Char.code c in
      if n <> o then begin
        Bytes.set into.vslots i (Char.chr n);
        changed := true
      end)
    s.vslots;
  Hashtbl.iter
    (fun k () ->
      if not (Hashtbl.mem into.fslots k) then begin
        Hashtbl.replace into.fslots k ();
        changed := true
      end)
    s.fslots;
  !changed

(* instructions that set the flags (from their operands); those that keep
   them when a count is zero also keep the old taint *)
let sets_flags = function
  | Alu _ | Alui _ | Test _ | Imul _ | Neg _ | Bsf _ | Bsr _ | Mul1 _ | Div _ | Ucomi _
  | Xadd _ | Cmpxchg _ | Cmpxchg16b _ ->
      `Set
  | Shift _ | Shd _ | Shifti _ -> `Merge
  | Ext _ | Copy _ | Movi _ | Load _ | Store _ | Fload _ | Fstore _ | Xload _ | Xstore _
  | Lea _ | Got _ | Tp _ | Tlsoff _ | Not _ | Bswap _ | Cqo | Setcc _ | Cmov _ | Fop _
  | Fsqrt _ | Movgx _ | Movxg _ | Cvtsi _ | Cvttsi _ | Cvtff _ | Rep_movsb | Rep_stosb
  | Std | Cld | Xchg _ | Mfence | Ud2 | Loc _ | Spget _ | Spset _ | Align_sp _ ->
      `Keep
  | Frem87 _ | X87 _ | Call _ | Syscall _ | Salloc _ | Asm _ -> `Clear

let run (f : func) (ra : Mir_ra.result) =
  if Hashtbl.length f.marks > 0 then begin
    let nv = Dynarray.length f.vcls in
    let nb = Array.length ra.items in
    let problems = ref [] in
    let report bi where what =
      let s = Printf.sprintf "%s: block %d: %s: a secret %s" f.name bi where what in
      if not (List.mem s !problems) then problems := s :: !problems
    in
    let init () =
      {
        regs = Bytes.make nphys '\000';
        flags = false;
        vslots = Bytes.make ((nv + 7) / 8) '\000';
        fslots = Hashtbl.create 1;
      }
    in
    let entry = Array.make nb None in
    entry.(0) <- Some (init ());
    (* [check]: report violations (the last pass, on the fixpoint) *)
    let step ~check bi (s0 : state) =
      let st = copy_state s0 in
      let t r =
        match r with V _ -> false | p -> Bytes.get st.regs (pidx p) <> '\000'
      in
      let set r on =
        match r with
        | V _ -> ()
        | p -> Bytes.set st.regs (pidx p) (if on then '\001' else '\000')
      in
      let bad ins what regs =
        if check && List.exists t regs then report bi (ins_s ins) what
      in
      let mem ins a = bad ins "address" (addr_regs a) in
      let orig before after =
        let u, d = uses_defs after in
        let any = List.exists t u in
        (* the refusals *)
        (match after with
        | Load (_, _, _, a) | Fload (_, _, a) | Xload (_, a) -> mem after a
        | Store (_, a, _) | Fstore (_, a, _) | Xstore (a, _) -> mem after a
        | X87 (Fld (_, a) | Fstp (_, a) | Fnstcw a | Fldcw a) -> mem after a
        | Xchg (_, a, _) | Xadd (_, a, _) | Cmpxchg (_, a, _) | Cmpxchg16b a ->
            mem after a;
            bad after "atomic operand" u
        | Div _ -> bad after "operand of a division" u
        | Bsf _ | Bsr _ -> bad after "operand of bsf or bsr" u
        | Fop _ | Fsqrt _ | Ucomi _ | Cvtsi _ | Cvttsi _ | Cvtff _ ->
            bad after "float operand" u
        | Call { target = Treg r; _ } -> bad after "call target" [ r ]
        | Syscall _ -> bad after "syscall argument" u
        | Rep_movsb | Rep_stosb ->
            bad after "pointer or count of a rep string instruction"
              X64.[ G Rdi; G Rsi; G Rcx ]
        | Salloc (_, n) -> bad after "dynamic stack size" [ n ]
        | Spset r -> bad after "stack pointer" [ r ]
        | Asm a -> bad after "inline asm input" a.ain
        | _ -> ());
        (* flags readers: setcc and cmov may read secret flags *)
        let fl = st.flags in
        (* the new taint of the written registers *)
        let out =
          match (before, after) with
          | Copy (V v, _), _ when Hashtbl.mem f.marks v -> Hashtbl.find f.marks v
          | _, Alu ((X64.Xor | Sub), x, y) when x = y -> false
          | ( _,
              ( Movi _ | Load _ | Fload _ | Xload _ | Got _ | Tp _ | Tlsoff _ | Spget _
              | Salloc _ ) ) ->
              false
          | _, (Call _ | Syscall _ | Asm _ | Cmpxchg _ | Cmpxchg16b _ | Xchg _ | Xadd _)
            ->
              false
          | _, (Setcc _ | Cmov _) -> any || fl
          | _, Lea (_, a) -> List.exists t (addr_regs a)
          | _ -> any
        in
        (match sets_flags after with
        | `Set ->
            st.flags <-
              (match after with
              | Alu ((X64.Xor | Sub), x, y) when x = y -> false
              | _ -> any)
        | `Merge -> st.flags <- st.flags || any
        | `Keep -> ()
        | `Clear -> st.flags <- false);
        List.iter (fun r -> set r false) (clobbers after);
        List.iter (fun r -> set r out) d
      in
      List.iter
        (function
          | Mir_ra.Orig (before, after) -> orig before after
          | Reload (v, r) -> set r (bit_get st.vslots v)
          | Spill (v, r) -> bit_set st.vslots v (t r)
          | Move (d, s) -> set d (t s)
          | Ld (k, r) -> set r (Hashtbl.mem st.fslots k)
          | St (k, r) ->
              if t r then Hashtbl.replace st.fslots k () else Hashtbl.remove st.fslots k
          | Remat (_, ins) ->
              let _, d = uses_defs ins in
              List.iter (fun r -> set r false) d
          | Elided _ -> ())
        ra.items.(bi);
      (match ra.terms.(bi) with
      | Jcc _ when check && st.flags ->
          report bi "the terminator" "condition (the flags)"
      | Jmpr (r, _) when check && t r ->
          report bi "the terminator" "indirect jump target"
      | _ -> ());
      st
    in
    let changed = ref true in
    while !changed do
      changed := false;
      for bi = 0 to nb - 1 do
        match entry.(bi) with
        | None -> ()
        | Some s ->
            let out = step ~check:false bi s in
            List.iter
              (fun succ ->
                match entry.(succ) with
                | None ->
                    entry.(succ) <- Some (copy_state out);
                    changed := true
                | Some e -> if join e out then changed := true)
              (successors ra.terms.(bi))
      done
    done;
    for bi = 0 to nb - 1 do
      match entry.(bi) with None -> () | Some s -> ignore (step ~check:true bi s)
    done;
    match List.rev !problems with
    | [] -> ()
    | ps -> raise (Failure (String.concat "\n" ps))
  end
