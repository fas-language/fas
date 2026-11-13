(* Mir_export: FAS_RA_EXPORT, the register allocation problem of every
   function, for the register allocator lab (optimizer-lab/regalloc/,
   FORMAT.md there is the format's specification).

   It is a research tool: it never changes the code Fas emits, and a
   function it cannot express is written as a "# skipped" comment.

   From selected Mir (before Mir_ra) to the regalloc2-style problem:

   1. Blocks unreachable from the entry are dropped, [Loc] markers too.
   2. Critical edges are split by new blocks holding only a jump, except
      the edges out of an indirect jump and the label edges of an asm goto,
      which cannot be retargeted ("pinned").
   3. SSA. The variables are the virtual registers plus the 32 physical
      ones. Liveness is computed per variable (walking back from the blocks
      that use it before defining it, as Mir_ra does); block parameters go
      on the iterated dominance frontier of the variable's defining blocks,
      where it is live in (pruned SSA, Cytron et al.); then a walk of the
      dominator tree renames. A register copy defines no value: its
      destination just takes the source's value (copy propagation), so the
      fixed-register copies of instruction selection become fixed
      constraints on the operands that read or write physical registers,
      and the edge copies of Mir's block parameters disappear into branch
      arguments. Physical registers read in the entry block before any
      write are the incoming arguments, defined by an [args] pseudo; one
      read anywhere else before a write is an error (Mir never keeps a
      physical register live across a block boundary). A virtual register
      read with no reaching definition gets an [undef] pseudo.
   4. Parameters whose arguments are all one value (ignoring itself and
      undefs) are replaced by that value, to a fixpoint.
   5. Static frequencies: 10^loop depth (natural loops), 0.001 for cold
      blocks (ending in halt, or all of whose successors are cold).

   Everything is deterministic: no hash-table order, ids in program order. *)

open Mir

exception Skip of string

let skip fmt = Printf.ksprintf (fun s -> raise (Skip s)) fmt

let target =
  match Sys.getenv_opt "FAS_RA_EXPORT" with Some "" | None -> None | Some p -> Some p

let enabled = target <> None

let gpr_names =
  [| "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"; "r12"; "r13"; "r14"; "r15" |]

(* physical registers as 0..31, like Mir_ra *)
let pidx = function G r -> X64.reg_num r | X i -> 16 + i | V _ -> invalid_arg "Mir_export.pidx"
let pname p = if p < 16 then gpr_names.(p) else Printf.sprintf "xmm%d" (p - 16)

(* ---------- what each instruction does to its operands ---------- *)

(* the def writes the register of use 0 (two-address forms) *)
let tied = function
  | Alu (op, _, _) | Alui (op, _, _) -> op <> X64.Cmp
  | Imul _ | Cmov _ | Fop _ | Shift _ | Shifti _ | Shd _ | Neg _ | Not _ | Bswap _ | Xchg _ | Xadd _ -> true
  | _ -> false

(* uses (by index in [uses_defs]) that x86 could read from memory *)
let mem_ok ins k =
  match ins with
  | Alu (X64.Cmp, _, _) | Test _ -> k = 0 || k = 1
  | Alu _ | Imul _ | Cmov _ | Fop _ | Ucomi _ | Mul1 _ -> k = 1
  | Alui (X64.Cmp, _, _) -> k = 0
  | Ext _ | Bsf _ | Bsr _ | Fsqrt _ | Movgx _ | Cvtsi _ | Cvttsi _ | Cvtff _ -> k = 0
  | Div _ -> k = 2
  | Call { target = Treg _; _ } -> k = 0
  | _ -> false

(* the use that may swap with use 0 *)
let commutes = function
  | Alu ((X64.Add | And | Or | Xor), _, _) | Imul _ | Fop ((X64.Fadd | Fmul), _, _, _) -> true
  | _ -> false

let reads_flags = function Setcc _ | Cmov _ -> true | _ -> false

(* conservative: whatever might change the flags *)
let writes_flags = function
  | Copy _ | Movi _ | Load _ | Store _ | Fload _ | Fstore _ | Xload _ | Xstore _ | Lea _ | Got _ | Tp _ | Tlsoff _
  | Not _ | Bswap _ | Ext _ | Cqo | Setcc _ | Cmov _ | Fop _ | Fsqrt _ | Movgx _ | Movxg _ | Cvtsi _ | Cvttsi _
  | Cvtff _ | Rep_movsb | Rep_stosb | Xchg _ | Mfence | Ud2 | Loc _ | Spget _ | Spset _ | X87 (Fld _ | Fstp _ | Farith _ | Fsqrt87 | Fpop | Fnstcw _ | Fldcw _) ->
      false
  | _ -> true

let remat = function
  | Movi _ | Tlsoff _ -> Some 1
  | Lea (_, { base = Bslot _ | Bsym _ | Bnone | Bin; index = None; _ }) -> Some 1
  | Got _ | Tp _ -> Some 2
  | _ -> None

let opname ins =
  let p = Printf.sprintf in
  let dq b = if b then "d" else "s" in
  match ins with
  | Copy _ -> "copy"
  | Movi _ -> "movi"
  | Load (n, sx, _, _) -> p "load%d%s" n (if sx then "s" else "")
  | Store (n, _, _) -> p "store%d" n
  | Fload (d, _, _) -> "fload" ^ dq d
  | Fstore (d, _, _) -> "fstore" ^ dq d
  | Xload _ -> "xload"
  | Xstore _ -> "xstore"
  | Align_sp _ -> "align_sp"
  | Lea _ -> "lea"
  | Got _ -> "got"
  | Tp _ -> "tp"
  | Tlsoff _ -> "tlsoff"
  | Alu (op, _, _) | Alui (op, _, _) -> alu_s op
  | Test _ -> "test"
  | Imul _ -> "imul"
  | Shift (op, _) | Shifti (op, _, _) -> shift_s op
  | Shd (l, _, _) -> if l then "shld" else "shrd"
  | Neg _ -> "neg"
  | Not _ -> "not"
  | Bswap _ -> "bswap"
  | Ext (b, s, _, _) -> p "%sext%d" (if s then "s" else "z") b
  | Mul1 (s, _) -> if s then "imul1" else "mul1"
  | Div (s, _) -> if s then "idiv" else "div"
  | Cqo -> "cqo"
  | Setcc (c, _) -> "set" ^ cond_s c
  | Cmov (c, _, _) -> "cmov" ^ cond_s c
  | Bsf _ -> "bsf"
  | Bsr _ -> "bsr"
  | Fop (op, d, _, _) -> fop_s op ^ dq d
  | Fsqrt (d, _, _) -> "sqrt" ^ dq d
  | Ucomi (d, _, _) -> "ucomi" ^ dq d
  | Movgx (d, _, _) -> "movgx" ^ dq d
  | Movxg (d, _, _) -> "movxg" ^ dq d
  | Cvtsi (d, _, _) -> "cvtsi2" ^ dq d
  | Cvttsi (d, _, _) -> p "cvtt%s2si" (dq d)
  | Cvtff (d, _, _) -> p "cvt%s2%s" (dq d) (dq (not d))
  | Frem87 _ -> "frem87"
  | X87 _ -> "x87"
  | Call _ -> "call"
  | Syscall _ -> "syscall"
  | Rep_movsb -> "rep_movsb"
  | Rep_stosb -> "rep_stosb"
  | Std -> "std"
  | Cld -> "cld"
  | Xchg _ -> "xchg"
  | Xadd _ -> "xadd"
  | Cmpxchg _ -> "cmpxchg"
  | Mfence -> "mfence"
  | Ud2 -> "ud2"
  | Loc _ -> "loc"
  | Salloc _ -> "salloc"
  | Spget _ -> "spget"
  | Spset _ -> "spset"
  | Asm _ -> "asm"

(* ---------- the exported function ---------- *)

type con = Creg | Cfixed of int | Creuse of int

type opnd = { use : bool; mutable v : int; con : con; early : bool; mem : bool }

type xins = { op : string; attrs : string list; opnds : opnd array; clob : int list; undef : bool }

type xterm =
  | Tjump of int * int array  (** target, arguments *)
  | Tbr of int * int
  | Tret of opnd array
  | Tjmpr of opnd * int list
  | Tasmgoto of int * int array * int list
  | Thalt

let reg_str = function Creg -> "reg" | Cfixed p -> "fixed=" ^ pname p | Creuse k -> Printf.sprintf "reuse=%d" k

let fnv_basis = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L

let fnv h s =
  let h = ref h in
  String.iter (fun c -> h := Int64.mul (Int64.logxor !h (Int64.of_int (Char.code c))) fnv_prime) s;
  !h

let func_text (f : func) =
  let nv = Dynarray.length f.vcls in
  let nvar = nv + 32 in
  let var_of = function V n -> n | r -> nv + pidx r in
  let var_cls x = if x < nv then Dynarray.get f.vcls x else if x - nv < 16 then Gpr else Xmm in
  (* 1. reachable blocks, Loc markers out *)
  let nb0 = Dynarray.length f.blocks in
  let code = Dynarray.create () and term = Dynarray.create () in
  Dynarray.iter
    (fun (b : block) ->
      Dynarray.add_last code (List.filter (function Loc _ -> false | _ -> true) b.code);
      Dynarray.add_last term b.term)
    f.blocks;
  let reach = Array.make nb0 false in
  let stack = ref [ 0 ] in
  reach.(0) <- true;
  while !stack <> [] do
    let b = List.hd !stack in
    stack := List.tl !stack;
    List.iter
      (fun s ->
        if not reach.(s) then begin
          reach.(s) <- true;
          stack := s :: !stack
        end)
      (successors (Dynarray.get term b))
  done;
  (* 2. critical edges: count incoming edges (per edge, a Jcc to one
     block twice counts twice) *)
  let nin = Array.make nb0 0 in
  for b = 0 to nb0 - 1 do
    if reach.(b) then
      match Dynarray.get term b with
      | Jmpr (_, l) | Asmgoto (_, l) ->
          let fall = match Dynarray.get term b with Asmgoto (x, _) -> [ x ] | _ -> [] in
          List.iter (fun s -> nin.(s) <- nin.(s) + 1) (fall @ List.sort_uniq compare l)
      | t -> List.iter (fun s -> nin.(s) <- nin.(s) + 1) (successors t)
  done;
  let split s =
    Dynarray.add_last code [];
    Dynarray.add_last term (Jmp s);
    Dynarray.length code - 1
  in
  for b = 0 to nb0 - 1 do
    if reach.(b) then
      match Dynarray.get term b with
      | Jcc (c, t, e) ->
          let t = if nin.(t) > 1 then split t else t in
          let e = if nin.(e) > 1 then split e else e in
          Dynarray.set term b (Jcc (c, t, e))
      | Asmgoto (x, l) when l <> [] && nin.(x) > 1 -> Dynarray.set term b (Asmgoto (split x, l))
      | _ -> ()
  done;
  let nb = Dynarray.length code in
  let reach = Array.init nb (fun b -> b >= nb0 || reach.(b)) in
  let code = Array.init nb (fun b -> Array.of_list (Dynarray.get code b)) in
  let term = Array.init nb (fun b -> Dynarray.get term b) in
  let succs = Array.init nb (fun b -> if reach.(b) then List.sort_uniq compare (successors term.(b)) else []) in
  let succs_ordered =
    (* successors in terminator order, without repeats (for the output) *)
    Array.init nb (fun b ->
        if not reach.(b) then []
        else
          List.fold_left (fun acc s -> if List.mem s acc then acc else acc @ [ s ]) [] (successors term.(b)))
  in
  let preds = Array.make nb [] in
  for b = nb - 1 downto 0 do
    List.iter (fun s -> preds.(s) <- b :: preds.(s)) succs.(b)
  done;
  if preds.(0) <> [] then skip "the entry block has predecessors";
  (* dominators (Cooper, Harvey, Kennedy) over reverse postorder *)
  let po = Dynarray.create () in
  let seen = Array.make nb false in
  let st = Stack.create () in
  Stack.push (0, ref succs.(0)) st;
  seen.(0) <- true;
  while not (Stack.is_empty st) do
    let b, rest = Stack.top st in
    match !rest with
    | s :: tl ->
        rest := tl;
        if not seen.(s) then begin
          seen.(s) <- true;
          Stack.push (s, ref succs.(s)) st
        end
    | [] ->
        ignore (Stack.pop st);
        Dynarray.add_last po b
  done;
  let rpo = Array.of_list (List.rev (Dynarray.to_list po)) in
  let rpo_num = Array.make nb (-1) in
  Array.iteri (fun i b -> rpo_num.(b) <- i) rpo;
  let idom = Array.make nb (-1) in
  idom.(0) <- 0;
  let rec intersect a b =
    if a = b then a
    else if rpo_num.(a) > rpo_num.(b) then intersect idom.(a) b
    else intersect a idom.(b)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iter
      (fun b ->
        if b <> 0 then begin
          let d =
            List.fold_left
              (fun acc p -> if idom.(p) < 0 then acc else if acc < 0 then p else intersect p acc)
              (-1) preds.(b)
          in
          if d <> idom.(b) then begin
            idom.(b) <- d;
            changed := true
          end
        end)
      rpo
  done;
  (* dominance frontiers *)
  let df = Array.make nb [] in
  Array.iter
    (fun b ->
      if List.length preds.(b) > 1 then
        List.iter
          (fun p ->
            let r = ref p in
            while !r <> idom.(b) do
              if not (List.mem b df.(!r)) then df.(!r) <- b :: df.(!r);
              r := idom.(!r)
            done)
          preds.(b))
    rpo;
  (* dominator tree, children in reverse postorder *)
  let kids = Array.make nb [] in
  for i = Array.length rpo - 1 downto 1 do
    let b = rpo.(i) in
    kids.(idom.(b)) <- b :: kids.(idom.(b))
  done;
  let pre = Array.make nb 0 and post = Array.make nb 0 in
  let clock = ref 0 in
  let st = Stack.create () in
  Stack.push (0, ref kids.(0)) st;
  pre.(0) <- 0;
  while not (Stack.is_empty st) do
    let _, rest = Stack.top st in
    match !rest with
    | c :: tl ->
        rest := tl;
        incr clock;
        pre.(c) <- !clock;
        Stack.push (c, ref kids.(c)) st
    | [] ->
        let b, _ = Stack.pop st in
        incr clock;
        post.(b) <- !clock
  done;
  let dominates a b = pre.(a) <= pre.(b) && post.(b) <= post.(a) in
  (* 3. per variable: blocks that define it, blocks that read it first *)
  let defb = Array.make nvar [] and ueb = Array.make nvar [] in
  let ins_regs = function
    | Copy (d, s) -> ([ s ], [ d ])
    | i -> uses_defs i
  in
  Array.iter
    (fun b ->
      let defined = Hashtbl.create 16 in
      let use r =
        let x = var_of r in
        if (not (Hashtbl.mem defined x)) && (match ueb.(x) with b' :: _ -> b' <> b | [] -> true) then
          ueb.(x) <- b :: ueb.(x)
      in
      let def r =
        let x = var_of r in
        Hashtbl.replace defined x ();
        match defb.(x) with b' :: _ when b' = b -> () | _ -> defb.(x) <- b :: defb.(x)
      in
      Array.iter
        (fun i ->
          let u, d = ins_regs i in
          List.iter use u;
          List.iter def d;
          List.iter def (clobbers i))
        code.(b);
      List.iter use (term_uses term.(b)))
    rpo;
  let defmark = Array.make nb (-1) and inmark = Array.make nb (-1) and placed = Array.make nb (-1) in
  let params = Array.make nb [] in
  let entry_args = ref [] in
  for x = 0 to nvar - 1 do
    List.iter (fun b -> defmark.(b) <- x) defb.(x);
    (* liveness: live-in blocks *)
    let work = ref [] in
    List.iter
      (fun b ->
        if inmark.(b) <> x then begin
          inmark.(b) <- x;
          work := b :: !work
        end)
      ueb.(x);
    while !work <> [] do
      let b = List.hd !work in
      work := List.tl !work;
      List.iter
        (fun p ->
          if defmark.(p) <> x && inmark.(p) <> x then begin
            inmark.(p) <- x;
            work := p :: !work
          end)
        preds.(b)
    done;
    if x >= nv then begin
      if inmark.(0) = x then entry_args := (x - nv) :: !entry_args;
      Array.iter
        (fun b -> if b <> 0 && inmark.(b) = x then skip "physical register %s is live into block %d" (pname (x - nv)) b)
        rpo
    end
    else begin
      (* parameters on the iterated dominance frontier, where live in *)
      let work = ref defb.(x) in
      let queued = Hashtbl.create 8 in
      List.iter (fun b -> Hashtbl.replace queued b ()) defb.(x);
      while !work <> [] do
        let b = List.hd !work in
        work := List.tl !work;
        List.iter
          (fun y ->
            if placed.(y) <> x && inmark.(y) = x then begin
              placed.(y) <- x;
              params.(y) <- x :: params.(y);
              if not (Hashtbl.mem queued y) then begin
                Hashtbl.replace queued y ();
                work := y :: !work
              end
            end)
          df.(b)
      done
    end
  done;
  let params = Array.map List.rev params in
  let entry_args = List.rev !entry_args in
  (* renaming *)
  let vcls = Dynarray.create () in
  let isundef = Dynarray.create () in
  let fresh x =
    Dynarray.add_last vcls (var_cls x);
    Dynarray.add_last isundef false;
    Dynarray.length vcls - 1
  in
  let cur = Array.make nvar (-1) in
  let log = Dynarray.create () in
  let set x v =
    Dynarray.add_last log (x, cur.(x));
    cur.(x) <- v
  in
  let bparams = Array.make nb [||] in
  let bcode = Array.make nb [] in
  let bterm = Array.make nb Thalt in
  let pinned_args = Array.make nb [] in
  (* pinned_args.(s): (pred, values) *)
  let rename b =
    let out = ref [] in
    let emit i = out := i :: !out in
    let read x =
      if cur.(x) >= 0 then cur.(x)
      else if x >= nv then skip "physical register %s read before it is written in block %d" (pname (x - nv)) b
      else begin
        let u = fresh x in
        Dynarray.set isundef u true;
        emit
          { op = "undef"; attrs = []; clob = []; undef = true;
            opnds = [| { use = false; v = u; con = Creg; early = false; mem = false } |] };
        set x u;
        u
      end
    in
    bparams.(b) <- Array.of_list (List.map (fun x -> let v = fresh x in set x v; v) params.(b));
    if b = 0 && entry_args <> [] then
      emit
        { op = "args"; attrs = []; clob = []; undef = false;
          opnds =
            Array.of_list
              (List.map
                 (fun p ->
                   let v = fresh (nv + p) in
                   set (nv + p) v;
                   { use = false; v; con = Cfixed p; early = false; mem = false })
                 entry_args) };
    let con r = match r with V _ -> Creg | _ -> Cfixed (pidx r) in
    Array.iter
      (fun i ->
        match i with
        | Copy (d, s) -> set (var_of d) (read (var_of s))
        | _ ->
            let u, d = uses_defs i in
            let uops =
              List.mapi (fun k r -> { use = true; v = read (var_of r); con = con r; early = true; mem = (match r with V _ -> mem_ok i k | _ -> false) }) u
            in
            let ain = match i with Asm a -> List.map (fun r -> pidx r) a.ain | _ -> [] in
            let dops =
              List.mapi
                (fun k r ->
                  let x = var_of r in
                  let v = fresh x in
                  let c =
                    match r with
                    | V _ when k = 0 && tied i -> Creuse 0
                    | _ -> con r
                  in
                  let early = match (i, r) with Asm _, (G _ | X _) -> not (List.mem (pidx r) ain) | _ -> false in
                  (x, { use = false; v; con = c; early; mem = false }))
                d
            in
            List.iter (fun (x, o) -> set x o.v) dops;
            let clob = List.sort_uniq compare (List.map pidx (clobbers i)) in
            (* clobbered registers hold nothing afterwards *)
            List.iter (fun p -> if not (List.exists (fun r -> (match r with V _ -> false | _ -> pidx r = p)) d) then set (nv + p) (-1)) clob;
            let attrs =
              (if reads_flags i then [ "rflags" ] else [])
              @ (if writes_flags i then [ "wflags" ] else [])
              @ (if commutes i then [ "comm=1" ] else [])
              @ (match (remat i, d) with Some c, [ _ ] -> [ Printf.sprintf "remat=%d" c ] | _ -> [])
              @ (match i with Call { rtwice; _ } -> "call" :: (if rtwice then [ "returns_twice" ] else []) | _ -> [])
            in
            emit
              { op = opname i; attrs; clob; undef = false;
                opnds = Array.of_list (uops @ List.map snd dops) })
      code.(b);
    let args s = Array.of_list (List.map read params.(s)) in
    let use_ops rs = Array.of_list (List.map (fun r -> { use = true; v = read (var_of r); con = con r; early = true; mem = false }) rs) in
    let t =
      match term.(b) with
      | Jmp s -> Tjump (s, args s)
      | Jcc (_, t, e) ->
          if params.(t) <> [] || params.(e) <> [] then skip "parameters on a branch target in block %d" b;
          Tbr (t, e)
      | Ret rs -> Tret (use_ops rs)
      | Jmpr (r, l) ->
          let o = (use_ops [ r ]).(0) in
          List.iter (fun s -> if params.(s) <> [] then pinned_args.(s) <- (b, args s) :: pinned_args.(s)) (List.sort_uniq compare l);
          Tjmpr (o, List.sort_uniq compare l)
      | Asmgoto (x, l) ->
          let a = args x in
          List.iter
            (fun s -> if s <> x && params.(s) <> [] then pinned_args.(s) <- (b, args s) :: pinned_args.(s))
            (List.sort_uniq compare l);
          Tasmgoto (x, a, List.sort_uniq compare l)
      | Halt -> Thalt
    in
    bcode.(b) <- List.rev !out;
    bterm.(b) <- t
  in
  (* dominator-tree walk, undoing each block's definitions after its subtree *)
  let st = Stack.create () in
  Stack.push (0, Dynarray.length log, ref None) st;
  while not (Stack.is_empty st) do
    let b, mark, rest = Stack.top st in
    match !rest with
    | None ->
        rename b;
        rest := Some kids.(b)
    | Some (c :: tl) ->
        rest := Some tl;
        Stack.push (c, Dynarray.length log, ref None) st
    | Some [] ->
        ignore (Stack.pop st);
        while Dynarray.length log > mark do
          let x, v = Dynarray.pop_last log in
          cur.(x) <- v
        done
  done;
  (* 4. trivial parameters *)
  let nval = Dynarray.length vcls in
  let repl = Array.init nval (fun i -> i) in
  let rec find v = if repl.(v) = v then v else begin let r = find repl.(v) in repl.(v) <- r; r end in
  let incoming = Array.make nb [] in
  (* incoming.(s): argument arrays, one per incoming edge *)
  Array.iter
    (fun b ->
      (match bterm.(b) with
      | Tjump (s, a) -> incoming.(s) <- a :: incoming.(s)
      | Tasmgoto (x, a, _) -> incoming.(x) <- a :: incoming.(x)
      | _ -> ());
      ())
    rpo;
  Array.iter (fun s -> List.iter (fun (_, a) -> incoming.(s) <- a :: incoming.(s)) pinned_args.(s)) rpo;
  let removed = Array.make nval false in
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iter
      (fun b ->
        Array.iteri
          (fun k p ->
            if not removed.(p) then begin
              let vals =
                List.sort_uniq compare
                  (List.filter_map
                     (fun a ->
                       let v = find a.(k) in
                       if v = p || Dynarray.get isundef v then None else Some v)
                     incoming.(b))
              in
              match vals with
              | [ v ] ->
                  removed.(p) <- true;
                  repl.(p) <- v;
                  changed := true
              | _ -> ()
            end)
          bparams.(b))
      rpo
  done;
  (* undef pseudos nothing reads any more go *)
  let used = Array.make nval false in
  let mark_ops = Array.iter (fun o -> if o.use then used.(find o.v) <- true) in
  Array.iter
    (fun b ->
      List.iter (fun i -> mark_ops i.opnds) bcode.(b);
      let kept s a = Array.iteri (fun k v -> if not removed.(bparams.(s).(k)) then used.(find v) <- true) a in
      match bterm.(b) with
      | Tjump (s, a) | Tasmgoto (s, a, _) -> kept s a
      | Tret ops -> mark_ops ops
      | Tjmpr (o, _) -> mark_ops [| o |]
      | _ -> ())
    rpo;
  Array.iter
    (fun s ->
      List.iter (fun (_, a) -> Array.iteri (fun k v -> if not removed.(bparams.(s).(k)) then used.(find v) <- true) a) pinned_args.(s))
    rpo;
  (* 5. loops and frequencies *)
  let depth = Array.make nb 0 in
  let headers = Dynarray.create () in
  Array.iter
    (fun h ->
      let latches = List.filter (fun p -> dominates h p) preds.(h) in
      if latches <> [] then Dynarray.add_last headers (h, latches))
    rpo;
  let inloop = Array.make nb (-1) in
  Dynarray.iter
    (fun (h, latches) ->
      inloop.(h) <- h;
      depth.(h) <- depth.(h) + 1;
      let work = ref latches in
      while !work <> [] do
        let b = List.hd !work in
        work := List.tl !work;
        if inloop.(b) <> h then begin
          inloop.(b) <- h;
          depth.(b) <- depth.(b) + 1;
          List.iter (fun p -> if rpo_num.(p) >= 0 then work := p :: !work) preds.(b)
        end
      done)
    headers;
  let cold = Array.make nb false in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = Array.length rpo - 1 downto 0 do
      let b = rpo.(i) in
      if not cold.(b) then
        let c = (match term.(b) with Halt -> true | _ -> false) || (succs.(b) <> [] && List.for_all (fun s -> cold.(s)) succs.(b)) in
        if c then begin
          cold.(b) <- true;
          changed := true
        end
    done
  done;
  (* output: dense block and value numbers in reverse postorder, so every
     block comes after its immediate dominator (regalloc3 needs that) *)
  let order = Array.to_list rpo in
  let bnum = Array.make nb (-1) in
  List.iteri (fun i b -> bnum.(b) <- i) order;
  let vnum = Array.make nval (-1) in
  let nout = ref 0 in
  let classes = Buffer.create 64 in
  let num v =
    let v = find v in
    if vnum.(v) < 0 then begin
      vnum.(v) <- !nout;
      incr nout;
      Buffer.add_char classes (match Dynarray.get vcls v with Gpr -> 'g' | Xmm -> 'x')
    end;
    vnum.(v)
  in
  let body = Buffer.create 4096 in
  let pf fmt = Printf.bprintf body fmt in
  let ninst = ref 0 in
  let opnd_s o =
    Printf.sprintf "%s:%%%d:%s:%s%s" (if o.use then "u" else "d") (num o.v) (reg_str o.con) (if o.early then "e" else "l")
      (if o.mem then ":mem" else "")
  in
  let blist l = String.concat "" (List.map (fun b -> Printf.sprintf " b%d" bnum.(b)) l) in
  let live_params s = List.filter (fun p -> not removed.(p)) (Array.to_list bparams.(s)) in
  let args_s s a =
    let l = ref [] in
    Array.iteri (fun k v -> if not removed.(bparams.(s).(k)) then l := Printf.sprintf "%%%d" (num v) :: !l) a;
    "(" ^ String.concat " " (List.rev !l) ^ ")"
  in
  List.iter
    (fun b ->
      let freq = if cold.(b) then "0.001" else Printf.sprintf "%g" (10. ** float_of_int (min depth.(b) 9)) in
      pf "block %d preds%s succs%s params%s freq %s depth %d%s%s\n" bnum.(b) (blist preds.(b))
        (blist succs_ordered.(b))
        (String.concat "" (List.map (fun p -> Printf.sprintf " %%%d" (num p)) (live_params b)))
        freq depth.(b)
        (if cold.(b) then " cold" else "")
        (if b >= nb0 then " split" else "");
      if live_params b <> [] then
        List.iter
          (fun (p, a) -> pf "pinned_args b%d from b%d %s\n" bnum.(b) bnum.(p) (args_s b a))
          (List.sort compare pinned_args.(b));
      List.iter
        (fun i ->
          if not (i.undef && not used.(find i.opnds.(0).v)) then begin
            pf "  %d %s" !ninst i.op;
            List.iter (fun a -> pf " %s" a) i.attrs;
            Array.iter (fun o -> pf " %s" (opnd_s o)) i.opnds;
            if i.clob <> [] then pf " clobbers %s" (String.concat " " (List.map pname i.clob));
            pf "\n";
            incr ninst
          end)
        bcode.(b);
      pf "  %d " !ninst;
      incr ninst;
      (match bterm.(b) with
      | Tjump (s, a) -> pf "jump b%d %s" bnum.(s) (args_s s a)
      | Tbr (t, e) -> pf "br b%d b%d rflags" bnum.(t) bnum.(e)
      | Tret ops -> pf "ret%s" (String.concat "" (Array.to_list (Array.map (fun o -> " " ^ opnd_s o) ops)))
      | Tjmpr (o, l) -> pf "jmpr %s targets%s pinned" (opnd_s o) (blist l)
      | Tasmgoto (x, a, l) -> pf "asmgoto b%d %s labels%s pinned" bnum.(x) (args_s x a) (blist l)
      | Thalt -> pf "halt");
      pf "\n")
    order;
  (* the hash covers every line between "func" and "end", newlines included *)
  let body = Printf.sprintf "class %s\n%s" (Buffer.contents classes) (Buffer.contents body) in
  let hash = fnv fnv_basis body in
  Printf.sprintf "func %s vregs %d blocks %d insts %d hash %016Lx\n%send\n" f.name !nout (List.length order) !ninst hash
    body

(* ---------- the file ---------- *)

let pending = Buffer.create 4096

let add_func (f : func) =
  if enabled then
    match func_text f with
    | s -> Buffer.add_string pending s
    | exception Skip why -> Printf.bprintf pending "# skipped %s: %s\n" f.name why

let header =
  "fasra 1\n\
   regs gpr rax rcx rdx rsi rdi r8 r9 r10 r11 rbx r12 r13 r14 r15\n\
   regs xmm xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15\n\
   callee_saved rbx r12 r13 r14 r15\n\
   reserved rsp rbp\n"

(* the module's functions go to FAS_RA_EXPORT: a file (appended to), or a
   directory, where the file is named after the module's first source file *)
let flush (m : Fir.modul) =
  match target with
  | None -> ()
  | Some t ->
      let path =
        if Sys.file_exists t && Sys.is_directory t then begin
          let name = ref "module" in
          (try
             for i = 1 to Fir.Vec.length m.locs - 1 do
               let file, _, _ = Fir.Vec.get m.locs i in
               if file <> "" then begin
                 name := Filename.remove_extension (Filename.basename file);
                 raise Exit
               end
             done
           with Exit -> ());
          Filename.concat t (!name ^ ".ra")
        end
        else t
      in
      let fresh = not (Sys.file_exists path) || (Unix.stat path).st_size = 0 in
      let oc = open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o644 path in
      if fresh then output_string oc header;
      output_string oc "module\n";
      Buffer.output_buffer oc pending;
      close_out oc;
      Buffer.clear pending
