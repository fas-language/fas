(* Fir_asm: GNU inline assembly for x86-64, shared by the verifier, the
   LLVM path and the native backend.

   Fir keeps an asm statement the way GCC's C front end sees it: the
   template text and one GCC constraint string per operand, so a C front end
   passes them through unchanged. This module gives them meaning:

   - [constraint] parses one operand's constraint (its first alternative:
     with "r,m" every operand uses alternative 0, a choice GCC may make);
   - [template] splits the template into text and operand references;
   - [reg_canon] names registers the way clobbers and "{reg}" may spell them.

   Operands are numbered as in GCC: outputs first, then inputs. A "+"
   output is one operand that is both read and written. An operand is
   "indirect" (its Fir value is an address) when its constraint allows
   memory and nothing else: "m", "=m", "+m", "o", "V", "<", ">". *)

type dir = In | Out | Inout

type con = {
  dir : dir;
  early : bool;  (** "&": written before every input is read *)
  regs : string list;  (** the registers it may use (canonical names), in no order *)
  mem : bool;  (** a memory operand allowed *)
  imm : (int64 -> bool) option;  (** integer constants allowed: which ones *)
  sym : bool;  (** a symbol's address (plus an offset) allowed as an immediate *)
  tied : int option;  (** input that must get output N's location ("0", "[name]") *)
  flag : string option;  (** output "=@cc<cond>": the condition code, e.g. "z", "nc", "be" *)
}

exception Bad of string

let fail fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

let gprs = [ "rax"; "rbx"; "rcx"; "rdx"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"; "r12"; "r13"; "r14"; "r15" ]
let xmms = List.init 16 (fun i -> "xmm" ^ string_of_int i)
let is_xmm r = String.length r > 3 && String.sub r 0 3 = "xmm"

(* Canonical name of a register as clobbers and "{...}" constraints may
   write it: any width of a GPR names the 64-bit register ("eax", "ax",
   "al", "ah", "r8d" -> "rax", "r8"), "ymmN"/"zmmN" name xmmN. Also "rsp"
   and "rbp", which callers refuse. None for anything else. *)
let reg_canon name =
  let n = String.lowercase_ascii name in
  let n = if String.length n > 0 && n.[0] = '%' then String.sub n 1 (String.length n - 1) else n in
  let legacy =
    [ ("a", "rax"); ("b", "rbx"); ("c", "rcx"); ("d", "rdx") ]
  in
  let idx = [ ("si", "rsi"); ("di", "rdi"); ("sp", "rsp"); ("bp", "rbp") ] in
  let numbered s =
    match int_of_string_opt s with Some k when k >= 0 && k <= 15 -> Some k | _ -> None
  in
  let len = String.length n in
  let tail k = String.sub n k (len - k) in
  match n with
  | _ when List.mem n gprs || n = "rsp" || n = "rbp" -> Some n
  | _ when len = 3 && n.[0] = 'e' && (List.mem_assoc (String.sub n 1 1) legacy && n.[2] = 'x') ->
      List.assoc_opt (String.sub n 1 1) legacy
  | _ when len = 3 && n.[0] = 'e' -> List.assoc_opt (tail 1) idx
  | _ when len = 2 && n.[1] = 'x' -> List.assoc_opt (String.sub n 0 1) legacy
  | _ when len = 2 && (n.[1] = 'l' || n.[1] = 'h') -> List.assoc_opt (String.sub n 0 1) legacy
  | _ when len = 2 -> List.assoc_opt n idx
  | _ when len = 3 && n.[2] = 'l' -> List.assoc_opt (String.sub n 0 2) idx  (* sil dil spl bpl *)
  | _ when len >= 2 && n.[0] = 'r' -> (
      (* r8 .. r15 with a d / w / b / l suffix *)
      let body, _ =
        match n.[len - 1] with
        | 'd' | 'w' | 'b' | 'l' -> (String.sub n 1 (len - 2), ())
        | _ -> (tail 1, ())
      in
      match numbered body with Some k when k >= 8 -> Some ("r" ^ string_of_int k) | _ -> None)
  | _ when len > 3 && (String.sub n 0 3 = "xmm" || String.sub n 0 3 = "ymm" || String.sub n 0 3 = "zmm") -> (
      match numbered (tail 3) with Some k -> Some ("xmm" ^ string_of_int k) | None -> None)
  | _ -> None

(* What a clobber means *)
type clobber = Creg of string | Cmemory | Cflags | Cignored

let clobber c =
  match String.lowercase_ascii c with
  | "memory" -> Cmemory
  | "cc" | "flags" | "eflags" | "rflags" | "dirflag" | "fpsr" | "fpcr" -> Cflags
  | n when String.length n >= 2 && (String.sub n 0 2 = "st" || String.sub n 0 2 = "mm") ->
      (* x87 and MMX registers: no value lives in them between instructions *)
      Cignored
  | n when String.length n = 2 && n.[0] = 'k' -> Cignored  (* AVX-512 masks: never used *)
  | _ -> (
      match reg_canon c with
      | Some r -> Creg r
      | None -> fail "unknown register %S in an asm clobber" c)

let in_range lo hi k = Int64.compare k lo >= 0 && Int64.compare k hi <= 0

(* [names]: the operands' names, outputs then inputs ("" = none);
   [nouts]: how many are outputs; [output]: is this one an output? *)
let constr ~names ~nouts ~output s =
  let n = String.length s in
  let dir = ref In and early = ref false in
  let regs = ref [] and mem = ref false and imm = ref None and sym = ref false in
  let tied = ref None and flag = ref None in
  let add l = regs := !regs @ List.filter (fun r -> not (List.mem r !regs)) l in
  let any_imm () = imm := Some (fun _ -> true) in
  let range lo hi =
    let old = !imm in
    imm :=
      Some (fun k -> in_range lo hi k || match old with Some f -> f k | None -> false)
  in
  let i = ref 0 in
  let stop = ref false in
  while !i < n && not !stop do
    let c = s.[!i] in
    incr i;
    match c with
    | '=' -> if !i = 1 then dir := Out else fail "'=' must come first in asm constraint %S" s
    | '+' -> if !i = 1 then dir := Inout else fail "'+' must come first in asm constraint %S" s
    | '&' -> early := true
    | '%' | '?' | '!' | '*' | ' ' | '\t' -> ()
    | ',' -> stop := true (* first alternative only *)
    | '#' -> stop := true
    | 'r' | 'g' | 'X' | 'p' | 'l' ->
        add gprs;
        if c = 'g' || c = 'X' then begin
          mem := true;
          any_imm ();
          sym := true
        end
    | 'q' | 'Q' | 'R' | 'U' ->
        add
          (match c with
          | 'q' | 'Q' -> [ "rax"; "rbx"; "rcx"; "rdx" ]
          | 'R' -> [ "rax"; "rbx"; "rcx"; "rdx"; "rsi"; "rdi" ]
          | _ -> [ "rax"; "rcx"; "rdx"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11" ])
    | 'a' -> add [ "rax" ]
    | 'b' -> add [ "rbx" ]
    | 'c' -> add [ "rcx" ]
    | 'd' -> add [ "rdx" ]
    | 'S' -> add [ "rsi" ]
    | 'D' -> add [ "rdi" ]
    | 'A' -> add [ "rax" ] (* a value of at most 64 bits: rax (GCC: rax or rdx) *)
    | 'x' | 'v' -> add xmms
    | 'Y' ->
        if !i < n && s.[!i] = 'z' then begin
          incr i;
          add [ "xmm0" ]
        end
        else fail "asm constraint %S: only Yz of the Y constraints is supported" s
    | 'm' | 'o' | 'V' | '<' | '>' -> mem := true
    | 'i' ->
        any_imm ();
        sym := true
    | 'n' -> any_imm ()
    | 's' -> sym := true
    | 'I' -> range 0L 31L
    | 'J' -> range 0L 63L
    | 'K' -> range (-128L) 127L
    | 'L' ->
        let old = !imm in
        imm := Some (fun k -> k = 0xffL || k = 0xffffL || k = 0xffffffffL || match old with Some f -> f k | None -> false)
    | 'M' -> range 0L 3L
    | 'N' -> range 0L 255L
    | 'O' -> range 0L 127L
    | 'e' -> range (-0x8000_0000L) 0x7fff_ffffL
    | 'Z' -> range 0L 0xffff_ffffL
    | '0' .. '9' ->
        let st = !i - 1 in
        while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
        (* an out-of-range number (too many digits) is no operand either *)
        let k = Option.value (int_of_string_opt (String.sub s st (!i - st))) ~default:max_int in
        if output then fail "asm output constraint %S cannot be a matching constraint" s;
        if k >= nouts then fail "asm matching constraint %S does not name an output" s;
        tied := Some k
    | '[' ->
        let st = !i in
        (match String.index_from_opt s st ']' with
        | None -> fail "unterminated [name] in asm constraint %S" s
        | Some e ->
            let nm = String.sub s st (e - st) in
            i := e + 1;
            if output then fail "asm output constraint %S cannot be a matching constraint" s;
            let rec find k = if k >= nouts then fail "asm constraint %S: no output named %s" s nm
              else if names.(k) = nm then k else find (k + 1) in
            tied := Some (find 0))
    | '{' -> (
        match String.index_from_opt s !i '}' with
        | None -> fail "unterminated {register} in asm constraint %S" s
        | Some e -> (
            let r = String.sub s !i (e - !i) in
            i := e + 1;
            match reg_canon r with
            | Some ("rsp" | "rbp") -> fail "asm constraint %S: %s cannot be an operand" s r
            | Some r -> add [ r ]
            | None -> fail "asm constraint %S: unknown register %s" s r))
    | '@' ->
        if !i + 2 <= n && String.sub s !i 2 = "cc" then begin
          let cond = String.sub s (!i + 2) (n - !i - 2) in
          let ok =
            [ "a"; "ae"; "b"; "be"; "c"; "e"; "g"; "ge"; "l"; "le"; "na"; "nae"; "nb"; "nbe"; "nc";
              "ne"; "ng"; "nge"; "nl"; "nle"; "no"; "np"; "ns"; "nz"; "o"; "p"; "s"; "z" ]
          in
          if not (List.mem cond ok) then fail "asm constraint %S: unknown condition @cc%s" s cond;
          if not output then fail "asm flag constraint %S must be an output" s;
          flag := Some cond;
          stop := true
        end
        else fail "asm constraint %S: unknown '@' constraint" s
    | 't' | 'u' | 'f' -> fail "asm constraint %S: x87 register operands are not supported" s
    | 'y' -> fail "asm constraint %S: MMX register operands are not supported" s
    | 'k' -> fail "asm constraint %S: AVX-512 mask register operands are not supported" s
    | c -> fail "asm constraint %S: unknown constraint letter '%c'" s c
  done;
  if output && !dir = In then fail "asm output constraint %S must start with '=' or '+'" s;
  if (not output) && !dir <> In then fail "asm input constraint %S cannot start with '=' or '+'" s;
  if !regs = [] && (not !mem) && !imm = None && (not !sym) && !tied = None && !flag = None then
    fail "asm constraint %S allows no operand" s;
  if output && !regs = [] && (not !mem) && !flag = None then
    fail "asm output constraint %S needs a register or memory" s;
  { dir = !dir; early = !early; regs = !regs; mem = !mem; imm = !imm; sym = !sym; tied = !tied; flag = !flag }

(* The Fir operand is the address of the bytes *)
let indirect c = c.mem && c.regs = [] && c.imm = None && (not c.sym) && c.tied = None && c.flag = None

(* An output that takes a Fir value: its address or its old value *)
let output_takes_arg c = indirect c || c.dir = Inout

(* An output that is a result of the Fir instruction *)
let output_is_result c = not (indirect c)

(* ---------- templates ---------- *)

type piece =
  | Text of string
  | Operand of char option * int  (** %N / %[name] with an optional modifier letter: operand N *)
  | Unique  (** %= : a number unique to this asm statement *)

(* Splits [tmpl], keeping the dialect alternative [intel] chooses from
   "{att|intel}" groups. [names]: the operands' names. *)
let template ~names ~intel tmpl =
  let n = String.length tmpl in
  let out = ref [] and buf = Buffer.create 64 in
  let flush () =
    if Buffer.length buf > 0 then begin
      out := Text (Buffer.contents buf) :: !out;
      Buffer.clear buf
    end
  in
  let nops = Array.length names in
  (* dialect state: -1 outside a group, else the alternative being read *)
  let alt = ref (-1) in
  let keep () = !alt < 0 || !alt = (if intel then 1 else 0) in
  let i = ref 0 in
  while !i < n do
    let c = tmpl.[!i] in
    incr i;
    match c with
    | '{' when !alt < 0 -> alt := 0
    | '|' when !alt >= 0 -> incr alt
    | '}' when !alt >= 0 -> alt := -1
    | '{' -> fail "nested '{' in asm template"
    | '%' ->
        if !i >= n then fail "asm template ends with '%%'";
        let c = tmpl.[!i] in
        incr i;
        let operand modifier =
          if !i < n && tmpl.[!i] = '[' then begin
            match String.index_from_opt tmpl !i ']' with
            | None -> fail "unterminated %%[name] in asm template"
            | Some e ->
                let nm = String.sub tmpl (!i + 1) (e - !i - 1) in
                i := e + 1;
                let rec find k =
                  if k >= nops then fail "asm template names %%[%s], which no operand has" nm
                  else if names.(k) = nm then k
                  else find (k + 1)
                in
                Operand (modifier, find 0)
          end
          else begin
            let st = !i in
            while !i < n && tmpl.[!i] >= '0' && tmpl.[!i] <= '9' do incr i done;
            if !i = st then
              fail "asm template: '%%%s' is not followed by an operand"
                (match modifier with Some m -> String.make 1 m | None -> "");
            let digits = String.sub tmpl st (!i - st) in
            let k = Option.value (int_of_string_opt digits) ~default:max_int in
            if k >= nops then fail "asm template refers to operand %s of %d" digits nops;
            Operand (modifier, k)
          end
        in
        let p =
          match c with
          | '%' -> `Text "%"
          | '=' -> `Piece Unique
          | '{' -> `Text "{"
          | '|' -> `Text "|"
          | '}' -> `Text "}"
          | '0' .. '9' | '[' ->
              decr i;
              `Piece (operand None)
          | ('a' .. 'z' | 'A' .. 'Z') as m -> `Piece (operand (Some m))
          | c -> fail "asm template: unknown '%%%c'" c
        in
        if keep () then (
          match p with
          | `Text t -> Buffer.add_string buf t
          | `Piece pc ->
              flush ();
              out := pc :: !out)
    | c -> if keep () then Buffer.add_char buf c
  done;
  if !alt >= 0 then fail "unterminated '{' in asm template";
  flush ();
  List.rev !out

(* ---------- a whole statement ---------- *)

type stmt = {
  outs : con array;
  ins : con array;
  pieces : piece list;
  clob : clobber list;
}

(* Parses and checks an asm statement's parts (types are the caller's) *)
let parse ~labels ~intel ~tmpl ~(outs : (string * string) array) ~(ins : (string * string) array) ~clobbers =
  let names = Array.append (Array.map fst outs) (Array.map fst ins) in
  let nops = Array.length names in
  let nouts = Array.length outs in
  let oc = Array.map (fun (_, c) -> constr ~names ~nouts ~output:true c) outs in
  let ic = Array.map (fun (_, c) -> constr ~names ~nouts ~output:false c) ins in
  Array.iter
    (fun c ->
      match c.tied with
      | Some k when indirect oc.(k) -> fail "an asm input is tied to output %d, a memory operand" k
      | Some k when oc.(k).flag <> None -> fail "an asm input is tied to output %d, a flag output" k
      | _ -> ())
    ic;
  let clob = List.map clobber (Array.to_list clobbers) in
  List.iter
    (function
      | Creg ("rsp" | "rbp" as r) -> fail "asm clobbers %s, which the frame needs" r
      | _ -> ())
    clob;
  (* an explicit register operand may not be clobbered *)
  Array.iter
    (fun c ->
      match c.regs with
      | [ r ] when List.mem (Creg r) clob -> fail "asm operand register %s is also clobbered" r
      | _ -> ())
    (Array.append oc ic);
  (* asm goto's [labels] come after the operands, named by %lN only *)
  let pieces = template ~names:(Array.append names (Array.make labels "")) ~intel tmpl in
  List.iter
    (function
      | Operand (Some 'l', k) when k < nops -> fail "asm template: %%l%d names an operand, not a label" k
      | Operand (m, k) when k >= nops && m <> Some 'l' -> fail "asm template: label %d needs %%l%d" k k
      | _ -> ())
    pieces;
  { outs = oc; ins = ic; pieces; clob }
