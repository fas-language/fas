(* Control-flow analyses on Fir functions, in flat arrays:
   - predecessors (CSR form),
   - reverse postorder,
   - dominator tree (Cooper, Harvey & Kennedy, "A Simple, Fast Dominance
     Algorithm"), with pre/post numbers for O(1) dominance queries.

   Blocks unreachable from the entry have rpo_index = -1 and idom = -1. *)

open Fir

type cfg = {
  nblocks : int;
  succ_start : int array;  (** successors of b: succs.(succ_start.(b)) .. succs.(succ_start.(b+1) - 1) *)
  succs : int array;
  pred_start : int array;
  preds : int array;
}

let cfg f =
  let n = num_blocks f in
  let succ_lists = Array.init n (fun b -> successors (block f b).term) in
  let succ_start = Array.make (n + 1) 0 in
  for b = 0 to n - 1 do succ_start.(b + 1) <- succ_start.(b) + List.length succ_lists.(b) done;
  let succs = Array.make succ_start.(n) 0 in
  let pred_count = Array.make n 0 in
  for b = 0 to n - 1 do
    List.iteri
      (fun k s ->
        succs.(succ_start.(b) + k) <- s;
        pred_count.(s) <- pred_count.(s) + 1)
      succ_lists.(b)
  done;
  let pred_start = Array.make (n + 1) 0 in
  for b = 0 to n - 1 do pred_start.(b + 1) <- pred_start.(b) + pred_count.(b) done;
  let preds = Array.make pred_start.(n) 0 in
  let fill = Array.copy pred_start in
  for b = 0 to n - 1 do
    for k = succ_start.(b) to succ_start.(b + 1) - 1 do
      let s = succs.(k) in
      preds.(fill.(s)) <- b;
      fill.(s) <- fill.(s) + 1
    done
  done;
  { nblocks = n; succ_start; succs; pred_start; preds }

let iter_succs c b g =
  for k = c.succ_start.(b) to c.succ_start.(b + 1) - 1 do g c.succs.(k) done

let iter_preds c b g =
  for k = c.pred_start.(b) to c.pred_start.(b + 1) - 1 do g c.preds.(k) done

type dom = {
  rpo : int array;  (** reachable blocks in reverse postorder; rpo.(0) = entry *)
  rpo_index : int array;  (** block -> position in rpo, -1 if unreachable *)
  idom : int array;  (** immediate dominator; entry -> entry; unreachable -> -1 *)
  pre : int array;  (** dominator-tree preorder number *)
  post : int array;  (** dominator-tree postorder number *)
}

(* Iterative DFS computing postorder without recursion (deep CFGs) *)
let reverse_postorder c =
  let n = c.nblocks in
  let visited = Array.make n false in
  let order = Array.make n 0 in
  let count = ref 0 in
  if n > 0 then begin
    let stack_b = Array.make n 0 and stack_k = Array.make n 0 in
    let sp = ref 0 in
    visited.(0) <- true;
    stack_b.(0) <- 0;
    stack_k.(0) <- c.succ_start.(0);
    sp := 1;
    while !sp > 0 do
      let top = !sp - 1 in
      let b = stack_b.(top) in
      let k = stack_k.(top) in
      if k < c.succ_start.(b + 1) then begin
        stack_k.(top) <- k + 1;
        let s = c.succs.(k) in
        if not visited.(s) then begin
          visited.(s) <- true;
          stack_b.(!sp) <- s;
          stack_k.(!sp) <- c.succ_start.(s);
          incr sp
        end
      end
      else begin
        order.(!count) <- b;
        incr count;
        decr sp
      end
    done
  end;
  let m = !count in
  Array.init m (fun i -> order.(m - 1 - i))

let dominators c =
  let n = c.nblocks in
  let rpo = reverse_postorder c in
  let rpo_index = Array.make n (-1) in
  Array.iteri (fun i b -> rpo_index.(b) <- i) rpo;
  let idom = Array.make n (-1) in
  if n > 0 then idom.(0) <- 0;
  let intersect a b =
    let a = ref a and b = ref b in
    while !a <> !b do
      while rpo_index.(!a) > rpo_index.(!b) do a := idom.(!a) done;
      while rpo_index.(!b) > rpo_index.(!a) do b := idom.(!b) done
    done;
    !a
  in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 1 to Array.length rpo - 1 do
      let b = rpo.(i) in
      let nd = ref (-1) in
      iter_preds c b (fun p ->
          if idom.(p) >= 0 then nd := if !nd < 0 then p else intersect p !nd);
      if !nd <> idom.(b) then begin
        idom.(b) <- !nd;
        changed := true
      end
    done
  done;
  (* dominator tree children in CSR, then pre/post numbering *)
  let child_count = Array.make n 0 in
  Array.iter (fun b -> if b <> 0 then child_count.(idom.(b)) <- child_count.(idom.(b)) + 1) rpo;
  let cstart = Array.make (n + 1) 0 in
  for b = 0 to n - 1 do cstart.(b + 1) <- cstart.(b) + child_count.(b) done;
  let children = Array.make (max 1 cstart.(n)) 0 in
  let fill = Array.copy cstart in
  Array.iter
    (fun b ->
      if b <> 0 then begin
        let d = idom.(b) in
        children.(fill.(d)) <- b;
        fill.(d) <- fill.(d) + 1
      end)
    rpo;
  let pre = Array.make n (-1) and post = Array.make n (-1) in
  if Array.length rpo > 0 then begin
    let stack_b = Array.make n 0 and stack_k = Array.make n 0 in
    let sp = ref 1 and pc = ref 0 and qc = ref 0 in
    stack_b.(0) <- 0;
    stack_k.(0) <- cstart.(0);
    pre.(0) <- 0;
    pc := 1;
    while !sp > 0 do
      let top = !sp - 1 in
      let b = stack_b.(top) in
      let k = stack_k.(top) in
      if k < cstart.(b + 1) then begin
        stack_k.(top) <- k + 1;
        let ch = children.(k) in
        pre.(ch) <- !pc;
        incr pc;
        stack_b.(!sp) <- ch;
        stack_k.(!sp) <- cstart.(ch);
        incr sp
      end
      else begin
        post.(b) <- !qc;
        incr qc;
        decr sp
      end
    done
  end;
  { rpo; rpo_index; idom; pre; post }

let reachable d b = d.rpo_index.(b) >= 0

(* Does block a dominate block b? (both reachable) *)
let dominates d a b = d.pre.(a) <= d.pre.(b) && d.post.(a) >= d.post.(b)
