(* The compile budgets (Limits) checked on a lowered Fir module: lowered
   nodes, static data bytes, stack scratch bytes. They replace the old Ir's
   checks (roadmap v0.6) with the same messages; each error names the first
   function or global over the budget. A node is a function (also a
   declaration), a block, an instruction or a terminator (constants are values, not nodes). *)

open Fir

let negative name limits =
  Error
    ( None,
      Printf.sprintf "budget %s must not be negative (profile %s)" name
        (Limits.budget_profile_name limits) )

(* Sums [cost] over [items] in order; the first item that takes the total over
   [budget] (or whose cost cannot be counted, None) is the error *)
let cumulative ~budget items cost error =
  let rec go total = function
    | [] -> Ok ()
    | x :: rest -> (
        match cost x with
        | Some n when n <= budget - total -> go (total + n) rest
        | _ -> Error (error x))
  in
  go 0 items

let func_nodes f =
  1 + Vec.fold (fun acc (b : block_rec) -> acc + 2 + Vec.fold (fun n i -> if (inst f i).op = Nop then n else n + 1) 0 b.insts) 0 f.blocks

let count_lowered_nodes m = Vec.fold (fun acc f -> acc + func_nodes f) 0 m.funcs

let check_lowered_nodes ~limits m =
  let budget = limits.Limits.max_ir_nodes in
  if budget < 0 then negative "max_ir_nodes" limits
  else
    cumulative ~budget (Vec.to_list m.funcs)
      (fun f -> Some (func_nodes f))
      (fun f ->
        ( Some f.name,
          Printf.sprintf
            "cumulative lowered IR nodes exceed budget max_ir_nodes of %d (profile %s) at function `%s`"
            budget (Limits.budget_profile_name limits) f.name ))

(* bytes a defined global puts in the object file: zero-filled storage costs
   nothing, as in the old Ir *)
let check_static_data_bytes ~limits m =
  let budget = limits.Limits.max_static_data_bytes in
  if budget < 0 then negative "max_static_data_bytes" limits
  else
    cumulative ~budget
      (List.filter (fun g -> g.gdefined) (Vec.to_list m.globals))
      (fun g -> Some (match g.init with None -> 0 | Some _ -> g.size))
      (fun g ->
        ( Some g.gname,
          Printf.sprintf
            "declared static data bytes exceed budget max_static_data_bytes of %d (profile %s) at global `%s`"
            budget (Limits.budget_profile_name limits) g.gname ))

(* the constant-size allocas of each function *)
let check_stack_scratch_bytes ~limits m =
  let budget = limits.Limits.max_stack_scratch_bytes in
  if budget < 0 then negative "max_stack_scratch_bytes" limits
  else
    let scratch f =
      Vec.fold
        (fun acc (b : block_rec) ->
          Vec.fold
            (fun acc i ->
              match (acc, (inst f i).op) with
              | Some total, Alloca (n, _) -> (
                  match int_value f n with
                  | Some k when Int64.compare k 0L >= 0 && Int64.compare k (Int64.of_int (max_int - total)) <= 0 ->
                      Some (total + Int64.to_int k)
                  | Some _ -> None
                  | None -> acc)
              | _ -> acc)
            acc b.insts)
        (Some 0) f.blocks
    in
    cumulative ~budget
      (List.filter (fun f -> f.defined) (Vec.to_list m.funcs))
      scratch
      (fun f ->
        ( Some f.name,
          Printf.sprintf
            "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes of %d (profile %s) at function `%s`"
            budget (Limits.budget_profile_name limits) f.name ))

(* `--emit-ir`, and `fas check`'s render test: the module's Fir text, within
   max_rendered_ir_bytes *)
let render_text ~limits m =
  let budget = limits.Limits.max_rendered_ir_bytes in
  if budget < 0 then
    Error
      (Printf.sprintf "budget max_rendered_ir_bytes must not be negative (profile %s)"
         (Limits.budget_profile_name limits))
  else
    let text = Fir_print.module_to_string m in
    if String.length text > budget then
      Error
        (Printf.sprintf "rendered debug IR bytes exceed budget max_rendered_ir_bytes of %d (profile %s)" budget
           (Limits.budget_profile_name limits))
    else Ok text
