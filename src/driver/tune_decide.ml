type side = { median : float; mad : float }

type comparison = {
  ratios : float array;
  geomean : float;
  band : float;
  passes : bool;
  keep : bool;
}

let median values =
  let sorted = Array.copy values in
  Array.sort Float.compare sorted;
  let n = Array.length sorted in
  if n = 0 then nan
  else if n mod 2 = 1 then sorted.(n / 2)
  else (sorted.((n / 2) - 1) +. sorted.(n / 2)) /. 2.

let summarize samples =
  let m = median samples in
  { median = m; mad = median (Array.map (fun x -> Float.abs (x -. m)) samples) }

let two_sided_tail z = Float.erfc (z /. Float.sqrt 2.)

let quantile tail =
  let rec search low high steps =
    let mid = (low +. high) /. 2. in
    if steps = 0 then mid
    else if two_sided_tail mid > tail then search mid high (steps - 1)
    else search low mid (steps - 1)
  in
  search 0. 40. 200

let z_for tests = quantile (0.0027 /. float_of_int (max 1 tests))

let standard_error ~rounds side =
  1.253 *. 1.4826 *. side.mad /. side.median /. Float.sqrt (float_of_int rounds)

let placement_band ~z ~rounds a b =
  let sa = standard_error ~rounds a and sb = standard_error ~rounds b in
  Float.max 0.01 (z *. Float.sqrt ((sa *. sa) +. (sb *. sb)))

let geomean values =
  let n = Array.length values in
  if n = 0 then nan
  else
    Float.exp
      (Array.fold_left (fun acc x -> acc +. Float.log x) 0. values /. float_of_int n)

let compare ~tests ~rounds ~baseline ~candidate =
  let z = z_for tests in
  let ratios = Array.map2 (fun b c -> b.median /. c.median) baseline candidate in
  let band =
    Array.fold_left Float.max 0.
      (Array.map2 (fun b c -> placement_band ~z ~rounds b c) baseline candidate)
  in
  let geomean = geomean ratios in
  let passes = Array.for_all (fun r -> r >= 1. -. band) ratios in
  { ratios; geomean; band; passes; keep = passes && geomean >= 1. +. band }

type selection = Selected of int * comparison | Rejected of (int * comparison) option

let select ~rounds ~baseline ~candidates =
  let placements = Array.length baseline in
  let tests = Array.length candidates * placements in
  let compared =
    Array.mapi
      (fun index candidate -> (index, compare ~tests ~rounds ~baseline ~candidate))
      candidates
    |> Array.to_list
  in
  let better (_, a) (_, b) = Float.compare b.geomean a.geomean in
  let passing =
    List.filter (fun (_, c) -> c.passes) compared |> List.stable_sort better
  in
  match passing with
  | (index, best) :: _ when best.keep -> Selected (index, best)
  | _ -> (
      match List.stable_sort better compared with
      | best :: _ -> Rejected (Some best)
      | [] -> Rejected None)

let kept ?held selection confirmation =
  match selection with
  | Selected _ ->
      confirmation.keep && Option.fold ~none:true ~some:(fun held -> held.passes) held
  | Rejected _ -> false

let confirm ~rounds ~baseline ~candidate =
  compare ~tests:(Array.length baseline) ~rounds ~baseline ~candidate

let best_layout ~rounds ~layouts =
  let count = Array.length layouts in
  if count < 2 then None
  else
    let scores =
      Array.map (fun offsets -> geomean (Array.map (fun s -> s.median) offsets)) layouts
    in
    let best = ref 0 in
    Array.iteri (fun i score -> if score < scores.(!best) then best := i) scores;
    let tests = (count - 1) * Array.length layouts.(0) in
    let beats other =
      other = !best
      ||
      let result =
        compare ~tests ~rounds ~baseline:layouts.(other) ~candidate:layouts.(!best)
      in
      result.keep
    in
    if List.for_all beats (List.init count Fun.id) then Some !best else None
