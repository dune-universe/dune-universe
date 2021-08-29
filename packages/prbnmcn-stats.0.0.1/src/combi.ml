(* Simple combinatorics. *)

(* -------------------------------------------------------------------------- *)
(* Enumerate all ways of picking n elements from a list (ie all injections
   from [n] to [l]) *)

let rec enumerate_injections n l current_pick acc =
  if n = 0 then List.rev current_pick :: acc
  else enumerate_picks n l current_pick acc

and enumerate_picks n l current_pick acc =
  match l with
  | [] -> acc
  | x :: tl ->
      let extended_pick = x :: current_pick in
      let acc = enumerate_injections (n - 1) tl extended_pick acc in
      enumerate_picks n tl current_pick acc

let enumerate_subsets n l = enumerate_injections n l [] []

(* -------------------------------------------------------------------------- *)
(* Sampling without replacement from a list *)

let rec take_n n list acc =
  if n = 0 then (List.rev acc, list)
  else
    match list with
    | [] -> invalid_arg "take_n"
    | x :: tl -> take_n (n - 1) tl (x :: acc)

let sample_without_replacement n list rng_state =
  let (first_n, rest) = take_n n list [] in
  let reservoir = Array.of_list first_n in
  let reject = ref [] in
  List.iteri
    (fun index elt ->
      let i = n + index in
      let j = Random.State.int rng_state (i + 1) in
      if j < n then (
        reject := reservoir.(j) :: !reject ;
        reservoir.(j) <- elt)
      else reject := elt :: !reject)
    rest ;
  (Array.to_list reservoir, !reject)
