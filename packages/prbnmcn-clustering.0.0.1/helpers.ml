let rec take_n n list acc =
  if n = 0 then (List.rev acc, list)
  else
    match list with
    | [] -> Stdlib.invalid_arg "take_n"
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

(* [forgy_init] picks [k] initial centroids uniformly at random. *)
let forgy_init k elements rng_state =
  let elements = Array.to_list elements in
  let (selected, _rejected) = sample_without_replacement k elements rng_state in
  Array.of_list selected

let array_fsum (arr : float array) =
  let acc = ref 0.0 in
  for i = 0 to Array.length arr - 1 do
    acc := !acc +. Array.unsafe_get arr i
  done ;
  !acc
