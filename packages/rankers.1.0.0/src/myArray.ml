(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

include BatArray

exception End_of_loop of int

let fold_while (p: 'acc -> 'a -> bool) (f: 'acc -> 'a -> 'acc)
    (init: 'acc) (arr: 'a array): 'acc * int =
  let n = length arr in
  if n = 0 then (init, 0)
  else
    let acc = ref init in
    try
      for i = 0 to n - 1 do
        let x = unsafe_get arr i in
        if p !acc x then
          acc := f !acc x
        else
          raise (End_of_loop i)
      done;
      (* folded over the whole array *)
      (!acc, n)
    with End_of_loop i ->
      (* stopped early *)
      (!acc, i)

(* get one bootstrap sample of size 'nb_samples' using
   sampling with replacement *)
let bootstrap_sample rng nb_samples a =
  let n = Array.length a in
  assert(nb_samples <= n);
  init nb_samples (fun _ ->
      let rand = Random.State.int rng n in
      a.(rand)
    )
