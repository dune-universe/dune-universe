(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

include BatArray

(* get one bootstrap sample of size 'nb_samples' using
   sampling with replacement *)
let bootstrap_sample (nb_samples: int) (a: 'a array): 'a list =
  let res = ref [] in
  let n = Array.length a in
  assert(nb_samples <= n);
  for _ = 1 to nb_samples do
    let i = Random.int n in
    res := a.(i) :: !res
  done;
  !res

(* same but returning an array and using a seedable RNG *)
let bootstrap_sample_a rng nb_samples a =
  let n = Array.length a in
  assert(nb_samples <= n);
  init nb_samples (fun _ ->
      let rand = Random.State.int rng n in
      a.(rand)
    )
