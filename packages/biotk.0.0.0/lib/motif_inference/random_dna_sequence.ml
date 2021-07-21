module Dna_sequence = struct
  let random_base gc =
    match Float.(Random.float 1. > gc, Random.float 1. > 0.5) with
      false, false -> 'c'
    | false, true  -> 'g'
    | true,  false -> 'a'
    | true,  true  -> 't'

  let random n gc = String.init n ~f:(fun _ -> random_base gc)

  let random_base comp =
    if Array.length comp <> 4 then invalid_arg "random_base: expected array of size 4" ;
    match Owl.Stats.categorical_rvs comp with
    | 0 -> 'A'
    | 1 -> 'C'
    | 2 -> 'G'
    | 3 -> 'T'
    | _ -> assert false

  let markov0 n comp = String.init n ~f:(fun _ ->
      random_base comp
    )
end

module Profile_matrix = struct
  let random ?(alpha = 1.) motif_length =
    let alpha = Array.init A.card ~f:(fun _ -> Owl.Stats.uniform_rvs ~a:0. ~b:alpha) in
    Array.init motif_length ~f:(fun _ ->
        Owl.Stats.dirichlet_rvs ~alpha
      )

  let simulate_sequence eps =
    String.init (Array.length eps) ~f:(fun i ->
        Dna_sequence.random_base eps.(i)
      )
end
