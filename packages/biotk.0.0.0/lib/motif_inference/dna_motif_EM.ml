open Core_kernel
open Biotk_croquis
open Misc

module Profile_matrix = Profile_matrix.DNA
let int_of_char_exn x =
  Nucleotide.(to_int (of_char_exn x))

let int_of_comp_char_exn x =
  Nucleotide.(to_int (complement (of_char_exn x)))


module Naive_MEME = struct
  module Simulation = struct
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }

    let make profile ~motif_probability ~n_sequences =
      let n = Profile_matrix.length profile in
      let n_motif_sequences = Float.(to_int (float n_sequences * motif_probability)) in
      let motif_sequences = Array.init n_motif_sequences ~f:(fun _ ->
          Profile_matrix.simulate_sequence profile
        )
      in
      let background_composition = Profile_matrix.composition profile in
      let background_sequences = Array.init (n_sequences - n_motif_sequences) ~f:(fun _ ->
          Dna_sequence.markov0 n background_composition
        )
      in
      { pi = motif_probability ; motif_sequences ; background_sequences }
  end

  type input = string array

  type param = {
    pi : float ;
    motif : float array array ;
    bg : float array ;
  }

  let initial_param ~motif_length = {
    pi = 0.5 ;
    motif =
      Array.init motif_length ~f:(fun _ ->
          Array.create ~len:4 0.25
        ) ;
    bg = Array.create ~len:4 0.25 ;
  }

  let background_probability bg sequence =
    prod (String.length sequence) ~f:(fun j ->
        bg.(int_of_char_exn sequence.[j])
      )

  let motif_probability motif sequence =
    prod (Array.length motif) ~f:(fun i ->
        motif.(i).(int_of_char_exn sequence.[i])
      )

  let gamma param sequences =
    Array.map sequences ~f:(fun seq ->
        let pm = motif_probability param.motif seq in
        let pg = background_probability param.bg seq in
        let x = pm *. param.pi /. (pm *. param.pi +. pg *. (1. -. param.pi)) in
        match Float.classify x with
        | Nan ->
          printf "%f %f %f\n" pm pg x ;
          assert false
        | Normal | Zero | Subnormal -> x
        | _ ->
          printf "%f %f %f\n" pm pg x ;
          assert false
      )

  let update_bg gamma sequences =
    let counts = Array.create ~len:4 0. in
    Array.iteri sequences ~f:(fun i seq ->
        String.iter seq ~f:(fun c ->
            let j = int_of_char_exn c in
            counts.(j) <- counts.(j) +. (1. -. gamma.(i))
          )
      ) ;
    let total = Owl.Stats.sum counts in
    Array.map counts ~f:(fun x -> x /. total)

  let update_motif motif_length gamma sequences =
    let counts = Array.init motif_length ~f:(fun _ ->
        Array.create ~len:4 0.
      )
    in
    Array.iteri sequences ~f:(fun k seq ->
        for i = 0 to String.length seq - 1 do
          let j = int_of_char_exn seq.[i] in
          counts.(i).(j) <- counts.(i).(j) +. gamma.(k)
        done
      ) ;
    let totals = Array.map ~f:Owl.Stats.sum counts in
    Array.map2_exn counts totals ~f:(fun t total ->
        if Float.(total = 0.) then assert false ;
        Array.map t ~f:(fun x -> x /. total))

  let update_pi gamma =
    Owl.Stats.sum gamma /. float (Array.length gamma)

  let infer ~motif_length sequences =
    let rec loop acc param niter =
      printf "iteration %d\n" niter ;
      if niter = 0 then List.rev acc
      else
        let gamma = gamma param sequences in
        let param' = {
          bg = update_bg gamma sequences ;
          motif = update_motif motif_length gamma sequences ;
          pi = update_pi gamma ;
        }
        in
        loop ((gamma, param') :: acc) param' (niter - 1)
    in
    let p0 = initial_param ~motif_length in
    loop [] p0 100

  let demo ?(alpha = 0.1) ?(motif_length = 6) () =
    let motif = Profile_matrix.random ~alpha motif_length in
    let sim = Simulation.make motif ~motif_probability:0.1 ~n_sequences:100 in
    let inference_trace = infer ~motif_length (Array.append sim.motif_sequences sim.background_sequences) in
    let picture =
      let open Croquis.Picture in
      vstack ~align:`centered (
        Profile_matrix.draw motif ::
        List.filter_map inference_trace ~f:(fun (_, p) ->
            Option.map (Profile_matrix.of_array p.motif) ~f:(fun motif ->
                hstack ~align:`centered [
                  text ~size:0.7 ~x:0. ~y:0. (sprintf "pi = %g" p.pi) ;
                  Profile_matrix.draw motif ;
                ]
              )
          )
      )
    in
    Croquis.Layout.(render_pdf (simple picture) "delme.pdf")
end

module Naive_MEME_revcomp = struct
  module Simulation = struct
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }

    let make profile ~motif_probability ~n_sequences =
      let n = Profile_matrix.length profile in
      let n_motif_sequences = Float.(to_int (float n_sequences * motif_probability)) in
      let revcomp_profile = Profile_matrix.reverse_complement profile in
      let motif_sequences = Array.init n_motif_sequences ~f:(fun _ ->
          Profile_matrix.simulate_sequence (if Float.(Owl.Stats.uniform_rvs ~a:0. ~b:1. > 0.5) then profile else revcomp_profile)
        )
      in
      let background_composition = Profile_matrix.composition profile in
      let background_sequences = Array.init (n_sequences - n_motif_sequences) ~f:(fun _ ->
          Dna_sequence.markov0 n background_composition
        )
      in
      { pi = motif_probability ; motif_sequences ; background_sequences }
  end

  type input = string array

  type param = {
    pi : float ;
    motif : float array array ;
    revcomp_motif : float array array ;
    bg : float array ;
  }

  let initial_param ~motif_length =
    let motif = Profile_matrix.flat motif_length in
    let revcomp_motif = Profile_matrix.reverse_complement motif in
    {
      pi = 0.5 ;
      motif = (motif :> float array array) ;
      revcomp_motif = (revcomp_motif :> float array array) ;
      bg = Array.create ~len:4 0.25 ;
    }

  let background_probability bg sequence =
    prod (String.length sequence) ~f:(fun j ->
        bg.(int_of_char_exn sequence.[j])
      )

  let motif_probability motif sequence =
    prod (Array.length motif) ~f:(fun i ->
        motif.(i).(int_of_char_exn sequence.[i])
      )

  let gamma param sequences =
    Array.map sequences ~f:(fun seq ->
        let pm = motif_probability param.motif seq in
        let pmrc = motif_probability param.revcomp_motif seq in
        let pg = background_probability param.bg seq in
        let den = (pm +. pmrc) /. 2. *. param.pi +. pg *. (1. -. param.pi) in
        [| 0.5 *. param.pi *. pm /. den ;
           0.5 *. param.pi *. pmrc /. den ;
           (1. -. param.pi) *. pg /. den |]
      )

  let update_bg gamma sequences =
    let counts = Array.create ~len:4 0. in
    Array.iteri sequences ~f:(fun i seq ->
        String.iter seq ~f:(fun c ->
            let j = int_of_char_exn c in
            counts.(j) <- counts.(j) +. (1. -. gamma.(i).(2))
          )
      ) ;
    let total = Owl.Stats.sum counts in
    Array.map counts ~f:(fun x -> x /. total)

  let update_motif motif_length gamma sequences =
    let counts = Array.init motif_length ~f:(fun _ ->
        Array.create ~len:4 0.
      )
    in
    Array.iteri sequences ~f:(fun k seq ->
        for i = 0 to String.length seq - 1 do
          let j = int_of_char_exn seq.[i] in
          let jrc = int_of_comp_char_exn seq.[i] in
          let irc = motif_length - i - 1 in
          counts.(i).(j) <- counts.(i).(j) +. gamma.(k).(0) ;
          counts.(irc).(jrc) <- counts.(irc).(jrc) +. gamma.(k).(1)
        done
      ) ;
    let totals = Array.map ~f:Owl.Stats.sum counts in
    Array.map2_exn counts totals ~f:(fun t total ->
        if Float.(total = 0.) then assert false ;
        Array.map t ~f:(fun x -> x /. total))

  let update_pi gamma =
    let n = Array.length gamma in
    sum n ~f:(fun i -> gamma.(i).(0) +. gamma.(i).(1)) /. float n

  let infer ?(niter = 100) ~motif_length sequences =
    let rec loop acc param niter =
      printf "iteration %d\n" niter ;
      if niter = 0 then List.rev acc
      else
        let gamma = gamma param sequences in
        let motif = update_motif motif_length gamma sequences in
        let param' = {
          bg = update_bg gamma sequences ;
          motif ;
          revcomp_motif = (
            let mat =
              Option.value_exn (Profile_matrix.of_array motif)
              |> Profile_matrix.reverse_complement
            in
            (mat :> float array array)
          ) ;
          pi = update_pi gamma ;
        }
        in
        loop ((gamma, param') :: acc) param' (niter - 1)
    in
    let p0 = initial_param ~motif_length in
    loop [] p0 niter

  let demo ?niter ?(alpha = 0.1) ?(motif_length = 6) ?(n_sequences = 100) () =
    let motif = Profile_matrix.random ~alpha motif_length in
    let sim = Simulation.make motif ~motif_probability:0.1 ~n_sequences in
    let inference_trace = infer ?niter ~motif_length (Array.append sim.motif_sequences sim.background_sequences) in
    let picture =
      let open Croquis.Picture in
      vstack ~align:`right (
        Profile_matrix.draw motif ::
        List.filter_map inference_trace ~f:(fun (_, p) ->
            Option.map (Profile_matrix.of_array p.motif) ~f:(fun motif ->
                hstack ~align:`centered [
                  text ~size:0.7 ~x:0. ~y:0. (sprintf "pi = %g" p.pi) ;
                  Profile_matrix.draw motif ;
                ]
              )
          )
      )
    in
    Croquis.Layout.(render_pdf (simple picture) "delme.pdf")
end
