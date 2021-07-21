(** 
   references:
   - https://academic.oup.com/bioinformatics/article/26/24/3028/289014#83366779
   - https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1002638#s4
   - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3967921/pdf/pcbi.1003501.pdf *)


open Core_kernel

let choose p =
  let x = Random.float 1. in
  let n = Array.length p in
  let rec loop i acc =
    if i < n then
      if Float.(acc > x) then (i - 1)
      else loop (i + 1) (acc +. (p :> float array).(i))
    else n - 1
  in
  loop 0 0.

let random_strand () =
  if Random.bool () then `Plus else `Minus

let rec random_read_pos ~region_length ~fragment_mean ~fragment_sd ~summit =
  let retry () =
    random_read_pos ~region_length ~fragment_mean ~fragment_sd ~summit
  in
  let occupied_width = 30 in
  assert (Float.to_int fragment_mean > 2 * occupied_width) ;
  let fragment_length =
    Owl.Stats.gaussian_rvs
      ~mu:fragment_mean
      ~sigma:fragment_sd
    |> Float.to_int in
  if fragment_length < occupied_width then retry ()
  else
    let delta = Random.int (fragment_length - occupied_width) / 2 in
    let strand = random_strand () in
    let pos = match strand with
      | `Plus -> summit - delta
      | `Minus -> summit + delta
    in
    if pos < 0 then retry ()
    else if pos >= region_length then retry ()
    else pos, strand

type simulation = {
  region_length : int ;
  peak_pos : int array ;
  peak_prop : float array ;
  reads : (int * [`Plus | `Minus] * [`Background | `Peak of int]) list ;
}

let simulator ~region_length ~nreads ~fragment_mean ~fragment_sd ~prop_background ~npeaks =
  assert (npeaks > 0) ;
  assert Float.(0. < prop_background && prop_background < 1.) ;
  let peak_pos = Array.init npeaks ~f:(fun _ -> Random.int region_length) in
  let peak_prop =
    Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:npeaks 0.5)
  in
  let reads =
    List.init nreads ~f:(fun _ ->
        if Float.(Random.float 1. < prop_background) then
          let pos = Random.int region_length in
          (pos, random_strand (), `Background)
        else
          let peak = choose peak_prop in
          let pos, strand = random_read_pos ~region_length ~fragment_mean ~fragment_sd ~summit:peak_pos.(peak) in
          (pos, strand, `Peak peak)
      )
  in
  { peak_pos ; peak_prop ; reads ; region_length }

type em_param = {
  m : int ;
  n : int ;
  pi : float array ;
  read_log_prob : float array array ;
  alpha : float ;
  strand : [`Plus | `Minus] array ;
  read_pos : int array ;
  peak_pos : int array ;
  region_length : int ;
}

let eps = 1e-30

let peak_read_prob ~binding_pos ~read_pos ~strand =
  let binding_pos = Float.of_int binding_pos in
  let read_pos = Float.of_int read_pos in
  match strand, Float.(read_pos < binding_pos) with
  | `Plus, false
  | `Minus, true -> Float.log eps
  | _ ->
    let x = Float.(abs (binding_pos - read_pos)) in
    let lp = Owl.Stats.gaussian_logpdf x ~mu:100. ~sigma:50. in
    (* printf "%f %f\n" x lp ; *)
    lp
        

let read_log_prob p i z_i =
  if z_i = 0 then Float.(- log (of_int p.region_length))
  else peak_read_prob ~binding_pos:p.peak_pos.(z_i - 1) ~read_pos:p.read_pos.(i) ~strand:p.strand.(i)  

let sum n ~f =
  let rec loop acc i =
    if i = n then acc
    else loop (acc +. f i) (i + 1)
  in
  loop 0. 0

let lnL p gamma =
  let m = Array.length p.pi in
  let n = Array.length p.read_log_prob in
  let log_prior = -. p.alpha *. sum m ~f:(fun j -> Float.log p.pi.(j)) in
  log_prior +. sum n ~f:(fun i ->
      sum m ~f:(fun j -> Float.(gamma.(i).(j) * (log p.pi.(j) + p.read_log_prob.(i).(j))))
    )

(* FIXME: stay in log for better precision? *)
let gamma param =
  Array.init param.n ~f:(fun i ->
      let den = sum param.m ~f:(fun j -> param.pi.(j) *. Float.exp param.read_log_prob.(i).(j)) in
      Array.init param.m ~f:(fun j -> param.pi.(j) *. Float.exp param.read_log_prob.(i).(j) /. den)
    )

let pi_hat param gamma =
  let _N = Array.init param.m ~f:(fun j ->
      sum param.n ~f:(fun i-> gamma.(i).(j))
    )
  in
  let den = sum param.m ~f:Float.(fun j -> max eps (_N.(j) - param.alpha)) in
  Array.init param.m ~f:Float.(fun j -> (max eps (_N.(j) -. param.alpha)) /. den)

let initial_param ~region_length ~reads ~motif_positions ~alpha =
  let m = Array.length motif_positions + 1 in
  let n = Array.length reads in
  let param = {
    m ; n ;
    pi = Array.init m ~f:(Fn.const (1. /. Float.of_int m)) ;
    (* pi = Array.init m ~f:(fun j -> if j = 0 then 0.99 else 0.01) ; *)
    alpha ;
    strand = Array.map reads ~f:snd ;
    read_pos = Array.map reads ~f:fst ;
    peak_pos = motif_positions ;
    region_length ;
    read_log_prob = [||]
  }
  in
  {
    param with read_log_prob =
                 Array.mapi reads ~f:(fun i _ ->
                     Array.init m ~f:(fun j -> read_log_prob param i j)
                   ) ;
  }

let em_infer ~region_length ~reads ~motif_positions ~alpha =
  let initial_param = initial_param ~region_length ~reads ~motif_positions ~alpha in
  let rec loop i param =
    if i = 0 then param
    else
      let gamma = gamma param in
      printf "%f\n" (lnL param gamma) ;
      let pi = pi_hat param gamma in
      let param = { param with pi } in
      loop (i - 1) param
  in
  loop 20 initial_param

let test ?(prop_background = 0.1) ?(alpha = 0.1) () =
  let { region_length ; reads ; peak_pos ; _ } as sim =
    simulator ~region_length:10_000 ~nreads:10_000 ~fragment_mean:200. ~fragment_sd:20. ~prop_background ~npeaks:4
  in
  let reads =
    Array.of_list reads
    |> Array.map ~f:(fun (pos, strand, _) -> pos, strand)
  in
  let res = em_infer ~region_length ~reads ~motif_positions:peak_pos ~alpha in
  sim.peak_pos,
  Array.map sim.peak_prop ~f:(fun x -> x *. (1. -. prop_background)),
  res.pi
