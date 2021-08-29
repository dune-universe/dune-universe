type 'a t = 'a Stats_intf.emp

let truncate (type elt) (module O : Basic_intf.Ordered with type t = elt)
    (distribution : elt t) (p : float) =
  if p <. 0.0 || p >. 1.0 then invalid_arg "truncate" ;
  Array.sort O.compare distribution ;
  let len = Array.length distribution in
  let plen = int_of_float (p *. float len) in
  Array.sub distribution 0 plen

let quantile (type elt) (module O : Basic_intf.Ordered with type t = elt)
    (distribution : elt t) (p : float) =
  if p <. 0.0 || p >. 1.0 then invalid_arg "quantile" ;
  Array.sort O.compare distribution ;
  let len = Array.length distribution in
  let plen = int_of_float (p *. float len) in
  distribution.(plen)

let map = Array.map

let of_generative ~nsamples (sampler : 'a Stats_intf.gen) rng_state =
  Array.init nsamples (fun _ -> sampler rng_state)

let of_raw_data array = array

let to_raw_data array = `Empirical array

module type Empirical_statistics = sig
  type r

  val empirical_mean : r t -> r

  val empirical_mean_generic :
    (module Basic_intf.Module with type t = 'elt and type R.t = r) ->
    'elt t ->
    'elt

  val empirical_variance : r t -> r
end

module Make_basic_statistics (Reals : Basic_intf.Reals) :
  Empirical_statistics with type r = Reals.t = struct
  type r = Reals.t

  (* mean/variance on empirical data *)
  let empirical_mean (distribution : Reals.t t) =
    let len = Array.length distribution in
    if len = 0 then invalid_arg "empirical_mean" ;
    let ilen = Reals.(one / of_int len) in
    let sum = Array.fold_left Reals.( + ) Reals.zero distribution in
    Reals.(ilen * sum)

  let empirical_mean_generic (type elt)
      (module L : Basic_intf.Module with type t = elt and type R.t = Reals.t)
      (distribution : elt t) =
    let len = Array.length distribution in
    if len = 0 then invalid_arg "empirical_mean_generic" ;
    let ilen = Reals.(one / of_int (Array.length distribution)) in
    let sum = Array.fold_left L.add L.zero distribution in
    L.smul ilen sum

  let empirical_variance (distribution : Reals.t t) =
    let mean = empirical_mean distribution in
    let ilen = Reals.(one / of_int (Array.length distribution)) in
    let sum =
      Array.fold_left
        (fun acc elt -> Reals.(acc + npow (elt - mean) 2))
        Reals.zero
        distribution
    in
    Reals.(ilen * sum)
end

module Float = Make_basic_statistics (Basic_impl.Reals.Float)
module Rational = Make_basic_statistics (Basic_impl.Reals.Rational)

let remove_outliers ~nsigmas (dist : float t) =
  if nsigmas <. 0.0 then invalid_arg "remove_outliers" ;
  let mean = Float.empirical_mean dist in
  let var = Float.empirical_variance dist in
  let std = sqrt var in
  let delta = std *. nsigmas in
  dist |> Array.to_list
  |> List.filter (fun x ->
         Basic_impl.Reals.Float.(abs_float (x -. mean) <= delta))
  |> Array.of_list
