open Linalg
module V = Vec.Float

let mean vec =
  let dim = Tensor.Int.numel @@ V.idim vec in
  let tot = V.reduce ( +. ) 0.0 vec in
  tot /. float_of_int dim

let stddev vec =
  let m = mean vec in
  let shape = V.idim vec in
  let idim = 1. /. float_of_int (Tensor.Int.numel shape) in
  let delta = V.sub vec (V.const shape m) in
  sqrt @@ (idim *. V.reduce ( +. ) 0.0 (V.mul delta delta))

let in_of_array (a : float array) =
  let n = Array.length a in
  let shape = Tensor.Int.rank_one n in
  Vec (shape, fun i -> Array.unsafe_get a i)

let out_of_array (a : float array) =
  let n = Array.length a in
  let shape = Tensor.Int.rank_one n in
  OVec (shape, fun i v -> Array.unsafe_set a i v)

let standardize array =
  let vec = in_of_array array in
  let ovec = out_of_array array in
  let mean = mean vec in
  let std = stddev vec in
  let shape = V.idim vec in
  V.(ovec := smul (1. /. std) (sub vec (const shape mean)))

let test_array = [| 1.; 2.; 3. |]

let () =
  let vec = in_of_array test_array in
  assert (mean vec =. 2.) ;
  assert (String.equal (string_of_float (stddev vec)) "0.816496580928")

let () = standardize test_array

let () =
  let vec = in_of_array test_array in
  assert (mean vec =. 0.) ;
  assert (String.equal (string_of_float (stddev vec)) "1.")
