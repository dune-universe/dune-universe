type t = {
  mutable total_weight : float;
  cumulative : float array;
}

let ideal_soliton_weights ~k : float array =
  let arr = Array.make k 0.0 in
  arr.(0) <- 1.0 /. float_of_int k;
  for i = 2 to k do
    arr.(i - 1) <- 1.0 /. float_of_int (i * (i - 1))
  done;
  arr

let of_weights (weights : float array) : t =
  let total_weight =
    CCArray.foldi
      (fun total i weight ->
        weights.(i) <- total;
        total +. weight)
      0.0 weights
  in
  { total_weight; cumulative = weights }

let robust_soliton_dist ~k : t =
  assert (k > 0);
  let failure_probability = 0.01 in
  let c = 0.001 in
  let k' = float_of_int k in
  let r = c *. log (k' /. failure_probability) *. sqrt k' in
  let k_div_r = int_of_float (k' /. r) in
  let t i =
    let i = i + 1 in
    if i < k_div_r then r /. float_of_int (i * k)
    else if i = k_div_r then r *. log (r /. failure_probability) /. k'
    else 0.
  in
  let weights = ideal_soliton_weights ~k in
  Array.iteri (fun i v -> weights.(i) <- t i +. v) weights;
  of_weights weights

let choose_into ?(offset = 0) (d : t) (arr : int array) : unit =
  let aux () =
    let v = Random.float d.total_weight in
    let res =
      match CCArray.bsearch ~cmp:Float.compare v d.cumulative with
      | `All_lower -> Array.length d.cumulative
      | `All_bigger -> 1
      | `Just_after i -> i + 1
      | `Empty -> failwith "Unexpected case"
      | `At i -> i + 1
    in
    assert (1 <= res && res <= Array.length d.cumulative);
    res
  in
  let n = Array.length arr in
  assert (offset < n);
  Random.self_init ();
  for i = offset to n - 1 do
    arr.(i) <- aux ()
  done

let choose_n (d : t) n : int array =
  let arr = Array.make n 0 in
  choose_into d arr;
  arr
