open Base

let to_array arr = Array.init (Bigarray.Array1.dim arr) ~f:(fun i -> arr.{i})

let equal a1 a2 =
  assert (Bigarray.Array1.dim a1 = Bigarray.Array1.dim a2) ;
  let n = Bigarray.Array1.dim a1 in
  let rec loop i =
    if i >= n then true else if a1.{i} = a2.{i} then loop (i + 1) else false
  in
  loop 0
