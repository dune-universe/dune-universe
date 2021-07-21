open Core_kernel

let int_fold a b ~init ~f =
  let rec loop acc i =
    if i = b then acc
    else loop (f acc i) (i + 1)
  in
  loop init a

let sum n ~f =
  int_fold 0 n ~init:0. ~f:(fun acc i -> acc +. f i)

let prod n ~f =
  int_fold 0 n ~init:1. ~f:(fun acc i -> acc *. f i)
