open! Import

type t =
  { mutable data : Bits.t array
  ; mutable length : int
  }
[@@deriving sexp_of, compare]

let length d = d.length

let get d n =
  if n < d.length then d.data.(n) else raise (Invalid_argument "wave out of bounds")
;;

let create () = { data = [||]; length = 0 }
let init n f = { data = Array.init n ~f; length = n }

let resize d =
  let old_data = d.data in
  let new_len = max 1 (Array.length d.data * 2) in
  d.data
  <- Array.init new_len ~f:(fun i ->
    try old_data.(i) with
    | _ -> Bits.gnd)
;;

let rec set d n v =
  try
    d.data.(n) <- v;
    d.length <- max d.length (n + 1)
  with
  | _ ->
    resize d;
    set d n v
;;
