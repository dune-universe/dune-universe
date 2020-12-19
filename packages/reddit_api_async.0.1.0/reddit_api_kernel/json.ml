open! Core_kernel
include Ezjsonm

type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list
  ]
[@@deriving sexp, bin_io]

let of_string = from_string
let get_map t = Ezjsonm.get_dict t |> String.Map.of_alist_exn
