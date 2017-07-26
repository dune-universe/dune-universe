open Core


type t = Time_ns.t [@@deriving sexp]
let of_time = Fn.id
let to_time = Fn.id
let add t x = Time_ns.add t x
let diff t x = Time_ns.diff x t

let of_int = Time_ns.of_int_ns_since_epoch
let to_int = Time_ns.to_int_ns_since_epoch
let to_string = Time_ns.to_string
