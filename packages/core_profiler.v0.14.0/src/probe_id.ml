open Core

module T = struct
  type t = int [@@deriving bin_io, compare, sexp, hash]
  let to_string = Int.to_string
  let of_string = Int.of_string
  let module_name = "Core_profiler.Common.Id"
end

include T
include Identifiable.Make(T)

let of_int_exn = Fn.id
let to_int_exn = Fn.id

let counter = ref (-1)
let create () = incr counter; !counter
