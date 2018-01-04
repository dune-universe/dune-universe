module Stable = struct
  open! Core.Core_stable
  module V1 = struct
    type t = Bigstring_shared.Stable.V1.t option [@@deriving bin_io, sexp]
  end
end

open! Core

type t = Bigstring_shared.t option [@@deriving compare, hash, sexp_of]

let of_bigstring_shared bstr = Some bstr
let of_string str = of_bigstring_shared (Bigstring_shared.of_string str)

let to_bigstring_shared = function
  | None      -> Bigstring_shared.empty
  | Some bstr -> bstr
;;

let length t = Bigstring_shared.length (to_bigstring_shared t)

module Expert = struct
  let of_bigstring_shared_option = Fn.id
  let to_bigstring_shared_option = Fn.id
end
