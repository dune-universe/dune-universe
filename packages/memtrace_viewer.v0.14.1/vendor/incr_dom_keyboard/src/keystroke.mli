open Core_kernel
open Js_of_ocaml

module Keyboard_code : sig
  type t = Dom_html.Keyboard_code.t [@@deriving sexp, compare, bin_io, hash]
end

type t [@@deriving sexp, compare, bin_io, hash]

include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t

val create : Keyboard_code.t -> ctrl:bool -> alt:bool -> shift:bool -> meta:bool -> t
val create' : ?ctrl:unit -> ?alt:unit -> ?shift:unit -> ?meta:unit -> Keyboard_code.t -> t
val key : t -> Keyboard_code.t
val ctrl : t -> bool
val alt : t -> bool
val shift : t -> bool
val meta : t -> bool
val of_event : Keyboard_event.t -> t
val to_string_hum : t -> string
