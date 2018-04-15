module type Pipe =
  sig
    type t
    type reader
    type writer
    val create : unit -> reader * writer
    val write : t -> writer -> unit Lwt.t
    val write_with_pushback : t -> writer -> unit Lwt.t
    val read : reader -> t Lwt.t
  end

module Make :
  functor (Material : sig type t end) -> Pipe with type t = Material.t

module BoolPipe : Pipe with type t = bool
module StringPipe : Pipe with type t = string
module IntPipe : Pipe with type t = int
module CharPipe : Pipe with type t = char
