module type VALUE = sig
  (** the interface for values to sort *)

  type t
  (** the type of values *)

  val encode_sz : int
  (** the size of encoded values *)

  val encode : t -> string
  (** [encode] must satiisfies for all [t] [encode t |> String.length = encode_sz] *)
end

module type HYPER = sig
  (** Hyperparameters *)

  val ram : int
  (** the maximum number of bindings that can be loaded in memory at any point *)

  val kway : int
  (** the maximum value of [k] such that we allow a [k]-way merge to be performed. This in turn
      controls the maximum number of open file descriptors open at any point. *)
end

module type S = sig
  type v
  (** the type of values *)

  val sort : ?with_prog:bool -> oracle:(unit -> v) -> out:string -> int -> unit
  (** [sort ~with_prog ~oracle ~out n] invokes [oracle] [n] times and store the obtained values in
      file [out] in sorted order. [out] must be a valid file path. If [with_prog] is true, a
      progress bar is printed to the standard output. *)
end

module type MAKER = functor (V : VALUE) (H : HYPER) -> S with type v := V.t

module type Oracle = sig
  module Make : MAKER

  val stringv : encode_sz:int -> (module VALUE with type t = string)
  (** [stringv ~encode_sz] is the value module for strings of fixed length [encode_sz] *)

  module Default : HYPER
  (** default hyperparameters *)
end
