module Ip : sig
  (** A representation of an IP address. Can be both IPv4 as well as IPv6 *)
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end

module Language : sig
  (** A representation of a language supported by an MMDB database *)
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end

module Path : sig
  (** A representation of a path in the file system *)
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end

module Version_number : sig
  (** A representation of a version of the binary format of an MMDB database *)
  type t [@@deriving show]

  val of_major_and_minor : int * int -> t

  val to_major_and_minor : t -> int * int
end
