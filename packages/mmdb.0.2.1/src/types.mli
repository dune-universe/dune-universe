module Path : sig
  (** A representation of a path in the file system *)
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end

module Ip : sig
  (** A representation of an IP address. Can be both IPv4 as well as IPv6 *)
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end
