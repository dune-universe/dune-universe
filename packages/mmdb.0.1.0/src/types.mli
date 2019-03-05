module Path : sig
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end

module Ip : sig
  type t [@@deriving show]

  include Base.Stringable.S with type t := t
end
