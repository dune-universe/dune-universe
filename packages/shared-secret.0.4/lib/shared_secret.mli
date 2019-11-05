module type IMessage = sig
  type t
  type a

  module Encoder : sig val encode : a -> t end;;
  module Decoder : sig val decode : t -> a end;;
end;;

module Message : functor (Type : sig type t end) ( ) -> (IMessage with type a := Type.t);;

module type IToken = sig
  type t
  type revoker

  exception RevokedToken
  exception AlreadyRevoked

  val create  : unit -> t * revoker
  val revoke  : revoker -> unit
  val revoked : t -> bool
  val (=)     : t -> t -> bool
end;;

module Token : IToken;;

module type IBox = sig
  type 'value t

  exception InvalidToken

  module Sealer   : sig val seal   : Token.t -> 'value   -> 'value t end;;
  module Unsealer : sig val unseal : Token.t -> 'value t -> 'value   end;;
end;;

module Box : IBox;;

module type IException = sig
  type t

  module Raiser  : sig val raise  : t -> 'a end;;
  module Handler : sig val handle : (unit -> 'a) -> (t -> 'a) -> 'a end;;
end;;

module Exception : functor (Type : sig type t end) -> (IException with type t := Type.t);;

module Revocable : functor ( ) -> sig
  exception RevokedReference
  exception AlreadyRevoked

  val revocable : ('a -> 'b) -> ('a -> 'b)
  val revoke    : unit -> unit
end;;

module Pair : sig
  val exceptional : unit -> ('a -> 'b) * ((unit -> 'c) -> ('a -> 'c) -> 'c)
  val sealing     : unit -> ('a -> 'a Box.t) * ('a Box.t -> 'a)
end;;

(* END *)
