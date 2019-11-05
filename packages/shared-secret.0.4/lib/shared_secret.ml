module type IMessage = sig
  type t
  type a

  module Encoder : sig val encode : a -> t end;;
  module Decoder : sig val decode : t -> a end;;
end;;

module Message (Type : sig type t end) ( ) : (IMessage with type a := Type.t) = struct
  type t = Type.t

  module Encoder = struct let encode msg = msg end;;
  module Decoder = struct let decode msg = msg end;;
end;;

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

module Token : IToken = struct
  exception RevokedToken
  exception AlreadyRevoked

  class token ( ) = object
    val mutable revoked = false

    method rescind ( ) =
      if revoked then raise AlreadyRevoked
      else revoked <- true

    method rescinded ( ) =
      revoked
  end;;

  type t       = token
  type revoker = token

  let create ( ) =
    let instance = new token ( ) in
    (instance, instance)

    let revoke instance =
      instance # rescind ( )

    let revoked instance =
      instance # rescinded ( )

    let (=) = (=)
end;;

module type IBox = sig
  type 'value t

  exception InvalidToken

  module Sealer   : sig val seal   : Token.t -> 'value   -> 'value t end;;
  module Unsealer : sig val unseal : Token.t -> 'value t -> 'value   end;;
end;;

module Box : IBox = struct
  type 'value t = Token.t * 'value

  exception InvalidToken

  module Sealer = struct
    let seal token value =
      if Token.revoked token then
        raise Token.RevokedToken
      else
        (token, value)
  end;;

  module Unsealer = struct
    let unseal token (token', value) =
      if Token.(token = token') then
       (if Token.revoked token' then
          raise Token.RevokedToken
        else
          value)
      else
        raise InvalidToken
  end;;
end;;

module type IException = sig
  type t

  module Raiser  : sig val raise  : t -> 'a end;;
  module Handler : sig val handle : (unit -> 'a) -> (t -> 'a) -> 'a end;;
end;;

module Exception (Type : sig type t end) : (IException with type t := Type.t) = struct
  exception Class of Type.t

  module Raiser = struct
    let raise value = raise (Class value)
  end;;
  
  module Handler = struct
    let handle unsafe handler = try unsafe ( ) with Class value -> handler value
  end;;
end;;

module Revocable ( ) = struct
  exception RevokedReference
  exception AlreadyRevoked

  let revoked = ref false

  let revocable lambda value =
    if !revoked then raise RevokedReference else lambda value

  let revoke ( ) =
    if !revoked then raise AlreadyRevoked else revoked := true
end;;

module Pair = struct
  let exceptional (type a) ( ) =
    let module Module = Exception (struct type t = a end) in
    let raiser  = Module.Raiser.raise in
    let handler = Module.Handler.handle in
    (raiser, handler)

  let sealing ( ) =
    let (token, _) = Token.create ( ) in
    let sealer     = Box.Sealer.seal token in
    let unsealer   = Box.Unsealer.unseal token in
    (sealer, unsealer)
end;;

(* END *)
