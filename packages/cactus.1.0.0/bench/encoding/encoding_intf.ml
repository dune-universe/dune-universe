module type KEY = sig
  type t [@@deriving repr]

  val equal : t -> t -> bool

  val hash : t -> int

  val hash_size : int

  val encode : t -> string

  val encoded_size : int

  val decode : string -> int -> t
end

module type VALUE = sig
  type t [@@deriving repr]

  val encode : t -> string

  val encoded_size : int

  val decode : string -> int -> t
end

module type Encoding = sig
  module Hash : sig
    include Irmin.Hash.S

    val of_string : string -> (t, [ `Msg of string ]) result
  end

  module Int63 : sig
    type t = Optint.Int63.t

    val t : t Repr.t
  end

  type int63 = Int63.t [@@deriving repr]

  module Key : KEY with type t = Hash.t

  module Val : VALUE with type t = Int63.t * int * char

  val random : unit -> Key.t * Val.t
end
