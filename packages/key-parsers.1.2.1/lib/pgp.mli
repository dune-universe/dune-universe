module Packet_error : sig
  type t =
    | Fatal of string
    | Header of
        { skip_length : int
        ; message : string }
end

module Algo : sig
  module Public : sig
    type t =
      | Rsa_enc_sign
      | Rsa_enc_only
      | Rsa_sign_only
      | Elgamal_enc_only
      | Dsa
      | Ec
      | Ecdsa
      | Unknown
    [@@deriving ord, eq, show]

    val of_int : int -> t

    val name : t -> string
  end

  module Hash : sig
    type t =
      | Md5
      | Sha1
      | Ripe_md160
      | Sha2_256
      | Sha2_384
      | Sha2_512
      | Sha2_224
      | Sha3_256
      | Sha3_512
      | Unknown_hash_algo
    [@@deriving ord, eq, show]

    val of_int : int -> t

    val name : t -> string
  end

  module Symmetric : sig
    type t =
      | Plaintext
      | Idea
      | Triple_des
      | Cast_5
      | Blowfish
      | Aes_128
      | Aes_192
      | Aes_256
      | Twofish_256
      | Unknown

    val size : t -> int

    val name : t -> string

    val of_int : int -> t
  end
end

module Rsa : sig
  module Public : sig
    type t =
      { n : Derivable.Z.t
      ; e : Derivable.Z.t }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> off:int -> Cstruct.t * t
  end

  module Private : sig
    type t =
      { d : Derivable.Z.t
      ; p : Derivable.Z.t
      ; q : Derivable.Z.t
      ; u : Derivable.Z.t }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> off:int -> Cstruct.t * t
  end
end

module Dsa : sig
  module Public : sig
    type t =
      { p : Derivable.Z.t
      ; q : Derivable.Z.t
      ; g : Derivable.Z.t
      ; y : Derivable.Z.t }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> off:int -> Cstruct.t * t
  end

  module Private : sig
    type t = Derivable.Z.t [@@deriving ord, eq, show]

    val decode : Cstruct.t -> off:int -> Cstruct.t * t
  end
end

module Elgamal : sig
  module Public : sig
    type t =
      { p : Derivable.Z.t
      ; g : Derivable.Z.t
      ; y : Derivable.Z.t }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> off:int -> Cstruct.t * t
  end

  module Private : sig
    type t = Derivable.Z.t [@@deriving ord, eq, show]

    val decode : Cstruct.t -> off:int -> Cstruct.t * t
  end
end

module Packet : sig
  type packet_type =
    | Session_key
    | Signature
    | Secret_key
    | Public_key
    | Secret_subkey
    | Id
    | Public_subkey
    | Unknown_packet
  [@@deriving ord, eq, show]

  val name : packet_type -> string

  module Header : sig
    type t =
      { packet_type : packet_type
      ; packet_length : int
      ; is_new : bool }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> (int * t, Packet_error.t) result
  end

  module Id : sig
    type t =
      { name : string
      ; email : string }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> t
  end

  module Public_key : sig
    module Value : sig
      type t =
        | Rsa of Rsa.Public.t
        | Dsa of Dsa.Public.t
        | Elgamal of Elgamal.Public.t
      [@@deriving ord, eq, show]
    end

    type t =
      { version : int
      ; creation_time : int32
      ; validity_period : int option
      ; algo : Algo.Public.t
      ; public_key : Value.t }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> (Cstruct.t * t, string) result
  end

  module Private_key_value : sig
    type t =
      | Rsa of Rsa.Private.t
      | Dsa of Dsa.Private.t
      | Elgamal of Elgamal.Private.t
    [@@deriving ord, eq, show]
  end

  module Secret_key : sig
    module S2k : sig
      type s2k_type =
        | Simple
        | Salted
        | Iterated_salted
        | Unknown

      val name : s2k_type -> string

      type t =
        | Simple of Algo.Hash.t
        | Salted of Algo.Hash.t * int64
        | Iterated_salted of Algo.Hash.t * int64 * int
      [@@deriving ord, eq, show]
    end

    type t =
      { public_key : Public_key.t
      ; s2k : S2k.t option
      ; initial_vector : string option
      ; private_key : Private_key_value.t option
      ; checksum : string option
      ; hash : string option }
    [@@deriving ord, eq, show]

    val decode : Cstruct.t -> (t, string) result
  end

  module Body : sig
    type t =
      | Id of Id.t
      | Secret_key of Secret_key.t
      | Public_key of Public_key.t
      | Signature
      | Secret_subkey of Secret_key.t
      | Public_subkey of Public_key.t
      | Unknown
    [@@deriving ord, eq, show]

    val decode : packet_type -> Cstruct.t -> (t, string) result
  end

  type t =
    { header : Header.t
    ; packet : Body.t }
  [@@deriving ord, eq, show]

  val decode : Cstruct.t -> (Cstruct.t * t, Packet_error.t) result
end

val decode : Cstruct.t -> (Packet.t, string) result list
