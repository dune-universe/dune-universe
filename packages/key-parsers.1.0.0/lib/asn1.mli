(** Parsers for RSA PKCS#1 and RSA, DSA, DH and EC PKCS#8 and X509 formats *)

module Rsa :
sig
  module Params :
  sig
    type t = unit
    val grammar : t Asn.t
  end

  module Public :
  sig
    type t = {
      n: Z.t;
      e: Z.t;
    }
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Private :
  sig
    type other_prime = {
      r: Z.t;
      d: Z.t;
      t: Z.t;
    }
    [@@deriving ord,eq,show]

    type t = {
      n: Z.t;
      e: Z.t;
      d: Z.t;
      p: Z.t;
      q: Z.t;
      dp: Z.t;
      dq: Z.t;
      qinv: Z.t;
      other_primes: other_prime list;
    }
    [@@deriving ord,eq,show]

    val other_prime_grammar : other_prime Asn.t
    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module Dsa :
sig
  module Params :
  sig
    type t = {
      p: Z.t;
      q: Z.t;
      g: Z.t;
    }
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Public :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Private :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module Ec :
sig
  type point = Cstruct.t
  [@@deriving ord,eq,show]

  val point_grammar : point Asn.t

  module Field :
  sig
    type basis =
      | GN
      | TP of Z.t
      | PP of Z.t * Z.t * Z.t
    [@@deriving ord,eq,show]

    val basis_grammar : basis Asn.t

    type characteristic_two_params = {
      m: Z.t;
      basis: basis;
    }
    [@@deriving ord,eq,show]

    val ctwo_params_grammar : characteristic_two_params Asn.t

    type t =
      | Prime of Z.t
      | C_two of characteristic_two_params
    [@@deriving ord,eq,show]

    val grammar : t Asn.t
  end

  module Specified_domain :
  sig
    type field_element = Cstruct.t
    [@@deriving eq,ord,show]

    val field_element_grammar : field_element Asn.t

    type curve = {
      a: field_element;
      b: field_element;
      seed: Cstruct.t option;
    }
    [@@deriving ord,eq,show]

    val curve_grammar : curve Asn.t

    type t = {
      field: Field.t;
      curve: curve;
      base: point;
      order: Z.t;
      cofactor: Z.t option;
    }
    [@@deriving ord,eq,show]

    val grammar : t Asn.t
  end

  module Params :
  sig
    type t =
      | Named of Asn.OID.t
      | Implicit
      | Specified of Specified_domain.t
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Public :
  sig
    type t = point
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Private :
  sig
    type t = {
      k: Cstruct.t;
      params: Params.t option;
      public_key: Public.t option;
    }
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module Dh :
sig
  module Params :
  sig
    type t = {
      p: Z.t;
      g: Z.t;
      l: Z.t option;
    }
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Public :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Private :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module Algorithm_identifier :
sig
  val rsa_grammar : Rsa.Params.t Asn.t
  val dsa_grammar : Dsa.Params.t Asn.t
  val ec_grammar : Ec.Params.t Asn.t
  val dh_grammar : Dh.Params.t Asn.t
end

module X509 :
sig
  type t =
    [ `RSA of Rsa.Public.t
    | `DSA of Dsa.Params.t * Dsa.Public.t
    | `EC of Ec.Params.t * Ec.Public.t
    | `DH of Dh.Params.t * Dh.Public.t
    ]
  [@@deriving ord,eq,show]

  val rsa_grammar : Rsa.Public.t Asn.t
  val dsa_grammar : (Dsa.Params.t * Dsa.Public.t) Asn.t
  val ec_grammar : (Ec.Params.t * Ec.Public.t) Asn.t
  val dh_grammar : (Dh.Params.t * Dh.Public.t) Asn.t

  val encode : t -> Cstruct.t
  val encode_rsa : Rsa.Public.t -> Cstruct.t
  val encode_dsa : (Dsa.Params.t * Dsa.Public.t) -> Cstruct.t
  val encode_ec : (Ec.Params.t * Ec.Public.t) -> Cstruct.t
  val encode_dh : (Dh.Params.t * Dh.Public.t) -> Cstruct.t
  val decode : Cstruct.t -> (t, string) Result.result
  val decode_rsa : Cstruct.t -> (Rsa.Public.t, string) Result.result
  val decode_dsa : Cstruct.t -> ((Dsa.Params.t * Dsa.Public.t), string) Result.result
  val decode_ec : Cstruct.t -> ((Ec.Params.t * Ec.Public.t), string) Result.result
  val decode_dh : Cstruct.t -> ((Dh.Params.t * Dh.Public.t), string) Result.result
end

module PKCS8 :
sig
  type t =
    [ `RSA of Rsa.Private.t
    | `DSA of Dsa.Params.t * Dsa.Private.t
    | `EC of Ec.Params.t * Ec.Private.t
    | `DH of Dh.Params.t * Dh.Private.t
    ]
  [@@deriving ord,eq,show]

  val rsa_grammar : Rsa.Private.t Asn.t
  val dsa_grammar : (Dsa.Params.t * Dsa.Private.t) Asn.t
  val ec_grammar : (Ec.Params.t * Ec.Private.t) Asn.t
  val dh_grammar : (Dh.Params.t * Dh.Private.t) Asn.t

  val encode : t -> Cstruct.t
  val encode_rsa : Rsa.Private.t -> Cstruct.t
  val encode_dsa : (Dsa.Params.t * Dsa.Private.t) -> Cstruct.t
  val encode_ec : (Ec.Params.t * Ec.Private.t) -> Cstruct.t
  val encode_dh : (Dh.Params.t * Dh.Private.t) -> Cstruct.t
  val decode : Cstruct.t -> (t, string) Result.result
  val decode_rsa : Cstruct.t -> (Rsa.Private.t, string) Result.result
  val decode_dsa : Cstruct.t -> ((Dsa.Params.t * Dsa.Private.t), string) Result.result
  val decode_ec : Cstruct.t -> ((Ec.Params.t * Ec.Private.t), string) Result.result
  val decode_dh : Cstruct.t -> ((Dh.Params.t * Dh.Private.t), string) Result.result
end
