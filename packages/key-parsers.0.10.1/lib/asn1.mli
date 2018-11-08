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

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

    val bin_other_prime : other_prime Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_other_prime : other_prime -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_other_prime : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> other_prime
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_other_prime : Bin_prot.Common.buf -> pos:int -> other_prime -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_other_prime : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_other_prime : other_prime Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_other_prime : other_prime Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val other_prime_to_yojson : other_prime -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val other_prime_of_yojson : Yojson.Safe.json -> (other_prime, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Public :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Private :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module Ec :
sig
  type point = Cstruct.t
  [@@deriving ord,eq,show]

  val bin_point : point Bin_prot.Type_class.t0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_size_point : point -> int
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_read_point : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> point
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_write_point : Bin_prot.Common.buf -> pos:int -> point -> int
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_shape_point : Bin_prot.Shape.t
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_reader_point : point Bin_prot.Type_class.reader0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_writer_point : point Bin_prot.Type_class.writer0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

  val point_to_yojson : point -> Yojson.Safe.json
  [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
  val point_of_yojson : Yojson.Safe.json -> (point, string) result
  [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

  val point_grammar : point Asn.t

  module Field :
  sig
    type basis =
      | GN
      | TP of Z.t
      | PP of Z.t * Z.t * Z.t
    [@@deriving ord,eq,show]

    val bin_basis : basis Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_basis : basis -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_basis : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> basis
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_basis : Bin_prot.Common.buf -> pos:int -> basis -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_basis : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_basis : basis Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_basis : basis Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val basis_to_yojson : basis -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val basis_of_yojson : Yojson.Safe.json -> (basis, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val basis_grammar : basis Asn.t

    type characteristic_two_params = {
      m: Z.t;
      basis: basis;
    }
    [@@deriving ord,eq,show]

    val bin_characteristic_two_params : characteristic_two_params Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_characteristic_two_params : characteristic_two_params -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_characteristic_two_params :
      Bin_prot.Common.buf ->
      pos_ref:Bin_prot.Common.pos_ref ->
      characteristic_two_params
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_characteristic_two_params :
      Bin_prot.Common.buf ->
      pos:int ->
      characteristic_two_params ->
      int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_characteristic_two_params : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_characteristic_two_params : characteristic_two_params Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_characteristic_two_params : characteristic_two_params Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val characteristic_two_params_to_yojson : characteristic_two_params -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val characteristic_two_params_of_yojson :
      Yojson.Safe.json ->
      (characteristic_two_params, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val ctwo_params_grammar : characteristic_two_params Asn.t

    type t =
      | Prime of Z.t
      | C_two of characteristic_two_params
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t
  end

  module Specified_domain :
  sig
    type field_element = Cstruct.t
    [@@deriving eq,ord,show]

    val bin_field_element : field_element Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_field_element : field_element -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_field_element :
      Bin_prot.Common.buf ->
      pos_ref:Bin_prot.Common.pos_ref ->
      field_element
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_field_element : Bin_prot.Common.buf -> pos:int -> field_element -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_field_element : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_field_element : field_element Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_field_element : field_element Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val field_element_to_yojson : field_element -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val field_element_of_yojson : Yojson.Safe.json -> (field_element, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val field_element_grammar : field_element Asn.t

    type curve = {
      a: field_element;
      b: field_element;
      seed: Cstruct.t option;
    }
    [@@deriving ord,eq,show]

    val bin_curve : curve Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_curve : curve -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_curve : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> curve
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_curve : Bin_prot.Common.buf -> pos:int -> curve -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_curve : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_curve : curve Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_curve : curve Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val curve_to_yojson : curve -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val curve_of_yojson : Yojson.Safe.json -> (curve, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val curve_grammar : curve Asn.t

    type t = {
      field: Field.t;
      curve: curve;
      base: point;
      order: Z.t;
      cofactor: Z.t option;
    }
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t
  end

  module Params :
  sig
    type t =
      | Named of Asn.OID.t
      | Implicit
      | Specified of Specified_domain.t
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Public :
  sig
    type t = point
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Public :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

    val grammar : t Asn.t

    val encode : t -> Cstruct.t
    val decode : Cstruct.t -> (t, string) Result.result
  end

  module Private :
  sig
    type t = Z.t
    [@@deriving ord,eq,show]

    val bin_t : t Bin_prot.Type_class.t0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_size_t : t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_shape_t : Bin_prot.Shape.t
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_reader_t : t Bin_prot.Type_class.reader0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
    val bin_writer_t : t Bin_prot.Type_class.writer0
    [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

    val to_yojson : t -> Yojson.Safe.json
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
    val of_yojson : Yojson.Safe.json -> (t, string) result
    [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

  val bin_t : t Bin_prot.Type_class.t0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_size_t : t -> int
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_shape_t : Bin_prot.Shape.t
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_reader_t : t Bin_prot.Type_class.reader0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_writer_t : t Bin_prot.Type_class.writer0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

  val to_yojson : t -> Yojson.Safe.json
  [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
  val of_yojson : Yojson.Safe.json -> (t, string) result
  [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

  val bin_t : t Bin_prot.Type_class.t0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_size_t : t -> int
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_shape_t : Bin_prot.Shape.t
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_reader_t : t Bin_prot.Type_class.reader0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]
  val bin_writer_t : t Bin_prot.Type_class.writer0
  [@@ocaml.deprecated "Bin_prot serializers will be removed in key-parsers 1.0.0"]

  val to_yojson : t -> Yojson.Safe.json
  [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]
  val of_yojson : Yojson.Safe.json -> (t, string) result
  [@@ocaml.deprecated "Yojson serializers will be removed in key-parsers 1.0.0"]

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

module RSA = Rsa
[@@ocaml.deprecated "Use module Rsa instead"]
module DSA = Dsa
[@@ocaml.deprecated "Use module Dsa instead"]
module EC = Ec
[@@ocaml.deprecated "Use module Ec instead"]
module DH = Dh
[@@ocaml.deprecated "Use module Dh instead"]
