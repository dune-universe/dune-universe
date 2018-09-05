(** Parsers for RSA and EC Card Verifiable Certificate key formats *)

module Rsa :
sig
  module Public :
  sig
    type t =
      { n: Z.t
      ; e: Z.t
      }
    [@@deriving eq,ord,show]

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

    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module Ec :
sig
  module Public :
  sig
    type t =
      { modulus : Z.t
      ; coefficient_a : Cstruct.t
      ; coefficient_b : Cstruct.t
      ; base_point_g : Cstruct.t
      ; base_point_r_order : Z.t
      ; public_point_y : Cstruct.t
      ; cofactor_f : Z.t
      }
    [@@deriving eq,ord,show]

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

    val decode : Cstruct.t -> (t, string) Result.result
  end
end

module RSA = Rsa
[@@ocaml.deprecated "Use module Rsa instead"]
module EC = Ec
[@@ocaml.deprecated "Use module Ec instead"]
