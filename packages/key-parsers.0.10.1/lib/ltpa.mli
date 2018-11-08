(** Lightweight Third Party Authentication - keys used in IBM Websphere & Lotus Notes*)

module Rsa : sig

  module Private : sig
    (** The format for private keys is:

           - 4 bytes: size of d encoded in big endian
           - d
           - 3 bytes: e (0x01 0x00 0x01)
           - p
           - q

         d, p and q are encoded with a leading 0x00. The size of p and q is
         determined from that of d (|p| = |q| = |d|/2 + 1).

         The format is a bit ambiguous if e is not 0x010001, so an error will be
         raised in that case.
    *)

    type t = {
      e: Z.t;
      d: Z.t;
      p: Z.t;
      q: Z.t;
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

  module Public : sig
    (** The format for public keys is:

        - n
        - e

        Here again there is an ambiguity, so e is assumed to be 0x010001: this
        is checked and an error is parsed if that is not the case.
    *)
    type t = {
      e: Z.t;
      n: Z.t;
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
