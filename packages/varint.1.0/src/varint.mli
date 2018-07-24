

module type VarIntEncoding = sig
  type t
         
  val to_cstruct: t -> Cstruct.t
  val of_cstruct: Cstruct.t -> t

  val read_varint: Mstruct.t -> t
  val write_varint: Mstruct.t -> t -> unit

  val to_int: t -> int
  val of_int: int -> t
                       
                             
end

(** Encoder for Int32 **)
module VarInt32: VarIntEncoding with type t = int32


(** Encoder for Int64*)
module VarInt64: VarIntEncoding with type t = int64




(** A module for length field prefixing with VarInts **)
module LengthFieldPrefixing:
functor (VI: VarIntEncoding) -> sig
  
  val encode: Cstruct.t -> Cstruct.t
  val decode: Mstruct.t -> Cstruct.t
                             
end 
