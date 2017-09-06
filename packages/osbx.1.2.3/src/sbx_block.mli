open Stdint
open Sbx_specs

module Header : sig
  exception Invalid_uid_length
  exception Missing_alt_seq_num
  exception Invalid_bytes

  type t

  type common_fields

  type raw_header =
    { version   : version
    ; crc_ccitt : uint16
    ; file_uid  : bytes
    ; seq_num   : uint32
    }

  val common_fields_to_ver : common_fields -> version

  (* raises Invalid_uid_length if uid length does not match the specs *)
  val make_common_fields   : ?uid:bytes    -> version    -> common_fields

  (* raises Invalid_bytes if bytes are invalid *)
  val of_bytes             : bytes         -> raw_header

  val raw_header_is_meta   : raw_header    -> bool

  val raw_header_is_data   : raw_header    -> bool
end

module Metadata : sig
  exception Too_much_data of string
  exception Invalid_bytes

  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of Multihash.hash_bytes
    | PID of bytes

  val dedup : t list -> t list
end

module Block : sig
  exception Too_much_data
  exception Invalid_bytes
  exception Invalid_size
  exception Invalid_seq_num

  type t

  type block_type = [ `Meta | `Data | `Any ]

  (* raises Metadata.Too_much_data if generated metadata bytes are too long *)
  val make_metadata_block : Header.common_fields          -> fields:Metadata.t list -> t

  (* raises
   *    Invalid_seq_num if provided seq_num is invalid
   *    Too_much_data   if provided data is too long
   *)
  val make_data_block     : ?seq_num:uint32               -> Header.common_fields   -> data:bytes -> t

  (* raises
   *    Header.Missing_alt_seq_num if all of the following conditions are met
   *      - block is a data block
   *      - block does not contain a sequence number
   *      - alt_seq_num is not specified
   *    Invalid_seq_num            if alt_seq_num is invalid
   *)
  val to_bytes            : ?alt_seq_num:uint32           -> t                      -> bytes

  (* raises
   *    Header.Invalid_bytes   if header bytes are invalid
   *    Metadata.Invalid_bytes if metadata bytes are invalid
   *    Invalid_size           if size of data is invalid
   *    Invalid_bytes          if CRC-CCITT of data does not match the one recorded in header
   *)
  val of_bytes            : ?raw_header:Header.raw_header -> ?skipped_already:bool  -> bytes      -> t

  val block_to_ver        : t -> version

  val block_to_file_uid   : t -> bytes

  val block_to_seq_num    : t -> uint32 option

  val block_to_data       : t -> bytes

  val block_to_meta       : t -> Metadata.t list

  val block_to_block_type : t -> block_type

  val is_meta             : t -> bool

  val is_data             : t -> bool
end
