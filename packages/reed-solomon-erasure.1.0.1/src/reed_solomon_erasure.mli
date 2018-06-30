(** This library provides an encoder/decoder for Reed-Solomon erasure code.

    Please note that erasure coding means errors are not directly detected or corrected,
    but missing data pieces(shards) can be reconstructed given that
    the configuration provides high enough redundancy.

    You will have to implement error detection separately(e.g. via checksums)
    and simply leave out the corrupted shards when attempting to reconstruct
    the missing data.
*)

type reed_solomon
(** Reed-Solomon codec

    {b Warning} : This type is {b NOT} thread-safe *)

type shard_by_shard
(** Reed-Solomon codec

    {b Warning} : This type is {b NOT} thread-safe *)

type bigstring = Core_kernel.Bigstring.t

module RS_Error : sig
  type t = TooFewShards
         | TooManyShards
         | TooFewDataShards
         | TooManyDataShards
         | TooFewParityShards
         | TooManyParityShards
         | TooFewBufferShards
         | TooManyBufferShards
         | IncorrectShardSize
         | TooFewShardsPresent
         | EmptyShard
         | InvalidShardFlags
         | InvalidIndex

  module Exn : sig
    exception TooFewShards
    exception TooManyShards
    exception TooFewDataShards
    exception TooManyDataShards
    exception TooFewParityShards
    exception TooManyParityShards
    exception TooFewBufferShards
    exception TooManyBufferShards
    exception IncorrectShardSize
    exception TooFewShardsPresent
    exception EmptyShard
    exception InvalidShardFlags
    exception InvalidIndex
  end

  val to_exn : t -> exn

  val unwrap : ('a, t) result -> 'a
end

module ReedSolomon : sig
  (** Reed-Solomon erasure code encoder/decoder.

      {1 Naming conventions}
      [typ] represents [bytes], [str], and [bigstr], e.g. [encode_typ] represents [encode_bytes], [encode_str], and [encode_bigstr].

      {1 Common error handling}
      {2 For [encode_typ], [verify_typ], [reconstruct_typ], [reconstruct_data_typ], [reconstruct_opt_typ], [reconstruct_data_opt_typ]}

      Raises {! RS_Error.Exn.TooFewShards} or {! RS_Error.TooManyShards} when the number of provided shards does not match the codec's one.

      Raises {! RS_Error.Exn.EmptyShard} when the first shard provided is of zero length.

      Raises {! RS_Error.Exn.IncorrectShardSize} when the provided shards are of different lengths.

      {2 For [reconstruct_typ], [reconstruct_data_typ], [reconstruct_opt_typ], [reconstruct_data_opt_typ]}

      Raises {! RS_Error.Exn.TooFewShardsPresent} when there are not enough shards for reconstruction.

      Raises {! RS_Error.Exn.InvalidShardFlags} when the number of flags does not match the total number of shards.

      {1 Variants of encoding methods}
      {2 [sep]}
      Methods ending in [_sep] use data shards immutably, and parity shards mutably.

      They are useful as they do not need to use of data shards mutably, and other
      work that only needs read-only access to data shards can be done in
      paralle/concurrently during the encoding.

      Following is a table of all the [sep] variants
      {v
| not sep           | sep                   |
| ----------------- | --------------------- |
| encode_single_typ | encode_single_sep_typ |
| encode_typ        | encode_sep_typ        |
v}

      The [sep] variants do similar checks on the provided data shards and parity shards.

      Raise {! RS_Error.Exn.TooFewDataShards}, {! RS_Error.Exn.TooManyDataShards},
      {! RS_Error.Exn.TooFewParityShards}, or {! RS_Error.Exn.TooManyParityShards}
      when applicable.

      {2 [single]}
      Functions containg [single] facilitate shard by shard encoding, where the parity
      shards are partially constructed using one data shard at a time. See {! ShardByShard}
      for more details on how shard by shard encoding can be useful.

      They are prone to {b misuse}, and it is recommended to use the {! shard_by_shard}
      bookkeeping type instead for shard by shard encoding.

      The ones that are also [sep] are {b ESPECIALLY} prone to {b misuse}. Only use them when
      you actually need the flexibility.

      Following is a table of all the [single] variants
      {v
| all shards at once | shard by shard        |
| ------------------ | --------------------- |
| encode_typ         | encode_single_typ     |
| encode_sep_typ     | encode_single_sep_typ |
v}

      The [signle] variants do similar checks on the provided data shards and parity shards,
      and also do index check on [i_data].

      Raise {! RS_Error.Exn.InvalidIndex} if [i_data < 0 || i_data >= data_shard_count].

      {1 Encoding behaviour}
      {2 For [encode_typ]}
      You do not need to clear the parity shards beforehand, as the functions
      will overwrite them completely.

      {2 For [encode_single_typ], [encode_single_sep_typ]}
      Calling them with [i_data] being [0] will overwrite the parity shards
      completely. If you are using the methods correctly, then you do not need
      to clear the parity shards beforehand.

      {1 Variants of verifying methods}
      [verify_typ] allocate a buffer of the same size as the parity shards, and
      encode the input once using the buffer to store the computed parity shards,
      then check if the provided parity shards match the computed ones.

      [verify_with_buffer_typ] allow you to provide the buffer to avoid making
      allocations(s) for the buffer in every call.

      The [with_buffer] variants also guarnatee that the buffer contains the
      parity shards if the result is [Ok _] (i.e. it does not matter whether the
      verification passed or not, as long as the result is not an error, the buffer
      will contain the correct parity shards after the call).

      [verify_ret_buffer_typ] are the same as [verify_typ] in behaviour, but returns
      the buffer along with the verification result. The [ret_buffer] variants
      provide the same guarantees as the [with_buffer] variants with regards to the
      buffer.

      Following is a table of all the [with_buffer] variants
      {v
| not with_buffer | with_buffer            |
| --------------- | ---------------------- |
| verify_typ      | verify_with_buffer_typ |
v}

      Following is a table of all the [ret_buffer] variants
      {v
| not ret_buffer | ret_buffer            |
| -------------- | --------------------- |
| verify_typ     | verify_ret_buffer_typ |
v}

      {1 Error returning variants}
      All functions which may raise an exception have a error returning counterpart
      that returns an error instead of raising an exception.

      The error returning variants share the same name but with [no_exn] attached
      at the end.

      {1 Performance}
      [bigstr] functions have highest performance, as they modify and use the shards
      in place.

      [bytes] and [str] functions are wrappers around the [bigstr] functions, and
      due to different memory representations of bytes and strings, there will be
      a copying from [bytes] or [string] to [bigstring] before processing, then another
      copying from [bigstring] to [bytes] or [string] after processing (if necessary).

      Usage of [bigstring] is necessary here because only it allows easy and safe use of the
      C code that does the SIMD enabled Galois operation.

      Note that even with copying, it is still significantly faster than the pure OCaml
      version (~20 times faster on my SIMD enabled system), so unless the garbage collector
      workload level is particularly important in your project, performance aspects of
      [bytes] and [string] should be acceptable.

      {1 Ownership of shards}
      Only [bigstr] functions modify and use the shards in-place, both [bytes] and
      [str] functions do not modify or use the shards in-place.

      More specifically, both [bytes] and [str] functions modify the arrays in-place,
      but they replace the shards by new shards instead of modifying the [string] or
      [bytes]. This means if you have previously bound some shards within the array
      to some identifiers, the bound shards will not reflect the changes.
  *)

  val make : int -> int -> reed_solomon
  (** [make data_shards parity_shards]

      Creates a new instance of Reed-Solomon erasure code codec.
      Note that the codec is {b NOT} thread-safe.

      Raises {! RS_Error.Exn.TooFewDataShards} if [data_shards <= 0].

      Raises {! RS_Error.Exn.TooFewParityShards} if [parity_shards <= 0].

      Raises {! RS_Error.Exn.TooManyShards} if [data_shards + parity_shards > 256].
  *)

  val make_no_exn : int -> int -> (reed_solomon, RS_Error.t) result
  (** [make_no_exn data_shards parity_shards]

      Error returning version of {!make}
  *)

  val data_shard_count : reed_solomon -> int
  (** [data_shard_count r]
  *)

  val parity_shard_count : reed_solomon -> int
  (** [parity_shard_count r]
  *)

  val total_shard_count : reed_solomon -> int
  (** [total_shard_count r]
  *)

  (* Encode functions *)
  val encode_bytes : reed_solomon -> bytes array -> unit
  (** [encode_bytes r shards]
  *)

  val encode_single_bytes : reed_solomon -> int -> bytes array -> unit
  (** [encode_single_bytes r i_data shards]

      Wrapper of {! encode_single_bigstr}.
  *)

  val encode_single_sep_bytes : reed_solomon -> int -> bytes -> bytes array -> unit
  (** [encode_single_sep_bytes r i_data single_data parity]

      Wrapper of {! encode_single_bigstr}.
  *)

  val encode_sep_bytes : reed_solomon -> bytes array -> bytes array -> unit
  (** [encode_sep_bytes r data parity]

      Wrapper of {! encode_sep_bigstr}.
  *)

  val encode_str : reed_solomon -> string array -> unit
  (** [encode_str r shards]

      Wrapper of {! encode_bigstr}.
  *)

  val encode_single_str : reed_solomon -> int -> string array -> unit
  (** [encode_single_str r i_data shards]

      Wrapper of {! encode_single_bigstr}.
  *)

  val encode_single_sep_str : reed_solomon -> int -> string -> string array -> unit
  (** [encode_single_sep_str r i_data single_data parity]

      Wrapper of {! encode_single_sep_bigstr}.
  *)

  val encode_sep_str : reed_solomon -> string array -> string array -> unit
  (** [encode_sep_str r data parity]

      Wrapper of {! encode_sep_bigstr}.
  *)

  val encode_bigstr : reed_solomon -> bigstring array -> unit
  (** [encode_bigstr r shards]
  *)

  val encode_single_bigstr : reed_solomon -> int -> bigstring array -> unit
  (** [encode_single_bigstr r i_data shards]

      Constructs the parity shards partially using only the data shard indexed by
      [i_data].

      The slots where the parity shards sit at will be overwritten.

      {3 Warning}
      You must apply this function on the data shards in strict sequential
      order(0..data shard count), otherwise the parity shards will be incorrect.

      It is recommended to use the {! shard_by_shard} bookkeeping type instead of
      this function directly.
  *)

  val encode_single_sep_bigstr : reed_solomon -> int -> bigstring -> bigstring array -> unit
  (** [encode_single_sep_bigstr r i_data single_data parity]

      Constructs the parity shards partially using only the data shard provided.

      The data shard must match the index [i_data].

      The slots where the parity shards sit at will be overwritten.

      {3 Warning}
      You must apply this function on the data shards in strict sequential
      order(0..data shard count), otherwise the parity shards will be incorrect.

      It is recommended to use the {! shard_by_shard} bookkeeping type instead of
      this function directly.
  *)

  val encode_sep_bigstr : reed_solomon -> bigstring array -> bigstring array -> unit
  (** [encode_sep_bigstr r data parity]

      Constructs the parity shards.

      The slots where the parity shards sit at will be overwritten.
  *)

  val encode_bytes_no_exn : reed_solomon -> bytes array -> (unit, RS_Error.t) result
  (** [encode_bytes_no_exn r shards]

      Error returning variant of {! encode_bytes}.
  *)

  val encode_single_bytes_no_exn : reed_solomon -> int -> bytes array -> (unit, RS_Error.t) result
  (** [encode_single_bytes_no_exn r i_data shards]

      Error returning variant of {! encode_single_bytes}.
  *)

  val encode_single_sep_bytes_no_exn : reed_solomon -> int -> bytes -> bytes array -> (unit, RS_Error.t) result
  (** [encode_single_sep_bytes_no_exn r i_data single_data parity]

      Error returning variant of {! encode_single_sep_bytes}.
  *)

  val encode_sep_bytes_no_exn : reed_solomon -> bytes array -> bytes array -> (unit, RS_Error.t) result
  (** [encode_sep_bytes_no_exn r data parity]

      Error returning variant of {! encode_sep_bytes}.
  *)

  val encode_str_no_exn : reed_solomon -> string array -> (unit, RS_Error.t) result
  (** [encode_str_no_exn r shards]

      Error returning variant of {! encode_str}.
  *)

  val encode_single_str_no_exn : reed_solomon -> int -> string array -> (unit, RS_Error.t) result
  (** [encode_single_str_no_exn r i_data shards]

      Error returning variant of {! encode_single_str}.
  *)

  val encode_single_sep_str_no_exn : reed_solomon -> int -> string -> string array -> (unit, RS_Error.t) result
  (** [encode_single_sep_str_no_exn r i_data single_data parity]

      Error returning variant of {! encode_single_sep_str}.
  *)

  val encode_sep_str_no_exn : reed_solomon -> string array -> string array -> (unit, RS_Error.t) result
  (** [encode_single_sep_str_no_exn r i_data single_data parity]

      Error returning variant of {! encode_sep_str}.
  *)

  val encode_bigstr_no_exn : reed_solomon -> bigstring array -> (unit, RS_Error.t) result
  (** [encode_bigstr_no_exn r shards]

      Error returning variant of {! encode_bigstr}.
  *)

  val encode_single_bigstr_no_exn : reed_solomon -> int -> bigstring array -> (unit, RS_Error.t) result
  (** [encode_single_bigstr_no_exn r i_data shards]

      Error returning variant of {! encode_single_bigstr}.
  *)

  val encode_single_sep_bigstr_no_exn : reed_solomon -> int -> bigstring -> bigstring array -> (unit, RS_Error.t) result
  (** [encode_single_sep_bigstr_no_exn r i_data single_data parity]

      Error returning variant of {! encode_single_sep_bigstr}.
  *)

  val encode_sep_bigstr_no_exn : reed_solomon -> bigstring array -> bigstring array -> (unit, RS_Error.t) result
  (** [encode_sep_bigstr_no_exn r data parity]

      Error returning variant of {! encode_sep_bigstr}.
  *)

  (* Verify functions *)
  val verify_bytes : reed_solomon -> bytes array -> bool
  (** [verify_bytes r shards]

      Wrapper of {! verify_bigstr}.
  *)

  val verify_ret_buffer_bytes : reed_solomon -> bytes array -> bool * bytes array
  (** [verify_ret_buffer_bytes r shards]

      Wrapper of {! verify_ret_buffer_bigstr}.
  *)

  val verify_with_buffer_bytes : reed_solomon -> bytes array -> bytes array -> bool
  (** [verify_with_buffer_bytes r shards buffer]

      Wrapper of {! verify_with_buffer_bigstr}.
  *)

  val verify_str : reed_solomon -> string array -> bool
  (** [verify_str r shards]

      Wrapper of {! verify_bigstr}.
  *)

  val verify_ret_buffer_str : reed_solomon -> string array -> bool * string array
  (** [verify_ret_buffer_str r shards]

      Wrapper of {! verify_ret_buffer_bigstr}.
  *)

  val verify_with_buffer_str : reed_solomon -> string array -> string array -> bool
  (** [verify_with_buffer_str r shards buffer]

      Wrapper of {! verify_with_buffer_bigstr}.
  *)

  val verify_bigstr : reed_solomon -> bigstring array -> bool
  (** [verify_bigstr r shards]

      Checks if the parity shards are correct.
  *)

  val verify_ret_buffer_bigstr : reed_solomon -> bigstring array -> bool * bigstring array
  (** [verify_ret_buffer_bigstr r shards]

      Checks if the parity shards are correct.
  *)

  val verify_with_buffer_bigstr : reed_solomon -> bigstring array -> bigstring array -> bool
  (** [verify_with_buffer_bigstr r shards buffer]

      Checks if the parity shards are correct.
  *)

  val verify_bytes_no_exn : reed_solomon -> bytes array -> (bool, RS_Error.t) result
  (** [verify_bytes_no_exn r shards]

      Error returning variant of {! verify_bytes}.
  *)

  val verify_ret_buffer_bytes_no_exn : reed_solomon -> bytes array -> (bool * bytes array, RS_Error.t) result
  (** [verify_ret_buffer_bytes_no_exn r shards]

      Error returning variant of {! verify_ret_buffer_bytes}.
  *)

  val verify_with_buffer_bytes_no_exn : reed_solomon -> bytes array -> bytes array -> (bool, RS_Error.t) result
  (** [verify_with_buffer_bytes_no_exn r shards buffer]

      Error returning variant of {! verify_with_buffer_bytes}.
  *)

  val verify_str_no_exn : reed_solomon -> string array -> (bool, RS_Error.t) result
  (** [verify_str_no_exn r shards]

      Error returning variant of {! verify_str}.
  *)

  val verify_ret_buffer_str_no_exn : reed_solomon -> string array -> (bool * string array, RS_Error.t) result
  (** [verify_ret_buffer_str_no_exn r shards]

      Error returning variant of {! verify_ret_buffer_str}.
  *)

  val verify_with_buffer_str_no_exn : reed_solomon -> string array -> string array -> (bool, RS_Error.t) result
  (** [verify_with_buffer_str_no_exn r shards buffer]

      Error returning variant of {! verify_with_buffer_str}.
  *)

  val verify_bigstr_no_exn : reed_solomon -> bigstring array -> (bool, RS_Error.t) result
  (** [verify_bigstr_no_exn r shards]

      Error returning variant of {! verify_bigstr}.
  *)

  val verify_ret_buffer_bigstr_no_exn : reed_solomon -> bigstring array -> (bool * bigstring array, RS_Error.t) result
  (** [verify_ret_buffer_bigstr_no_exn r shards]

      Error returning variant of {! verify_ret_buffer_bigstr}.
  *)

  val verify_with_buffer_bigstr_no_exn : reed_solomon -> bigstring array -> bigstring array -> (bool, RS_Error.t) result
  (** [verify_with_buffer_bigstr_no_exn r shards buffer]

      Error returning variant of {! verify_with_buffer_bigstr}.
  *)

  (* Reconstruct functions *)
  val reconstruct_bytes : reed_solomon -> bytes array -> bool array -> unit
  (** [reconstruct_bytes r shards shard_present]

      Wrapper of {! reconstruct_bigstr}.
  *)

  val reconstruct_data_bytes : reed_solomon -> bytes array -> bool array -> unit
  (** [reconstruct_data_bytes r shards shard_present]

      Wrapper of {! reconstruct_data_bigstr}.
  *)

  val reconstruct_opt_bytes : reed_solomon -> bytes option array -> unit
  (** [reconstruct_opt_bytes r shards]

      Wrapper of {! reconstruct_opt_bigstr}.
  *)

  val reconstruct_data_opt_bytes : reed_solomon -> bytes option array -> unit
  (** [reconstruct_data_opt_bytes r shards]

      Wrapper of {! reconstruct_data_opt_bytes}.
  *)

  val reconstruct_str : reed_solomon -> string array -> bool array -> unit
  (** [reconstruct_str r shards shard_present]

      Wrapper of {! reconstruct_bigstr}.
  *)

  val reconstruct_data_str : reed_solomon -> string array -> bool array -> unit
  (** [reconstruct_data_str r shards shard_present]

      Wrapper of {! reconstruct_data_bigstr}.
  *)

  val reconstruct_opt_str : reed_solomon -> string option array -> unit
  (** [reconstruct_opt_str r shards]

      Wrapper of {! reconstruct_opt_bigstr}.
  *)

  val reconstruct_data_opt_str : reed_solomon -> string option array -> unit
  (** [reconstruct_data_opt_str r shards]

      Wrapper of {! reconstruct_data_opt_bigstr}.
  *)

  val reconstruct_bigstr : reed_solomon -> bigstring array -> bool array -> unit
  (** [reconstruct_bigstr r shards shard_present]

      Reconstructs all shards.

      The shards marked not present are only overwritten when no error is detected.

      This means if the method returns an [Error], then nothing is touched.
  *)

  val reconstruct_data_bigstr : reed_solomon -> bigstring array -> bool array -> unit
  (** [reconstruct_data_bigstr r shards shard_present]

      Reconstructs only the data shards.

      The shards marked out present are only overwritten when no error is detected.

      This meeans if the method returns an [Error], then nothing is touched.
  *)

  val reconstruct_opt_bigstr : reed_solomon -> bigstring option array -> unit
  (** [reconstruct_opt_bigstr r shards]

      Reconstructs all shards.

      This fills in the missing shards with blank shards if and only if there
      are enough shards for reconstruction.
  *)

  val reconstruct_data_opt_bigstr : reed_solomon -> bigstring option array -> unit
  (** [reconstruct_data_opt_bigstr r shards]

      Reconstructs only the data shards.

      This fills in the missing shards with blank shards if and only if there
      are enough shards for reconstruction.
  *)

  val reconstruct_bytes_no_exn : reed_solomon -> bytes array -> bool array -> (unit, RS_Error.t) result
  (** [reconstruct_bytes_no_exn r shards shard_present]

      Error returning variant of {! reconstruct_bytes}.
  *)

  val reconstruct_data_bytes_no_exn : reed_solomon -> bytes array -> bool array -> (unit, RS_Error.t) result
  (** [reconstruct_data_bytes_no_exn r shards shard_present]

      Error returning variant of {! reconstruct_data_bytes}.
  *)

  val reconstruct_opt_bytes_no_exn : reed_solomon -> bytes option array -> (unit, RS_Error.t) result
  (** [reconstruct_opt_bytes_no_exn r shards]

      Error returning variant of {! reconstruct_opt_bytes}.
  *)

  val reconstruct_data_opt_bytes_no_exn : reed_solomon -> bytes option array -> (unit, RS_Error.t) result
  (** [reconstruct_data_opt_bytes_no_exn r shards]

      Error returning variant of {! reconstruct_data_opt}.
  *)

  val reconstruct_str_no_exn : reed_solomon -> string array -> bool array -> (unit, RS_Error.t) result
  (** [reconstruct_str_no_exn r shards shard_present]

      Error returning variant of {! reconstruct_str}.
  *)

  val reconstruct_data_str_no_exn : reed_solomon -> string array -> bool array -> (unit, RS_Error.t) result
  (** [reconstruct_data_str_no_exn r shards shard_present]

      Error returning variant of {! reconstruct_data_str}.
  *)

  val reconstruct_opt_str_no_exn : reed_solomon -> string option array -> (unit, RS_Error.t) result
  (** [reconstruct_opt_str_no_exn r shards]

      Error returning variant of {! reconstruct_opt_str}.
  *)

  val reconstruct_data_opt_str_no_exn : reed_solomon -> string option array -> (unit, RS_Error.t) result
  (** [reconstruct_data_opt_str_no_exn r shards]

      Error returning variant of {! reconstruct_data_opt_str}.
  *)

  val reconstruct_bigstr_no_exn : reed_solomon -> bigstring array -> bool array -> (unit, RS_Error.t) result
  (** [reconstruct_bigstr_no_exn r shards shard_present]

      Error returning variant of {! reconstruct_bigstr}.
  *)

  val reconstruct_data_bigstr_no_exn : reed_solomon -> bigstring array -> bool array -> (unit, RS_Error.t) result
  (** [reconstruct_data_bigstr_no_exn r shards shard_present]

      Error returning variant {! reconstruct_data_bigstr}.
  *)

  val reconstruct_opt_bigstr_no_exn : reed_solomon -> bigstring option array -> (unit, RS_Error.t) result
  (** [reconstruct_opt_bigstr_no_exn r shards]

      Error returning variant of {! reconstruct_opt_bigstr}.
  *)

  val reconstruct_data_opt_bigstr_no_exn : reed_solomon -> bigstring option array -> (unit, RS_Error.t) result
  (** [reconstruct_data_opt_bigstr_no_exn r shards]

      Error returning variant of {! reconstruct_data_opt_bigstr}.
  *)
end

module RS_SBS_Error : sig
  type t = TooManyCalls
         | LeftoverShards
         | RSError of RS_Error.t

  module Exn : sig
    exception TooManyCalls
    exception LeftoverShards
    exception RSError of RS_Error.t
  end

  val to_exn : t -> exn

  val unwrap : ('a, t) result -> 'a
end

module ShardByShard : sig
  (** Bookkeeper for shard by shard encoding.

      This is useful for avoiding incorrect use of [encode_single_typ], and [encode_single_sep_typ]
      functions.

      {1 Naming conventions}
      [typ] represents [bytes], [str], and [bigstr], e.g. [encode_typ] represents [encode_bytes],
      [encode_str], and [encode_bigstr].

      {1 Use cases}
      Shard by shard encoding is useful for streamed data encoding
      where you do not have all the needed data shards immediately,
      but you want to spread out the encoding workload rather than
      doing the encoding after everything is ready.

      A concrete example would be network packets encoding,
      where encoding packet by packet as you receive them may be more efficient
      than waiting for N packets then encode them all at once.

      {1 Example}
      {[
open Reed_solomon_erasure

let () =
  let r = ReedSolomon.make 3 2 in

  let sbs = ShardByShard.make r in

  let shards = [|Bytes.of_string "\000\001\002\003\004";
                 Bytes.of_string "\005\006\007\008\009";
                 (* say we don't have the 3rd data shard yet
                    and we want to fill it in later *)
                 Bytes.of_string "\000\000\000\000\000";
                 Bytes.of_string "\000\000\000\000\000";
                 Bytes.of_string "\000\000\000\000\000"|] in

  (* encode 1st and 2nd data shard *)
  ShardByShard.encode_bytes sbs shards;
  ShardByShard.encode_bytes sbs shards;

  (* fill in 3rd data shard *)
  Bytes.set shards.(2) 0 '\010';
  Bytes.set shards.(2) 1 '\011';
  Bytes.set shards.(2) 2 '\012';
  Bytes.set shards.(2) 3 '\013';
  Bytes.set shards.(2) 4 '\014';

  (* now do the encoding*)
  ShardByShard.encode_bytes sbs shards;

  (* above is equivalent to doing ReedSolomon.encode_bytes shards *)

  assert (ReedSolomon.verify_bytes r shards)
      ]}

      {1 Ownership of shards}
      The shard by shard codec is just a wrapper around the core codec type [reed_solomon],
      so all potential shard ownership issues in functions provided by {! ReedSolomon} are
      carried over.

      See {! ReedSolomon}, {b Ownership of shards} section for details.
  *)

  val make : reed_solomon -> shard_by_shard
  (** [make r]

      Makes a new instance of the bookkeeping type.
      Note that the codec is {b NOT} thread-safe.

      The original Reed-Solomon codec is still usable - [shard_by_shard]
      codec does not require exclusive ownership of the Reed-Solomon codec.
  *)

  val parity_ready : shard_by_shard -> bool
  (** [parity_ready sbs]
  *)

  val reset : shard_by_shard -> unit
  (** [reset sbs]

      Resets the bookkeeping data.

      You should call this when you have added and encoded all data shards,
      and have finished using the parity shards.

      Raises {! RS_SBS_Error.Exn.LeftoverShards}
  *)

  val reset_no_exn : shard_by_shard -> (unit, RS_SBS_Error.t) result
  (** [reset sbs]

      Error returning variant of {! reset}.
  *)

  val reset_force : shard_by_shard -> unit
  (** [reset_force sbs]

      Resets the bookkeeping data without checking.
  *)

  val cur_input_index : shard_by_shard -> int
  (** [cur_input_index sbs]

      Returns the current input shard index.
  *)

  val encode_bytes : shard_by_shard -> bytes array -> unit
  (** [encode_bytes sbs shards]

      See {! encode_bigstr}.
  *)

  val encode_sep_bytes : shard_by_shard -> bytes array -> bytes array -> unit
  (** [encode_sep_bytes sbs data parity]

      See {! encode_sep_bigstr}.
  *)

  val encode_str : shard_by_shard -> string array -> unit
  (** [encode_str sbs shards]

      See {! encode_bigstr}.
  *)

  val encode_sep_str : shard_by_shard -> string array -> string array -> unit
  (** [encode_sep_str sbs data parity]

      See {! encode_sep_bigstr}.
  *)

  val encode_bigstr : shard_by_shard -> bigstring array -> unit
  (** [encode_bigstr sbs shards]

      Constructs the parity shards partially using the current input data shard.

      Raises {! RS_SBS_Error.Exn.TooManyCalls} when all input shards have already
      been filled in via any of the [encode] functions.
  *)

  val encode_sep_bigstr : shard_by_shard -> bigstring array -> bigstring array -> unit
  (** [encode_sep_bigstr sbs data parity]

      Constructs the parity shards partially using the current input data shard.

      Raises {! RS_SBS_Error.Exn.TooManyCalls} when all input shards have already
      been filled in via any of the encode functions.
  *)

  val encode_bytes_no_exn : shard_by_shard -> bytes array -> (unit, RS_SBS_Error.t) result
  (** [encode_bytes_no_exn sbs shards]

      Error returning variant of {! encode_bytes}.
  *)

  val encode_sep_bytes_no_exn : shard_by_shard -> bytes array -> bytes array -> (unit, RS_SBS_Error.t) result
  (** [encode_sep_bytes_no_exn sbs data parity]

      Error returning variant of {! encode_sep_bytes}.
  *)

  val encode_str_no_exn : shard_by_shard -> string array -> (unit, RS_SBS_Error.t) result
  (** [encode_str_no_exn sbs shards]

      Error returning variant of {! encode_str}.
  *)

  val encode_sep_str_no_exn : shard_by_shard -> string array -> string array -> (unit, RS_SBS_Error.t) result
  (** [encode_sep_str_no_exn sbs data parity]

      Error returning variant of {! encode_sep_str}.
  *)

  val encode_bigstr_no_exn : shard_by_shard -> bigstring array -> (unit, RS_SBS_Error.t) result
  (** [encode_bigstr_no_exn sbs shards]

      Error returning variant of {! encode_bigstr}.
  *)

  val encode_sep_bigstr_no_exn : shard_by_shard -> bigstring array -> bigstring array -> (unit, RS_SBS_Error.t) result
  (** [encode_sep_bigstr_no_exn sbs data parity]

      Error returning variant of {! encode_sep_bigstr}.
  *)
end

module RS_Shard_utils : sig
  val option_shards_to_shards_bytes : bytes option array -> bytes array
  (** [option_shards_to_shards_bytes shards]

      See {! option_shards_to_shards_bigstr}.
  *)

  val shards_to_option_shards_bytes : bytes array -> bytes option array
  (** [shards_to_option_shards_bytes shards]

      See {! shards_to_option_shards_bigstr}.
  *)

  val option_shards_to_shards_str : string option array -> string array
  (** [option_shards_to_shards_str shards]

      See {! option_shards_to_shards_bigstr}.
  *)

  val shards_to_option_shards_str : string array -> string option array
  (** [shards_to_option_shards_str shards]

      See {! shards_to_option_shards_bigstr}.
  *)

  val option_shards_to_shards_bigstr : bigstring option array -> bigstring array
  (** [option_shards_to_shards_bigstr shards]

      Transforms an array of shards into an array of option shards.

      This is mainly useful when you want to repair an array of shards using
      [reconstruct] functions.

      {3 Remarks}
      Shards are copied - the returned array of shards do not share ownership
      with provided shards.
  *)

  val shards_to_option_shards_bigstr : bigstring array -> bigstring option array
  (** [shards_to_option_shards_bigstr shards]

      Transforms array of shards to array of option shards.

      This is mainly useful when you want to repair an array of shards using
      [reconstruct] functions.

      {3 Remarks}
      Shards are copied - the returned array of shards do not share ownership
      with provided shards.
  *)

  val make_blank_shards_str : count:int -> size_per_shard:int -> string array
  (** [make_blank_shards_str ~count ~size_per_shard]

      Creates an [string array] of size [count], each string is filled with zeros and of
      size [size_per_shard].
  *)

  val make_blank_shards_bytes : count:int -> size_per_shard:int -> bytes array
  (** [make_blank_shards_bytes ~count ~size_per_shard]

      Creates an [bytes array] of size [count], each bytes is filled with zeros and of
      size [size_per_shard].
  *)

  val make_blank_shards_bigstr : count:int -> size_per_shard:int -> bigstring array
  (** [make_blank_shards_bigstr ~count ~size_per_shard]

      Creates an [bigstring array] of size [count], each bigstring is filled with zeros and of
      size [size_per_shard].
  *)

  val copy_bigstr : bigstring -> bigstring
  (**
  *)

  val copy_shards_bytes : bytes array -> bytes array
  (**
  *)

  val copy_shards_str : string array -> string array
  (**
  *)

  val copy_shards_bigstr : bigstring array -> bigstring array
  (**
  *)
end
