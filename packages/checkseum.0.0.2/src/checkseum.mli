type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type S =
sig
  type t = Optint.t
  (** Representation of the checksum value. *)

  val pp: Format.formatter -> t -> unit
  (** Pretty-printer of {!t}. *)

  val equal: t -> t -> bool
  (** The equal function of {!t}. *)

  val default: t
  (** Default value of {!t}. *)

  val digest_bytes: Bytes.t -> int -> int -> t -> t
  (** [digest_bytes msg off len t] is the digest of [msg] at [off] on [len]
     byte(s). *)

  val digest_string: String.t -> int -> int -> t -> t
  (** Same as {!digest_bytes} but for {!String.t}. *)

  val digest_bigstring: bigstring -> int -> int -> t -> t
  (** Same as {!digest_bigstring} but for {!bigstring}. *)
end

module Adler32: S
(** Implementation of the ADLER-32 cheksum. *)

module Crc32c: S
(** Implementation of the CRC32C checksum. *)
