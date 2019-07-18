(** Unicode strings encoded according to the "Modified UTF-8" scheme used by Java and derivative systems. *)

(** The abstract type of MUTF8 strings. Internally these are a normal
   OCaml octet-string (containing the MUTF8 encoding) plus some
   auxiliary information about its contents. *)
type t

(** Returns a MUTF8 string equivalent to a given UTF8 string. O(n).
    May raise BatUTF8.Malformed_code *)
val of_utf8 : string -> t

(** Returns a UTF8 string equivalent to a given MUTF8 string. O(n),
   but O(1) in the common case in which the MUTF8 and UTF8 encodings
   are identical. *)
val to_utf8 : t -> string

(** Create a [MUTF8.t] from its byte representation. O(n)
    May raise BatUTF8.Malformed_code *)
val of_bytes : string -> t

(** Returns the MUTF8 encoded string value. O(1) *)
val to_bytes : t -> string

(** Creates a MUTF8 string from a sequence of UTF-16 values. Any
    sequence of UTF-16 values is valid. Numbers outside the range
    of 0 to 0xFFFF will raise BatUChar.Out_of_range. *)
val of_utf16_seq : int Seq.t -> t

(** Creates a MUTF8 string from a sequence of unichars. The sequence
    may contain high-plane characters and/or unpaired surrogates but
    must not contain paired surrogates: providing a sequence with
    paired surrogates will produce a malformed MUTF8 object. *)
val of_uchar_seq : BatUChar.t Seq.t -> t

(** Traverse a MUTF8 string as a sequence of UTF-16 values. To get Unicode characters, compose this
    with wobbly_to_ucs32 or strict_to_ucs32. *)
val to_utf16_seq : t -> int Seq.t
val to_utf16_enum : t -> int BatEnum.t

(** The number of UTF-16 characters required to represent this string *)
val utf16_length : t -> int

(** The number of Unicode codepoints required to represent this string *)
val unicode_length : t -> int

(** Lexicographically compare MUTF8 strings according to their UCS-16
   representations. Note that because of the special treatment of
   codepoint 0 in MUTF8, this is different from comparing the
   bytestring representations. *)
val compare : t -> t -> int

(** Traverse a UTF8, MUTF8, or CESU string as a sequence of integer
   values. For UTF8, these will be UCS32 code points; for CESU or
   MUTF8, these will be UTF16 values. The sequence is evaluated
   lazily, and may raise BatUTF8.Malformed_code if it reaches a byte
   sequence invalid in (M)UTF8. *)
val seq_of_utf8 : ?startbyte:int -> string -> int Seq.t

(** Convert a UTF-16 sequence to a UCS-32 sequence by combining
    surrogate pairs. Unpaired surrogates are passed through. Codepoints
    outside of the UTF-16 range are also passed through as UCS-32
    values.

    This does not return BatUChar.t because BatUChar rejects
    codepoints in the surrogate range. *)
val wobbly_to_ucs32 : int Seq.t -> int Seq.t

(** Convert a UTF-16 sequence to a UCS-32 sequence by combining
    surrogate pairs. Unpaired surrogates will cause this funtion to
    raise BatUChar.Out_of_range. Otherwise the same as wobbly_to_ucs32;
    in particular, integers greater than 0xFFFF are still
    passed through as UCS-32 values. *)
val strict_to_ucs32 : int Seq.t -> BatUChar.t Seq.t

val debugdump : t -> unit
