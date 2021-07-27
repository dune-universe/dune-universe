(** Pecu, decoder/encoder of quoted-printable flow.

    A quoted-printable flow, is a rich text encoding used by emails to safely
   transmit a text even if it use some bytes which can not be encoded into 7 bits.
    It is described by RFC2045 as a possible {i Content-Transfer-Encoding} of the
   email's contents. This library wants to provide a non-blocking encoder/decoder
    to this such format. {i quoted-printable} does not imply any encoding. A
   {i latin1}i can be used or {i UTF-8}. This such information is available into
   the {i Content-type} value. *)

(** {2:decode Decode} *)

(** The type for decoders. *)
type decoder

(** The type for input sources. With a [`Manual] source the client must provide
    input with {!src}. *)
type src = [`Manual | `Channel of in_channel | `String of string]

type decode =
  [`Await | `End | `Data of string | `Line of string | `Malformed of string]

val src : decoder -> Bytes.t -> int -> int -> unit
(** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
    This byte range is read by calls to {!decode} with [d] until [`Await] is
    returned. To signal the end of input, call the function with [l = 0]. *)

val decoder : src -> decoder
(** [decoder src] is a decoder that inputs from [src]. *)

val decode : decoder -> decode
(** [decode d] is:

    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits for more input.
   The client must use {!src} to provide it.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed bytes] if the [bytes] sequence is malformed according to the
   decoded quoted-printable encoding scheme. If you are interested in a
   best-effort decoding you can still continue to decode after an error until
   the decode synchronizes again on valid bytes.}
    {- [`Data data] if a [data] sequence value was decoded.}
    {- [`Line line] if a [line sequence value plus a line-break was decoded.]}}

    {b Note.} Repeated invocation always eventually returns [`End], even in case
   of errors. *)

val decoder_byte_count : decoder -> int
(** [decoder_byte_count d] is the number of characters already decoded on [d]
    (included malformed ones). This is the last {!decode}'s end output offset
    counting from beginning of the stream. *)

val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_dangerous : decoder -> bool
(** [decoder_dangerous d] returns [true] if encoded input does not respect the
    80-columns rule. In this case, internal buffers can grow automatically. If
    you are interested in a best-effort decoding you can still continue to
    decode even if [decoder_dangerous d] returns [true]. However, it could be
    an attack entry point in a server-context. *)

(** {2:encode Encode} *)

(** The type for output destinations. With a [`Manual] destination the client
    must provide output storage with {!dst}. *)
type dst = [`Channel of out_channel | `Buffer of Buffer.t | `Manual]

type encode = [`Await | `End | `Char of char | `Line_break]

(** The type for Quoted-Printable encoder. *)
type encoder

val encoder : dst -> encoder
(** [encoder dst] is an encoder for quoted-printable that outputs to [dst]. *)

val encode : encoder -> encode -> [`Ok | `Partial]
(** [encode e v]: is

    {ul
    {- [`Partial] iff [e] has a [`Manual] destination and needs more output
   storage. The client must use {!dst} to provide a new buffer and then call
   {!encode} with [`Await] until [`Ok] is returned.}
    {- [`Ok] when the encoder is ready to encode a new [`Char], [`Line_break] or
   [`End]}}

    For [`Manual] destination, encoding [`End] always return [`Partial], the
   client should continue as usual with [`Await] until [`Ok] is returned at
   which point {!dst_rem} [encoder] is guaranteed to be the sode of the last
   provided buffer (i.e. nothing was written).

    {b Raises.} [Invalid_argument] if a [`Char], [`Line_break] or [`End] is
   encoded after a [`Partial] encode. *)

val encoder_dst : encoder -> dst
(** [encoder_dst encoder] is [encoder]'s output destination. *)

val dst : encoder -> Bytes.t -> int -> int -> unit
(** [dst e s j l] provides [e] with [l] bytes to write, starting at [j] in [s].
    This byte range is written by calls to {!encode} with [e] until [`Partial]
    is returned. Use {!dst_rem} to know the remaining number of non-written
    free bytes in [s]. *)

val dst_rem : encoder -> int
(** [dst_rem e] is the remaining number of non-written, free bytes in the last
    buffer provided with {!dst}. *)

module Inline : sig
  (** {3:decode-inline Decode inline quoted-printable value.}

      The purpose of this sub-module is to decode/encode {i inline}
     quoted-printable data described by RFC2047. Header of an email can
     not emit in anyway (before RFC6532) bytes encoded into 8 bits. To 
     be able to use an encoding such as {i latin1} or {i UTF-8}, we can
     use an {i inlined} quoted-printable text. *)

  (** The type for decoders. *)
  type decoder

  type decode = [`Await | `End | `Char of char | `Malformed of string]

  val src : decoder -> Bytes.t -> int -> int -> unit
  (** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in
      [s]. This byte range is read by calls to {!decode} with [d] until
      [`Await] is returned. To signal the end of input, call the function with
      [l = 0]. *)

  val decoder : src -> decoder
  (** [decoder src] is a decoder that inputs from [src]. *)

  val decode : decoder -> decode
  (** [decode d] is:

      {ul
      {- [`Await] if [d] has a [`Manual] input source and awaits for more input.
     The client must use {!src} to provide it.}
      {- [`End] if the end of input was reached.}
      {- [`Malformed bytes] if the [bytes] sequence is malformed according to
     the decoded quoted-printable encoding scheme. If you are interested in a
     best-effort decoding you can still continue to decode after an error until
     the decode synchronizes again on valid bytes.}
      {- [`Data data] if a [data] sequence value was decoded.}
      {- [`Line line] if a [line sequence value plus a line-break was
     decoded.]}}

      {b Note.} Repeated invocation always eventually returns [`End], even in
     case of errors. *)

  val decoder_byte_count : decoder -> int
  (** [decoder_byte_count d] is the number of characters already decoded on [d]
      (included malformed ones). This is the last {!decode}'s end output offset
      counting from beginning of the stream. *)

  val decoder_src : decoder -> src
  (** [decoder_src d] is [d]'s input source. *)

  type encode = [`Await | `End | `Char of char]

  (** The type for encoders. *)
  type encoder

  val encoder : dst -> encoder
  (** [encoder dst] is an encoder that outputs to [dst]. *)

  val encode : encoder -> encode -> [`Ok | `Partial]
  (** [encode encoder] is:

      {ul
      {- [`Partial] iff [e] has a [`Manual] destination and needs more output
     storage. The client must use {!dst} to provide a new buffer and then call
     {!encode} with [`Await] until [`Ok] is returned.}
      {- [`Ok] when the encoder is ready to encode a new [`Char], [`Line_break] or
     [`End]}}

      For [`Manual] destination, encoding [`End] always return [`Partial], the
     client should continue as usual with [`Await] until [`Ok] is returned at
     which point {!dst_rem} [encoder] is guaranteed to be the sode of the last
     provided buffer (i.e. nothing was written).

      {b Raises.} [Invalid_argument] if a [`Char], [`Line_break] or [`End] is
     encoded after a [`Partial] encode. *)

  val encoder_dst : encoder -> dst
  (** [encoder_dst encoder] is [encoder]'s output destination. *)

  val dst : encoder -> Bytes.t -> int -> int -> unit
  (** [dst e s j l] provides [e] with [l] bytes to write, starting at [j] in [s].
     This byte range is written by calls to {!encode} with [e] until [`Partial]
     is returned. Use {!dst_rem} to know the remaining number of non-written
     free bytes in [s]. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is the remaining number of non-written, free bytes in the last
     buffer provided with {!dst}. *)
end


