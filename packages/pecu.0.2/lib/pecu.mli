(** {1:decode Decode} *)

type decoder
(** The types for decoders. *)

type src = [ `Manual | `Channel of in_channel | `String of string ]
(** The type for input sources. With a [`Manual] source the client must provide
   input with {!src}. *)

type decode = [ `Await | `End | `Data of string | `Line of string | `Malformed of string ]

val src: decoder -> Bytes.t -> int -> int -> unit
(** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
   This byte range is read by calls to {!decode} with [d] until [`Await] is
   returned. To signal the end of input call the function [l = 0]. *)

val decoder: src -> decoder
(** [decoder src] is a decoder that inputs from [src]. *)

val decode: decoder -> decode
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits
       for more input. The client must use {!src} to provide it.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed bytes] if the [bytes] sequence is malformed according to the
       decoded quoted-printable encoding scheme. If you are interested in a
       best-effort decoding you can still continue to decode after an error
       until the decode synchronizes again on valid bytes.}
    {- [`Data data] if a [data] sequence value was decoded.}
    {- [`Line line] if a [line sequence value plus a line-break was decoded.]}}

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors. *)

val decoder_byte_count: decoder -> int
(** [decoder_byte_count d] is the number of characters already decoded on [d]
   (inclueded malformed ones). This is the last {!decode}'s end output offset
   counting from beginning of the stream. *)

val decoder_src: decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_dangerous: decoder -> bool
(** [decoder_dangerous d] returns [true] if encoded input does not respect the
   80-columns rule. In this case, internal buffers can grow automatically. If
   you are interested in a best-effort decoding you can still continue to decode
   even if [decoder_dangerous d] returns [true]. However, it could be an attack
   entry point in a server-context. *)

(** {!:encode Encode} *)

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
(** The type for output destinations. With a [`Manual] destination the client
   must provide output storage with {!dst}. *)

type encode = [ `Await | `End | `Char of char | `Line_break ]

type encoder
(** The type for Quoted-Printable encoder. *)

val encoder: dst -> encoder
(** [encoder dst] is an encoder for quoted-printable that outputs to [dst]. *)

val encode: encoder -> encode -> [ `Ok | `Partial ]
(** [encode e v]: is
    {ul
    {- [`Partial] iff [e] has a [`Manual] destination and needs more output
       storage. The client must use {!dst} to provide a new buffer
       and then call {!encode} with [`Await] until [`Ok] is returned.}
    {- [`Ok] when the encoder is ready to encode a new [`Char], [`Line_break]
       or [`End]}}

    For [`Manual] destination, encoding [`End] always return
    [`Partial], the client should continue as usual with [`Await]
    until [`Ok] is returned at which point {!dst_rem} [encoder] is
    guaranteed to be the sode of the last provided buffer (i.e. nothing
    was written).

    {b Raises.} [Invalid_argument] if a [`Char], [`Line_break] or [`End]
    is encoded after a [`Partial] encode. *)

val encoder_dst: encoder -> dst
(** [encoder_dst encoder] is [encoder]'s output destination. *)

val dst: encoder -> Bytes.t -> int -> int -> unit
(** [dst e s j l] provides [e] with [l] bytes to write, starting
    at [j] in [s]. This byte range is written by calls to {!encode} with [e]
    until [`Partial] is returned. Use {!dst_rem} to know the remaining
    number of non-written free bytes in [s]. *)

val dst_rem: encoder -> int
(** [dst_rem e] is the remaining number of non-written, free bytes in the last
   buffer provided with {!dst}. *)
