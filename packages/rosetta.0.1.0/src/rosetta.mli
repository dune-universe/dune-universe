type encoding = [ `UTF_7 | Uuuu.encoding | Coin.encoding ]
(** The type of encoding. *)

val encoding_of_string: string -> encoding
(** [encoding_of_string s] converts a (case insensitive)
   {{:http://www.iana.org/assignments/character-sets}IANA character set name} to
   an encoding. *)

val encoding_to_string: encoding -> string
(** [encoding_to_string e] is a
   {{:http://www.iana.org/assignments/character-sets}IANA character set name}
   for [e]. *)

type 'kind decoder constraint 'kind = [< encoding ]
(** The type for decoders. *)

type src = [ `Manual | `Channel of in_channel | `String of string ]
(** The type for input sources. With a [`Manual] source the client must provide
   input with {!src}. *)

type decode = [ `Await | `End | `Uchar of Uchar.t | `Malformed of string ]

val src: encoding decoder -> Bytes.t -> int -> int -> unit
(** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
   This byte range is read by calls to {!decode} with [d] until [`Await] is
   returned. To signal the end of input call the function with [l = 0]. *)

val decoder: encoding -> src -> encoding decoder
(** [decoder encoding src] is a decoder that inputs from [src].

    {b Encoding.} [encoding] specifies the decoded encoding scheme. *)

val decode: encoding decoder -> decode
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits
       for more input. The client must use {!src} to provide it.}
    {- [`Uchar u] if a Unicode scalar value [u] was decoder.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed err] if [d] encountered an error [err].}}

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors. *)

val decoder_byte_count: encoding decoder -> int
(** [decoder_byte_count d] is the number of characters already decoder on [d]
   (including malformed ones). This is the last {!decode}'s and byte offset
   counting from beginning of the stream. *)

val decoder_src: encoding decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_kind: encoding decoder -> encoding
(** [decoder_kind d] is [d]'s the decoded encoding scheme of [d]. *)

module String: sig
  type 'a folder = 'a -> int -> [ `Malformed of string | `Uchar of Uchar.t ] -> 'a
  (** The type for character folder. The integer is the index in the string
     where the [`Uchar] or [`Malformed] starts. *)

  val fold: encoding -> ?off:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
  (** [fold e ?off ?len f a s] is [f (] ... [(f (f a pos u]{_0}[) j]{_1}[
     u]{_1}[)] ... [)] ... [) j]{_n}[ u]{_n} where [u]{_i}, [j]{_i} are
     characters and their start position in the [e] encoded substring [s]
     starting at [pos] and [len] long. The default value for [pos] is [0] and
     [len] is [ String.length s - pos]. *)
end
