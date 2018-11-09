(** The type of ISO-8859. *)
type encoding =
  [ `ISO_8859_1
  | `ISO_8859_2
  | `ISO_8859_3
  | `ISO_8859_4
  | `ISO_8859_5
  | `ISO_8859_6
  | `ISO_8859_7
  | `ISO_8859_8
  | `ISO_8859_9
  | `ISO_8859_10
  | `ISO_8859_11
  | `ISO_8859_13
  | `ISO_8859_14
  | `ISO_8859_15
  | `ISO_8859_16 ]

val encoding_of_string : string -> encoding
(** [encoding_of_string s] converts a (case insensitive)
   {{:http://www.iana.org/assignments/character-sets}IANA character set name} to
   an encoding. *)

val encoding_to_string : encoding -> string
(** [encoding_to_string e] is a
   {{:http://www.iana.org/assignments/character-sets}IANA character set name}
   for [e]. *)

(** The type for decoders. *)
type 'kind decoder constraint 'kind = [< encoding]

(** The type for input sources. With a [`Manual] source the client must provide
   input with {!src}. *)
type src = [`Manual | `Channel of in_channel | `String of string]

type decode = [`Await | `End | `Uchar of Uchar.t | `Malformed of string]

val pp_decode : Format.formatter -> [< decode] -> unit

val src : 'kind decoder -> Bytes.t -> int -> int -> unit
(** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
   This byte range is read by calls to {!decode} with [d] until [`Await] is
   returned. To signal the end of input call the function with [l = 0]. *)

val decoder : ([< encoding] as 'kind) -> src -> 'kind decoder
(** [decoder encoding src] is a decoder that inputs from [src].

    {b Encoding.} [encoding] specifies the decoded encoding scheme. *)

val decode : 'kind decoder -> decode
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits
       for more input. The client must use {!src} to provide it.}
    {- [`Uchar u] if a Unicode scalar value [u] was decoder.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed err] if [d] encountered an error [err].}}

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors. *)

val decoder_byte_count : 'kind decoder -> int
(** [decoder_byte_count d] is the number of characters already decoder on [d]
   (including malformed ones). This is the last {!decode}'s and byte offset
   counting from beginning of the stream. *)

val decoder_src : 'kind decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_kind : 'kind decoder -> 'kind
(** [decoder_kind d] is [d]'s the decoded encoding scheme of [d]. *)

module Char : sig
  val is_valid : encoding -> char -> bool
  (** [is_valid e n] is [true] iff [n] is an [e] scalar value. *)

  val equal : encoding -> char -> char -> bool
  (** [equal e u u'] is [u = u']. *)

  val compare : encoding -> char -> char -> int
  (** [compare u u'] is [Pervasives.compare u u']. *)

  val unicode : encoding -> char -> Uchar.t
  (** [unicode e c] is the decoded Unicode of encoding character [c] in encoding
     scheme [e]. *)
end

module String : sig
  (** The type for character folder. The integer is the index in the string
     where the [`Uchar] or [`Malformed] starts. *)
  type 'a folder =
    'a -> int -> [`Malformed of string | `Uchar of Uchar.t] -> 'a

  val fold :
    encoding -> ?off:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
  (** [fold e ?off ?len f a s] is [f (] ... [(f (f a pos u]{_0}[) j]{_1}[
     u]{_1}[)] ... [)] ... [) j]{_n}[ u]{_n} where [u]{_i}, [j]{_i} are
     characters and their start position in the [e] encoded substring [s]
     starting at [pos] and [len] long. The default value for [pos] is [0] and
     [len] is [ String.length s - pos]. *)
end
