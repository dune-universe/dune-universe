(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. %%NAME%% %%VERSLwtN%%
  ---------------------------------------------------------------------------*)

(** REdis Serialization Protocol library for OCaml

    {e %%VERSLwtN%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Resp} *)

type t =
  [ `Nil
  | `Integer of int64
  | `String of string
  | `Error of string
  | `Bulk of string
  | `Array of t array ]

type lexeme =
  [ `Nil
  | `Integer of int64
  | `String of string
  | `Error of string
  | `Bs of int
  | `As of int ]

type error =
  [ `Msg of string
  | `Unexpected of char
  | `Invalid_value
  | `Invalid_encoder ]

val pp_error : Format.formatter -> error -> unit
val string_of_error : error -> string
val unwrap : ('a, error) result -> 'a

exception Exc of error

module type INPUT = sig
  type ic

  val read : ic -> int -> string Lwt.t
  val read_line : ic -> string Lwt.t
  val read_char : ic -> char Lwt.t
end

module type OUTPUT = sig
  type oc

  val write : oc -> string -> unit Lwt.t
end

module type READER = sig
  include INPUT

  val discard_sep : ic -> unit Lwt.t
  val read_lexeme : ic -> (lexeme, error) result Lwt.t
  val decode : ic -> lexeme -> t Lwt.t
end

module type WRITER = sig
  include OUTPUT

  val write_sep : oc -> unit Lwt.t
  val write_lexeme : oc -> lexeme -> unit Lwt.t
  val encode : oc -> t -> unit Lwt.t
end

module type S = sig
  module Reader : READER
  module Writer : WRITER

  val write : Writer.oc -> t -> unit Lwt.t
  val read : Reader.ic -> t Lwt.t
end

module Reader (I : INPUT) : READER with type ic = I.ic
module Writer (O : OUTPUT) : WRITER with type oc = O.oc

module Make (Reader : READER) (Writer : WRITER) :
  S with module Reader = Reader and module Writer = Writer

module String_reader : READER with type ic = string ref
module String_writer : WRITER with type oc = string ref

module String :
  S with module Reader = String_reader and module Writer = String_writer

val is_nil : t -> bool
val to_string : t -> (string, error) result
val to_string_exn : t -> string
val to_integer : t -> (int64, error) result
val to_integer_exn : t -> int64
val to_float : t -> (float, error) result
val to_float_exn : t -> float
val to_array : (t -> 'b) -> t -> ('b array, error) result
val to_array_exn : (t -> 'b) -> t -> 'b array
val of_alist : (string * t) list -> t
val to_alist : (t -> 'k) -> (t -> 'v) -> t -> (('k * 'v) list, error) result
val to_alist_exn : (t -> 'k) -> (t -> 'v) -> t -> ('k * 'v) list
(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTLwtN OF CONTRACT, NEGLIGENCE
  OR OTHER TORTLwtUS ACTLwtN, ARISING OUT OF OR IN CONNECTLwtN WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
