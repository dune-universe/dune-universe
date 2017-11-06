(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Mutable byte windows on top of Cstruct bigarrays *)

type t
(** Type for mutable windows on top of bigarrays, with bounds
    automatically updated when values are read/write in the buffer. *)

val create: int -> t
(** Create a new buffer. *)

val of_string: ?allocator:(int -> Cstruct.t) -> string -> t
(** Create a buffer from a string. *)

val to_string: t -> string
(** Create a string from a buffer. *)

val length: t -> int
(** Get the buffer length. *)

val offset: t -> int
(** Get the buffer offset. *)

val sub: t -> int -> int -> t
(** Return a sub-window of the given buffer window. *)

val clone: t -> t
(** Clone the window buffer (the actual contents is still shared). *)

val shift: t -> int -> unit
(** Shift the buffer window. Negative offsets are supported. *)

val index: t -> char -> int option
(** Return the offset corresponding to the first occurence of the
    character in the given string. *)

(** {2 Errors} *)

exception Parse_error of string
(** Parse error *)

val parse_error_buf: t -> ('a, unit, string, 'b) format4 -> 'a
(** Print an error message, the buffer contents and raise a exception *)

val parse_error: ('a, unit, string, 'b) format4 -> 'a
(** Print an error message and raise an exception *)

val hexdump: t -> unit
(** Same as [Cstruct.hexdump]. *)

val hexdump_to_buffer: Buffer.t -> t -> unit
(** Same as [Cstruct.hexdump_to_buffer] *)

val debug: t -> string
(** Same as [Cstruct.debug]. *)

(** {Basic IO operations} *)

(** Get/set big-endian integers of various sizes. *)

val get_char: t -> char
(** [get_char buf] return the character stored in [buf]. *)

val get_uint8: t -> int
(** [get_uint8 buf] is the 8 bit unsigned integer stored in [buf]. *)

val get_be_uint16: t -> int
(** [get_uint16 buf] is the 16 bit long big-endian unsigned integer
    stored in [buf]. *)

val get_be_uint32: t -> int32
(** [get_uint32 buf] is the 32 bit long big-endian unsigned integer
    stored in [buf]. *)

val get_be_uint64: t -> int64
(** [get_uint64 buf] is the 64 bit long big-endian unsigned integer
    stored in [buf]. *)

val get_string: t -> int -> string
(** [get_string buf len] is the string of size [len] stored in [buf]. *)

val pick_string: t -> int -> string option
(** [pick_string buf len] looks for the string of size [len] in the
    buffer, without consuming it. Return [None] if the buffer is
    bigger than [len]. *)

val get_delim: t -> char -> (t -> 'a) -> 'a option
(** [get_delim buf c fn] builds a subwindow of [buf] by looking at the
    first occurence of the character [c]. Once the window is built, apply
    [fn] on the resulting buffer. Return [None] if [c] does not appear in
    the current buffer. *)

val get_string_delim: t -> char -> string option
(** [get_string_delim buf c] returns the string appearing between the
    start of the buffer [buf] and ending at the first occurence of the
    character [c]. Return [None] if [c] does not appear in [buf]. *)

(** {2 Setters} *)

val set_char: t -> char -> unit
(** [set_char buf off c] write the character [c] in [buf] at offset
    [off]. *)

val set_uint8: t -> int -> unit
(** [set_uint8 buf] write the 8 bit long integer stored in [buf]. *)

val set_be_uint16: t -> int -> unit
(** [set_uint16 buf i] writes the 16 bit long big-endian unsigned
    integer [i] in [buf]. *)

val set_be_uint32: t -> int32 -> unit
(** [set_uint32 buf i] writes the 32 bit long big-endian unsigned
    integer [i] in [buf]. *)

val set_be_uint64: t -> int64 -> unit
(** [set_uint64 buf i] writes the 64 bit long big-endian unsigned
    integer [i] in [buf]. *)

val set_string: t -> string -> unit
(** [set_string buf str] write the string [str] into [buf]. *)

(** {2 Little endian} *)

val get_le_uint16: t -> int
(** [get_uint16 buf] is the 16 bit long little-endian unsigned integer
    stored in [buf]. *)

val get_le_uint32: t -> int32
(** [get_uint32 buf] is the 32 bit long little-endian unsigned integer
    stored in [buf]. *)

val get_le_uint64: t -> int64
(** [get_uint64 buf] is the 64 bit long little-endian unsigned integer
    stored in [buf]. *)

val set_le_uint16: t -> int -> unit
(** [set_uint16 buf i] writes the 16 bit long little-endian unsigned
    integer [i] in [buf]. *)

val set_le_uint32: t -> int32 -> unit
(** [set_uint32 buf i] writes the 32 bit long little-endian unsigned
    integer [i] in [buf]. *)

val set_le_uint64: t -> int64 -> unit
(** [set_uint64 buf i] writes the 64 bit long little-endian unsigned
    integer [i] in [buf]. *)

(** {2 Bigarrays} *)

val of_bigarray: ?off:int -> ?len:int -> Cstruct.buffer -> t
(** [of_bigarray ~off ~len b] is the mstruct contained in [b] starting
    at [off], of length [len]. *)

val to_bigarray: t -> Cstruct.buffer
(** Accessor. Return the underlying bigarray. *)

(** {2 Cstructs} *)

val of_cstruct: Cstruct.t -> t
(** Create mutable cstruct. *)

val to_cstruct: t -> Cstruct.t
(** Create an immutable cstruct. *)

val with_mstruct: Cstruct.t -> (t -> unit) -> unit
(** [with_mstruct c f] apply [f] to [of_cstruct c]. *)
