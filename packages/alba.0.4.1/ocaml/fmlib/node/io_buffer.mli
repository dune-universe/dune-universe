open Fmlib.Module_types

(**
   An IO buffer is an array of bytes
{v
   b0 b1 b2 ...   br  ...   bw ...  bn-1
                  ^         ^            ^
                  rp        wp           n

   rp: read pointer
   wp: write pointer
   n:  size of the buffer

   Invariant: 0 <= rp <= wp <= n

   rp = wp:  no more to read
   wp = n:   buffer is full
v}
 *)



(** The type of the buffer. *)
type t

(** [alloc size] allocates an io buffer of [size] and set its read pointer and
   write pointer to 0. *)
val alloc: int -> t

(** [getc b] reads the character at the read pointer of the buffer [b],
   advances the read pointer and returns the character. If the read pointer is
   at the position of the write pointer, then [None] is retured. *)
val getc: t -> char option

(** [putc b ch] appends the character [c] at the position of the write pointer
   and advances the write pointer. If the write pointer is already at the end
   [None] is returned. *)
val putc: t -> char -> unit option


(** The javascript buffer object. *)
type js_buffer

(** The javascript buffer object. *)
val js_buffer: t -> js_buffer

(** The capacity of the buffer. *)
val capacity: t -> int

(** The length of the content of the buffer. *)
val length: t -> int

(** Position of the next character to read. *)
val read_pointer: t -> int

(** Position of the next character to write. *)
val write_pointer: t -> int

(** Is the buffer empty? [read_pointer = write_pointer]*)
val is_empty: t -> bool

(** Is the buffer full? *)
val is_full: t -> bool

(** Set the read and the write pointer to zero. *)
val reset: t -> unit

(** Set the read pointer. *)
val set_read_pointer: t -> int -> unit

(** Set the write pointer. *)
val set_write_pointer: t -> int -> unit

(** [copy src s0 s1 dst d0] copies the data from the buffer [src] between
   position [s0] and [s1] (excluding [s1] to the buffer [dst] starting at
   position [d0]. Overlapping is handled properly. *)
val copy: t -> int -> int -> t -> int -> unit


module Read (W:WRITABLE):
sig
  val read: t -> W.t -> W.t
end

module Write (R:READABLE):
sig
  val write: t -> R.t -> R.t
end
