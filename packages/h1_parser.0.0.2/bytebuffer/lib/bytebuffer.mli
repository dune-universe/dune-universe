(** Extensible buffers using bigstrings. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t

module View : sig
  type t = private {
    buffer : bigstring;
    pos : int;
    len : int;
    continue : int -> unit;
  }
end

val create : int -> t
(** [create size] returns a new empty bytebuffer. This function will allocate a
    new bigstring of size [size]. The bytebuffer will be resized automatically
    if attempting to add more than [size] characters to the bytebuffer. *)

val of_bigstring : bigstring -> t
(** [of_bigstring] creates a new bytebuffer and uses the user provided bigstring
    as the backing store. *)

val contents : t -> bigstring
(** [contents] returns a copy of the current contents of the bytebuffer. *)

val contents_string : t -> string
(** [contents_string] is the same as [contents] except it returns the copy as a
    [string]. *)

val length : t -> int
(** [length] returns the number of characters in the bytebuffer. *)

val capacity : t -> int
(** [capacity] is the size of the underlying bigstring. *)

val clear : t -> unit
(** [clear] empties the bytebuffer. *)

val reset : t -> unit
(** [reset] clears the bytebuffer and resets the underlying bigstring to the
    initial bigstring used during [create]. *)

val add_char : t -> char -> unit
(** [add_char] appends a charater at the end of the bytebuffer. *)

val add_string : t -> string -> unit
(** [add_string] appends a string at the end of the bytebuffer. *)

val add_bigstring : t -> ?pos:int -> ?len:int -> bigstring -> unit
(** [add_bigstring] appends a bigstring at the end of the bytebuffer. *)

val fill : t -> View.t
(** [fill] returns a view into the buffer's underlying bigstring that can be
    used to blit bytes directly into the buffer. This can be useful to directly
    read content into the buffer. *)

val consume : t -> View.t
(** [consume] returns a view into the buffer's underlying bigstring that can be
    used to blit bytes from the buffer. This can be useful to consume bytes from
    the buffer and blit them directly for I/O operations. *)

val addf : t -> ('a, Format.formatter, unit, unit) format4 -> 'a

val index : ?pos:int -> ?len:int -> char -> t -> int option
(** [index] returns the index of the character if it exists within the buffer. *)

val unsafe_index : ?pos:int -> ?len:int -> char -> t -> int
(** [unsafe_index] is similar to [index] but instead of options it returns a
    negative integer if the character isn't present in the buffer. *)

val drop : t -> int -> unit
(** [drop n] deletes the first [n] bytes from the buffer. *)
