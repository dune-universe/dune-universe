
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

(** {1 Interface to 1-dimension Bigarrays of bytes (char)}

    A "bigstring" here is simply a bigarray of chars. It can be used instead
    of regular strings when IO involve calling C (or another language),
    when very large strings are required, or for memory-mapping. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t
(** Create a new bigstring of the given size. Content is arbitrary. *)

val make : int -> char -> t
(** Create a new bigstring of the given size filled with [c]. *)

val empty : t
(** Empty string *)

val init : int -> (int -> char) -> t
(** Initialize with the given function (called at every index).
    [init n f] is the string [f 0, f 1, ..., f (n-1)]. *)

val fill : t -> char -> unit
(** Fill the string with the given byte. *)

val fill_slice : t -> char -> int -> int -> unit
(** [fill_slice s c i len] is the same as [fill (sub s i len) c], it
    fills the slice from [i] to [i+len-1] of [s] with the char [c] *)

val size : t -> int
(** Number of bytes *)

val length : t -> int
(** Alias for {!size}. *)

val get : t -> int -> char
(** Obtain the byte at the given index.
    @raise Invalid_argument if the index is invalid *)

val unsafe_get : t -> int -> char
(** Same as {!get}, but without bound check. Can fail arbitrarily (including
    segfault) if used improperly. *)

val set : t -> int -> char -> unit
(** Change the byte at the given index.
    @raise Invalid_argument if the index is invalid *)

val unsafe_set : t -> int -> char -> unit
(** Same as {!set}, but without bound check. Can fail arbitrarily (including
    segfault) if used improperly. *)

val blit : t -> int -> t -> int -> int -> unit
(** Blit a slice of the bigstring into another.
    [blit s1 i1 s2 i2 len] means that elements from [s1] whose indices
    range from [i1] to [i1+len-1] are copied into the slots of [s2]
    whose indices range from [i2] to [i2+len-1]. This is similar to
    {!String.blit} or {!Bytes.blit} or {!Array.blit}. *)

val copy : t -> t
(** Copy of the string *)

val sub : t -> int -> int -> t
(** [sub s i len] takes a slice of length [len] from the string [s], starting
    at offset [i]. The slice shares the same memory as [s], meaning that
    modifications of the slice will modify [s] as well.
    Slicing is cheap since it does not involve copying  the whole range.
    @raise Invalid_argument if [i, len] doesn't designate a valid substring *)

val fold : ('a -> char -> 'a) -> 'a -> t -> 'a
(** Fold on every char in increasing order *)

val foldi : ('a -> int -> char -> 'a) -> 'a -> t -> 'a
(** Fold on every char in increasing order *)

val iter : (char -> unit) -> t -> unit
(** Iterate on every char in increasing order *)

val iteri : (int -> char -> unit) -> t -> unit
(** Iterate on every char in increasing order *)

val equal : t -> t -> bool
(** Equality of content. *)

val compare : t -> t -> int
(** Lexicographic order *)

(** {2 Conversions} *)

val to_bytes : t -> Bytes.t

val of_bytes : Bytes.t -> t

val of_bytes_slice : Bytes.t -> int -> int -> t

val sub_bytes : t -> int -> int -> Bytes.t

val blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit

val blit_of_bytes : Bytes.t -> int -> t -> int -> int -> unit

val blit_of_buffer : Buffer.t -> int -> t -> int -> int -> unit

val to_string : t -> string

val of_string : string -> t

val of_string_slice : string -> int -> int -> t

val of_buffer : Buffer.t -> t
(** [of_buffer b] creates a string that contains the same as [b] *)

val of_gen : char gen -> t

val sub_string : t -> int -> int -> string

val blit_of_string : string -> int -> t -> int -> int -> unit

val to_seq : t -> char sequence

val to_gen : t -> char gen

val to_seq_slice : t -> int -> int -> char sequence

val to_gen_slice : t -> int -> int -> char gen

val to_buffer : t -> Buffer.t -> unit
(** Add the whole string at the end of the buffer *)

val print : t printer
(** Pretty-print the string into a formatter, surrounded with '"' *)

(** {2 Utils} *)

val concat : string -> t list -> t
(** [concat set l] concatenates the list [l], inserting [sep] between
    each pair of string. *)

val map : f:(char -> char) -> t -> t

val mapi : f:(int -> char -> char) -> t -> t

val lowercase : t -> t
(** Copy of the string with all characters in lowercase (see {!Char.lowercase}) *)

val uppercase : t -> t
(** Copy of the string with all characters in uppercase (see {!Char.uppercase}) *)

val trim : t -> t
(** [trim s] returns a slice of [s] without the leading and trailing
    whitespaces, where whitespaces are defined identically to {!String.trim}.
    note that it does not copy the substring, but returns a slice!
    @return a slice of [s], or empty if [s] is totally composed of whitespaces *)

val index : t -> c:char -> int
(** [index s ~c] returns the index of the first
    occurrence of character [c] in string [s].
    @raise Not_found if [c] does not occurr in [s] *)

val rindex : t -> c:char -> int
(** [rindex s ~c] returns the index of the last
    occurrence of character [c] in string [s].
    @raise Not_found if [c] does not occurr in [s] *)

val index_pred : f:(char -> bool) -> t -> int
(** [index_pred ~f s] returns the index of the first char in [s] that
    satisfies [s].
    @raise Not_found if no character in [s] satisfies [p] *)

val rindex_pred : f:(char -> bool) -> t -> int
(** [rindex_pred ~f s] returns the index of the last char in [s] that
    satisfies [s].
    @raise Not_found if no character in [s] satisfies [p] *)

val contains : t -> c:char -> bool
(** [String.contains s c] tests if character [c] appears in the string [s]. *)

val for_all : f:(char -> bool) -> t -> bool
(** True for all chars? *)

val exists : f:(char -> bool) -> t -> bool
(** True for some char? *)

val split : by:char -> t -> t list
(** [split s ~by] splits [s] along the occurrences of [by]. *)

val split_gen : by:char -> t -> t gen
(** Same as {!split} but returns a generator *)

val lines : t -> t list
(** [lines s] returns a list of the lines of [s] (splits along '\n') *)

val lines_gen : t -> t gen
(** [lines_gen s] returns a generator of the lines of [s] (splits along '\n')
    where every line is a slice of [s] *)
