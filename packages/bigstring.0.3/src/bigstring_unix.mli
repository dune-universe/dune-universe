
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** This is equivalent to [Bigstring.t]. It is redifined here to avoid depending
    on [Bigstring].
    @since 0.3 *)

(** {2 I/O} *)

(** These I/O functions are missing from the Bigarray library.
    They release the runtime during I/O. *)

val read  : Unix.file_descr -> ?off:int -> ?len:int -> t -> int
val write : Unix.file_descr -> ?off:int -> ?len:int -> t -> int


(** {2 Memory-map} *)

val with_map_file :
  ?pos:int64 -> ?len:int -> ?mode:int -> ?flags:open_flag list -> ?shared:bool ->
  string -> (t -> 'a) -> 'a
(** [with_map_file name f] maps the file into memory, opening it, and
    call [f] with a slice [pos.... pos+len] of the bytes of the file
    where [len] is the length of the file if not provided.
    When [f] returns, the file is closed.
    @param pos offset in the file (default 0)
    @param shared if true, modifications are shared between processes that
      have mapped this file (requires the filedescr to be open in write mode).
    @param mode the mode for the file, if it's created
    @param flags opening flags (default rdonly)
    see {!Bigarray.Array1.map_file} for more details *)

val map_file_descr : ?pos:int64 -> ?shared:bool -> Unix.file_descr -> int -> t
(** [map_file_descr descr len] is a lower-level access to an underlying file descriptor.
    @param shared if true, modifications are shared between processes that
    have mapped this file (requires the filedescr to be open in write mode).
    see {!Bigarray.Array1.map_file} for more details *)
