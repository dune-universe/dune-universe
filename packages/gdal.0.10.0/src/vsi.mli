(** {1 Virtual System Interface} *)

(** Virtual file system management *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

exception VSI_error

val of_buffer : string -> bigstring -> unit
(** [of_buffer path buf] create a new virtual file at [path] with the bytes
    from [buf].

    A reference to [buf] is kept internally until {!unlink} is called.

    @raise VSI_error if there is an error. *)

val unlink : string -> unit
(** [unlink path] removes [path] from the virtual filesystem.  If any
    references are kept to external resources they are also released. *)
