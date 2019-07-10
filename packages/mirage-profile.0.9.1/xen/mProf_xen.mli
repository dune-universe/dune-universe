(** Trace processes on Xen, keeping the results in a memory region
    that is shared with another domain. *)

type log_buffer =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external get_monotonic_time : unit -> int64 = "caml_get_monotonic_time"
val timestamper : EndianBigstring.bigstring -> int -> unit
val make_shared_buffer : size:int -> Io_page.t
val share_with : domid:int -> Io_page.t -> unit Lwt.t
