open Apero

(** I/O related functions *)

val read : Lwt_unix.file_descr -> Abuf.t -> int -> int Lwt.t
(** [read fd buf len] reads at most [len] bytes out of the 
    file descriptior [fd] in to the buffer [buf] at index [w_pos buf]. 
    Returns the  actual number of bytes read. *)

val read_all : Lwt_unix.file_descr -> Abuf.t -> int -> int Lwt.t
(** [read fd buf len] reads [len] bytes out of the 
    file descriptior [fd] in to the buffer [buf] at index [w_pos buf]. 
    Returns the  actual number of bytes read. *)

val write : Lwt_unix.file_descr -> Abuf.t -> int Lwt.t
(** [write fd buf] writes at most [readable_bytes buf] bytes from the 
    buffer [buf] at index [r_pos buf] to the file descriptor [fd]. 
    Returns the number of bytes actually written. *)

val write_all : Lwt_unix.file_descr -> Abuf.t -> int Lwt.t
(** [write fd buf] writes [readable_bytes buf] bytes from the 
    buffer [buf] at index [r_pos buf] to the file descriptor [fd]. 
    Returns the number of bytes actually written. *)

val read_vle : Lwt_unix.file_descr -> Vle.t Lwt.t

val write_vle : Lwt_unix.file_descr -> Vle.t -> int Lwt.t

val safe_close : Lwt_unix.file_descr -> unit Lwt.t

val connect : Lwt_unix.file_descr -> Locator.Locator.t -> Lwt_unix.file_descr Lwt.t