
module type S = sig
  (** A low-level socket interface, common to both Windows and Linux kernels *)

  type t

  type sockaddr
  (** A socket address *)

  val string_of_sockaddr: sockaddr -> string

  val create: unit -> t
  (** [create ()] creates an unbound socket *)

  val bind: t -> sockaddr -> unit
  (** [bind socket sockaddr] binds [socket] to [sockaddr] *)

  val listen: t -> int -> unit
  (** [listen socket queue_length] sets the socket to listening mode with the
      given maximum queue length *)

  val accept: t -> t * sockaddr
  (** [accept fd] accepts a single connection *)

  val connect: ?timeout_ms:int -> t -> sockaddr -> unit
  (** [connect ?timeout_ms fd sockaddr] connects to a remote socket.
      On Windows the raw connect call can block forever if the server is not
      running when the call is executed (even if the server starts up afterwards)
      there is a default timeout of 300ms. On timeout this will raise
      [Unix_error(Unix.ETIMEDOUT)] *)

  val writev: t -> Cstruct.t list -> int
  (** Write a list of buffers *)

  val read_into: t -> Cstruct.t -> int
  (** Read into a buffer, returning the number of bytes written *)

  val shutdown_read: t -> unit
  (** Close the read half of the connection *)

  val shutdown_write: t -> unit
  (** Close the write half of the connection *)

  val close: t -> unit
  (** Close both halves of the connection *)
end

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external stub_ba_sendv: Unix.file_descr -> (buffer * int * int) list -> int = "stub_hvsock_ba_sendv"
external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"

let writev fd bs =
  let bs' = List.map (fun b -> b.Cstruct.buffer, b.Cstruct.off, b.Cstruct.len) bs in
  stub_ba_sendv fd bs'
let read_into fd b = stub_ba_recv fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len