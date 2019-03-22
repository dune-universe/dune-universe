(** Connection to bluetooth and USB devices. *)

#ifndef LWT_t
#define LWT_t
#endif

type usb
type bluetooth

type 'a t
(** Mutable bidirectional connection handle. *)

val connect_bluetooth :
  check_status:bool -> check_status_fn:(Bytes.t -> unit LWT_t) ->
  string -> bluetooth t LWT_t

(** Connection to USB devices.  See {!Mindstorm.NXT.USB} and
    {!Mindstorm.NXT.USB_lwt} for more information.  *)
module USB :
sig
  type device
  val bricks : unit -> device list LWT_t
  val connect :
    check_status:bool -> check_status_fn:(Bytes.t -> unit LWT_t) ->
    device -> usb t LWT_t
end


val send : 'a t -> Bytes.t -> unit LWT_t
(** [send conn s] send "package" [s] on the connection [c].  [s] is
    supposed to come prefixed with 2 bytes indicating its length
    (since this is necessary for bluetooth) — they will be stripped
    for USB. *)

val recv : 'a t -> int -> Bytes.t LWT_t
(** [recv conn n] reads a "package" of length [n] and return it.  For
    bluetooth, the prefix of 2 bytes indicating the length are also
    read but not returned (and not counted in [n]).  [recv] checks the
    status byte using [check_status_fn] and possibly raise an
    exception (it is the task of [check_status_fn] to do that),
    regardless of the value of [check_status] (this is to allow to
    override [check_status] locally). *)

val really_input : 'a t -> Bytes.t -> int -> int -> unit LWT_t
(** [really_input conn buf ofs len] reads [len] characters from
    [conn] and puts them in [buf] starting at position [ofs].  It does
    NOT read the bluetooth prefix bytes, so should not be used for
    packages but only for additional data. *)

val close : 'a t -> unit LWT_t
(** Close the connection. *)

val want_check_status_by_default : 'a t -> bool
(** Says whether [~check_status:true] was passed to the connection
    function. *)


#ifdef LWT
(** With Lwt, connections come with a Lwt mutex that may be used to
    ensure that a sequence of send + recv is atomic. *)
val lock : 'a t -> unit Lwt.t
val unlock : 'a t -> unit
#endif

;;
