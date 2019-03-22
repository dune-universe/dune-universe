(* File: mindstorm_connect.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


#include "mindstorm_macros.ml"

#ifndef MODULE
#define MODULE(fn) STRINGIFY(Mindstorm_connect.fn)
#endif

(** Connection *)

type usb
type bluetooth = UNIX(file_descr)

(* The type parameter is because we want to distinguish usb and
   bluetooth connections as some commands are only available through USB. *)
type 'a t = {
  fd : 'a;
  (* We need specialized function depending on the fact that the
     connection is USB or bluetooth because bluetooth requires a
     prefix of 2 bytes indicating the length of the packet. *)
  send : 'a -> Bytes.t -> unit LWT_t;
  recv : 'a -> int -> Bytes.t LWT_t;
  really_input : 'a -> Bytes.t -> int -> int -> unit LWT_t;
  close : 'a -> unit LWT_t;
  check_status_fn : Bytes.t -> unit LWT_t; (* [check_status] function. *)
  check_status: bool; (* whether one wants a status check by default *)
#ifdef LWT
  mutex: Lwt_mutex.t;
#endif
}

let close conn = conn.close conn.fd

let send conn s =
  conn.send conn.fd s

let recv conn n =
  LET(pkg, conn.recv conn.fd n)
  EXEC(conn.check_status_fn pkg)
  RETURN(pkg)

let really_input conn buf ofs len =
  conn.really_input conn.fd buf ofs len

let want_check_status_by_default conn = conn.check_status

#ifdef LWT
let lock conn = Lwt_mutex.lock conn.mutex
let unlock conn = Lwt_mutex.unlock conn.mutex
#endif


(** Utils -------------------- *)

(* [really_input fd buf ofs len] reads [len] bytes from [fd] and store
   them into [buf] starting at position [ofs]. *)
#if (defined WIN32 || defined WIN64) && not defined CYGWIN
let really_input_fd =
  let rec loop ntries fd buf i n =
    EXEC(if ntries > 50 && i = 0 then
           (* Naive way of detecting that we are not connected -- because,
              when the brick is off, windows connects and "send" the data
              but always reads 0 bytes back. *)
           FAIL(Unix.Unix_error(Unix.EHOSTDOWN,
                                MODULE(connect_bluethooth), ""))
         else RETURN())
    LET(r, UNIX(read) fd buf i n)
    if r < n then (
      (* Unix.select implemented on windows: [fd] is a file handle, not
         a socket. *)
      loop (ntries+1) fd buf (i + r) (n - r)
    )
    else RETURN() in
  fun fd buf ofs n -> loop 1 fd buf ofs n

#else
(* Unix, Mac OS X, Cygwin *)
let really_input_fd =
  let rec loop fd buf i n =
    LET(r, UNIX(read) fd buf i n)
    if r < n then (
      (* The doc says 60ms are needed to switch from receive to
         transmit mode. *)
#ifdef LWT
      Lwt_unix.sleep 0.060 >>= fun () ->
#else
      ignore(Unix.select [fd] [] [] 0.060);
#endif
      loop fd buf (i + r) (n - r)
    )
    else RETURN() in
  fun fd buf ofs n -> loop fd buf ofs n

#endif

let really_read fd n =
  let buf = Bytes.create n in
  EXEC(really_input_fd fd buf 0 n)
  RETURN(buf)


(** USB -------------------- *)
module USB =
struct
  type device (* a handle to a USB LEGO device. *)

#ifdef HAS_USB
#ifdef MACOSX
  (* Mac OS X *)
  let bricks () = RETURN([])
  let connect ~check_status:_ ~check_status_fn:_ _socket =
    FAIL(Failure "Not yet implemented")
    (* libusb should work *)

#elif (defined WIN32 || defined WIN64) && not defined CYGWIN
  (* Windows *)
  let bricks () = RETURN([])
  let connect ~check_status:_ ~check_status_fn:_ _socket =
    FAIL(Failure "Not yet implemented");
    (* See http://www.microsoft.com/whdc/connect/usb/winusb_howto.mspx *)

#else
  (* Unix & Cygwin *)
  external bricks : unit -> device list = "ocaml_mindstorm_bricks"
  external exit_libusb : unit -> unit = "ocaml_mindstorm_exit"
  external connect_device : device -> usb = "ocaml_mindstorm_connect_usb"
  external close : usb -> unit = "ocaml_mindstorm_close_usb"
  external write : usb -> bytes -> int -> int -> unit
    = "ocaml_mindstorm_usb_write"
  external really_input : usb -> bytes -> int -> int -> unit
    = "ocaml_mindstorm_usb_really_input"

  let () = at_exit exit_libusb

#ifdef LWT
  (* Is there a better solution? *)
  let bricks = Lwt_preemptive.detach bricks
  let connect_device = Lwt_preemptive.detach connect_device
  let close = Lwt_preemptive.detach close
  let write usb s ofs len =
    Lwt_preemptive.detach (write usb s ofs) len
  let really_input usb s ofs len =
    Lwt_preemptive.detach (really_input usb s ofs) len
#endif

  let recv usb n =
    let buf = Bytes.create n in
    EXEC(really_input usb buf 0 n)
    RETURN(buf)

  (* Ignore the first 2 bytes of [pkg] that are for bluetooth only *)
  let send fd pkg = write fd pkg 2 (Bytes.length pkg - 2)

  let connect ~check_status ~check_status_fn dev =
    LET(fd, connect_device dev)
    RETURN({ fd = fd;  send = send;
             recv = recv;  really_input = really_input;
             close = close;
             check_status_fn = check_status_fn;
             check_status = check_status;
#ifdef LWT
             mutex = Lwt_mutex.create();
#endif
    })

#endif
#else
  (* No USB libary *)
  let bricks () = RETURN([])
  let connect ~check_status:_ ~check_status_fn:_ _socket =
    FAIL(Failure "The Mindstorm module was compiled without USB support")
#endif
end

(** Bluetooth -------------------- *)

let bt_send fd pkg =
  LET(_, UNIX(write) fd pkg 0 (Bytes.length pkg))
  RETURN()

let bt_recv fd n =
  LET(_size, really_read fd 2)
  LET(pkg, really_read fd n)
  assert(Bytes.get pkg 0 = '\x02');
  (* pkg.[1] is the cmd id, do we check it ?? *)
  (* We wanted to check the status and raise the corresponding
     exception here but we cannot because of the behavior of [input]. *)
  RETURN(pkg)


#ifdef MACOSX
(* Mac OS X *)
let connect_bluetooth ~check_status ~check_status_fn tty =
  LET(fd, UNIX(openfile) tty [Unix.O_RDWR] 0o660)
  RETURN({ fd = fd;  send = bt_send;
           recv = bt_recv;  really_input = really_input_fd;
           close = UNIX(close);
           check_status_fn = check_status_fn;
           check_status = check_status;
#ifdef LWT
           mutex = Lwt_mutex.create();
#endif
  })

#elif (defined WIN32 || defined WIN64) && not defined CYGWIN
(* Windows *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

#ifdef LWT
let socket_bluetooth s =
  Lwt_preemptive.detach socket_bluetooth s >>= fun fd ->
  Lwt.return(Lwt_unix.of_unix_file_descr fd)
#endif

let connect_bluetooth ~check_status ~check_status_fn addr =
  LET(fd, socket_bluetooth ("\\\\.\\" ^ addr))
  RETURN({ fd = fd;  send = bt_send;
           recv = bt_recv;  really_input = really_input_fd;
           close = UNIX(close);
           check_status_fn = check_status_fn;
           check_status = check_status;
#ifdef LWT
           mutex = Lwt_mutex.create();
#endif
  })

#else
(* Unix & Cygwin *)
external socket_bluetooth : string -> Unix.file_descr
  = "ocaml_mindstorm_connect"

#ifdef LWT
let socket_bluetooth s =
  Lwt_preemptive.detach socket_bluetooth s >>= fun fd ->
  Lwt.return(Lwt_unix.of_unix_file_descr fd)
#endif

let connect_bluetooth ~check_status ~check_status_fn addr =
  LET(fd, socket_bluetooth addr)
  RETURN({ fd = fd;  send = bt_send;
           recv = bt_recv;  really_input = really_input_fd;
           close = UNIX(close);
           check_status_fn = check_status_fn;
           check_status = check_status;
#ifdef LWT
           mutex = Lwt_mutex.create();
#endif
  })

#endif
