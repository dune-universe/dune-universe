(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ipv6_multicast

module Socket = struct
  type 'a domain = 'a Ipv6_multicast.Socket.domain
  type 'a typ = 'a Ipv6_multicast.Socket.typ

  type ('domain, 'typ) t = {
    sock : ('domain, 'typ) Ipv6_multicast.Socket.t ;
    fd : Unix.file_descr ;
    lwt : Lwt_unix.file_descr ;
  }

  let create ?proto domain typ =
    let sock = Socket.create ?proto domain typ in
    let fd = Socket.to_fd sock in
    let lwt = Lwt_unix.of_unix_file_descr ~blocking:false fd in
    { sock ; fd ; lwt }
end

module Sockopt = struct
  let set { Socket.sock } opt = Sockopt.set sock opt
end

let bind { Socket.sock ; lwt } sa =
  Lwt_unix.check_descriptor lwt ;
  bind sock sa

let connect { Socket.sock ; fd ; lwt } sa =
  let open Lwt_unix in
  if Sys.win32 then
    (* [in_progress] tell wether connection has started but not
       terminated: *)
    let in_progress = ref false in
    wrap_syscall Write lwt begin fun () ->
      if !in_progress then
        (* Nothing works without this test and i have no idea why... *)
        if writable lwt then
          try
            Ipv6_multicast.connect sock sa
          with
          | Unix.Unix_error (EISCONN, _, _) ->
              (* This is the windows way of telling that the connection
                 has completed. *)
              Ok ()
        else
        raise Retry
      else
      try
        Ipv6_multicast.connect sock sa
      with
      | Unix.Unix_error (EWOULDBLOCK, _, _) ->
          in_progress := true;
          raise Retry
    end
  else
  (* [in_progress] tell wether connection has started but not
     terminated: *)
  let in_progress = ref false in
  wrap_syscall Write lwt begin fun () ->
    if !in_progress then
      (* If the connection is in progress, [getsockopt_error] tells
         wether it succceed: *)
      match Unix.getsockopt_error fd with
      | None ->
          (* The socket is connected *)
          Ok ()
      | Some err ->
          (* An error happened: *)
          raise (Unix.Unix_error(err, "connect", ""))
    else
    try
      (* We should pass only one time here, unless the system call
         is interrupted by a signal: *)
      Ipv6_multicast.connect sock sa
    with
    | Unix.Unix_error (EINPROGRESS, _, _) ->
        in_progress := true;
        raise Retry
  end

let send ?saddr ?flags { Socket.sock ; lwt } cs =
  Lwt_unix.(wrap_syscall Write lwt begin fun () ->
      Ipv6_multicast.send ?saddr ?flags sock cs
    end)

let send_bytes ?saddr ?flags { Socket.sock ; lwt } buf pos len =
  if pos < 0 || len < 0 || pos > Bytes.length buf - len then
    Lwt.fail_invalid_arg "send_bytes"
  else
  Lwt_unix.(wrap_syscall Write lwt begin fun () ->
      Ipv6_multicast.send_bytes ?saddr ?flags sock buf pos len
    end)

let recv ?flags { Socket.sock ; lwt } cs =
  Lwt_unix.(wrap_syscall Read lwt begin fun () ->
      Ipv6_multicast.recv ?flags sock cs
    end)

let recv_bytes ?flags { Socket.sock ; lwt } buf pos len =
  if pos < 0 || len < 0 || pos > Bytes.length buf - len then
    Lwt.fail_invalid_arg "recv_bytes"
  else
  Lwt_unix.(wrap_syscall Read lwt begin fun () ->
      Ipv6_multicast.recv_bytes ?flags sock buf pos len
    end)

let recvfrom ?flags { Socket.sock ; lwt } cs =
  Lwt_unix.(wrap_syscall Read lwt begin fun () ->
      Ipv6_multicast.recvfrom ?flags sock cs
    end)

let recvfrom_bytes ?flags { Socket.sock ; lwt } buf pos len =
  if pos < 0 || len < 0 || pos > Bytes.length buf - len then
    Lwt.fail_invalid_arg "recvfrom_bytes"
  else
  Lwt_unix.(wrap_syscall Read lwt begin fun () ->
      Ipv6_multicast.recvfrom_bytes ?flags sock buf pos len
    end)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
