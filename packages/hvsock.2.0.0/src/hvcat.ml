(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
 * Copyright (C) 2016 Docker Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt

let sigint_t, sigint_u = Lwt.task ()

let proxy buffer_size (ic, oc) (stdin, stdout) =
  let a_buffer = Bytes.create buffer_size in
  let b_buffer = Bytes.create buffer_size in
  let rec proxy buffer a b =
    Lwt_io.read_into a buffer 0 buffer_size
    >>= function
    | 0 -> Lwt.fail End_of_file
    | n ->
      Lwt_io.write_from_exactly b buffer 0 n
      >>= fun () ->
      proxy buffer a b in
  let (a: unit Lwt.t) = proxy a_buffer stdin oc in
  let (b: unit Lwt.t) = proxy b_buffer ic stdout in
  Lwt.catch
    (fun () -> Lwt.pick [a; b])
    (function End_of_file -> Lwt.return ()
     | e -> Lwt.fail e)

open Cmdliner

let listen =
  let doc = "Act as a server rather than a client." in
  Arg.(value & flag & info [ "l"; "listen"] ~doc)

let uri =
  Arg.(value & pos 0 string "vsock://:80" & info ~docv:"URI" ~doc:"URI of service" [])

let echo =
  let doc = "Run a simple multithreaded echo server" in
  Arg.(value & flag & info ["echo"] ~doc)

let register =
  let doc = "Add Hyper-V service GUID to the registry (requires Administrator)" in
  Arg.(value & flag & info ["register-guid"] ~doc)

let buffer_size = 4096

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Duration.to_f ns)
end
module Hv = Hvsock_lwt.Socket.Make(Time)(Hvsock_lwt_unix.Preemptive_detach)(Hvsock.Socket)

let make_channels t =
  let read_buffer = Cstruct.create buffer_size in
  let read b off _len =
    Hv.read t read_buffer
    >>= fun n ->
    Lwt_bytes.blit read_buffer.Cstruct.buffer 0 b off n;
    Lwt.return n in
  let write_buffer = Cstruct.create buffer_size in
  let write b off len =
    Lwt_bytes.blit b off write_buffer.Cstruct.buffer 0 len;
    Hv.write t write_buffer in
  let ic = Lwt_io.make ~buffer:(Lwt_bytes.create buffer_size) ~mode:Lwt_io.input read in
  let oc = Lwt_io.make ~buffer:(Lwt_bytes.create buffer_size) ~mode:Lwt_io.output write in
  ic, oc

let rec connect sockaddr =
  let fd = Hv.create () in
  Lwt.catch
    (fun () ->
      Hv.connect fd sockaddr
      >>= fun () ->
      Lwt.return fd
    ) (fun e ->
      Printf.fprintf stderr "connect raised %s: sleep 1s and retrying\n%!" (Printexc.to_string e);
      Hv.close fd
      >>= fun () ->
      Lwt_unix.sleep 1.
      >>= fun () ->
      connect sockaddr
    )

let client sockaddr =
  try
    connect sockaddr
    >>= fun fd ->
    Printf.fprintf stderr "Connected\n%!";
    let ic, oc = make_channels fd in
    proxy buffer_size (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
    >>= fun () ->
    Hv.close fd
  with
  | Unix.Unix_error(Unix.ENOENT, _, _) ->
    Printf.fprintf stderr "Server not found (ENOENT)\n";
    Lwt.return ()

let one_shot_server sockaddr =
  let s = Hv.create () in
  Hv.bind s sockaddr;
  Hv.listen s 1;
  Hv.accept s
  >>= fun (client, sockaddr) ->
  Printf.fprintf stderr "Connection from %s\n%!" (Hv.string_of_sockaddr sockaddr);
  let ic, oc = make_channels client in
  proxy buffer_size (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
  >>= fun () ->
  Hv.close client
  >>= fun () ->
  Hv.close s

let echo_server sockaddr =
  let s = Hv.create () in
  Hv.bind s sockaddr;
  Hv.listen s 5;
  let rec loop () =
    Hv.accept s
    >>= fun (fd, sockaddr) ->
    Printf.fprintf stderr "Connection from %s\n%!" (Hv.string_of_sockaddr sockaddr);
    Lwt.async (fun () ->
      let ic, oc = make_channels fd in
      proxy buffer_size (ic, oc) (ic, oc)
      >>= fun () ->
      Hv.close fd
      >>= fun () ->
      Printf.fprintf stderr "Disconnected\n%!";
      Lwt.return ()
    );
    loop () in
  loop ()
  >>= fun () ->
  Hv.close s

let register_guid sockaddr =
  match Hvsock.Socket.to_hyperv sockaddr with
  | Some { Hvsock.Af_hyperv.serviceid; _ } ->
    Hvsock.Af_hyperv.register_serviceid serviceid
  | None -> ()

(* --register ? *)
let main listen echo uri register =
  let sockaddr = Hvsock.Socket.sockaddr_of_uri (Uri.of_string uri) in
  Printf.fprintf stderr "listen=%b echo=%b sockaddr=%s\n%!" listen echo (Hvsock.Socket.string_of_sockaddr sockaddr);
  if register then register_guid sockaddr;
  let t = match listen, echo with
    | true, false -> one_shot_server sockaddr
    | true, true -> echo_server sockaddr
    | false, _ -> client sockaddr in
  Lwt_main.run t

let cmd =
  let doc = "Establish hypervisor socket connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to a server via a hypervisor socket and transfer data over stdin/stdout, in a similar way to 'nc'";
    `S "EXAMPLES";
    `P "To listen for an incoming Hyper-V connection from anywhere on a given serviceid (Windows only):";
    `P "hvcat -l hvsock:///3049197C-9A4E-4FBF-9367-97F792F16994";
    `P "To listen for an incoming connection from anywhere to AF_VSOCK port 80 (or the corresponding AF_HYPERV port):";
    `P "hvcat -l vsock://:80";
    `P "To connect to a service in a remote partition on Windows:";
    `P "hvcat hvsock://<VM name or GUID>/3049197C-9A4E-4FBF-9367-97F792F16994";
    `P "To connect to a service in a VM on Linux:";
    `P "hvcat hvsock://2:80/";
  ] in
  Term.(pure main $ listen $ echo $ uri $ register),
  Term.info "hvcat" ~version:"0.1" ~doc ~man

let () =
let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal Sys.sigint
  (fun (_: int) ->
    Lwt.wakeup_later sigint_u ();
  ) in
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
