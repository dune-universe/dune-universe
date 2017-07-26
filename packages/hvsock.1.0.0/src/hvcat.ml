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

let vmid =
  Arg.(value & opt (some string) None & info ~docv:"VMID" ~doc:"Identifier of VM/partition" [ "vmid" ])

let serviceid =
  Arg.(value & pos 0 string "3049197C-9A4E-4FBF-9367-97F792F16994" & info ~docv:"SERVICEID" ~doc:"Identifier of service" [])

let echo =
  let doc = "Run a simple multithreaded echo server" in
  Arg.(value & flag & info ["echo"] ~doc)

let buffer_size = 4096

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Duration.to_f ns)
end
module Hv = Lwt_hvsock.Make(Time)(Lwt_hvsock_detach)

let make_channels t =
  let read_buffer = Cstruct.create buffer_size in
  let read b off len =
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

let rec connect vmid serviceid =
  let fd = Hv.create () in
  Lwt.catch
    (fun () ->
      Hv.connect fd { Hvsock.vmid; serviceid }
      >>= fun () ->
      Lwt.return fd
    ) (fun e ->
      Printf.fprintf stderr "connect raised %s: sleep 1s and retrying\n%!" (Printexc.to_string e);
      Hv.close fd
      >>= fun () ->
      Lwt_unix.sleep 1.
      >>= fun () ->
      connect vmid serviceid
    )

let client vmid serviceid =
  try
    connect vmid serviceid
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

let one_shot_server vmid serviceid =
  let s = Hv.create () in
  Hv.bind s { Hvsock.vmid; serviceid };
  Hv.listen s 1;
  Hv.accept s
  >>= fun (client, { Hvsock.vmid; serviceid }) ->
  Printf.fprintf stderr "Connection from %s:%s\n%!" (Hvsock.string_of_vmid vmid) serviceid;
  let ic, oc = make_channels client in
  proxy buffer_size (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)
  >>= fun () ->
  Hv.close client
  >>= fun () ->
  Hv.close s

let echo_server vmid serviceid =
  let s = Hv.create () in
  Hv.bind s { Hvsock.vmid; serviceid };
  Hv.listen s 5;
  let rec loop () =
    Hv.accept s
    >>= fun (fd, { Hvsock.vmid; serviceid }) ->
    Printf.fprintf stderr "Connection from %s:%s\n%!" (Hvsock.string_of_vmid vmid) serviceid;
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

let main listen echo vmid serviceid =
  let vmid = match vmid with
    | None -> Hvsock.Wildcard
    | Some x -> Hvsock.Id x in
  Printf.fprintf stderr "listen=%b echo=%b vmid=%s serviceid=%s\n%!" listen echo (Hvsock.string_of_vmid vmid) serviceid;
  let t = match listen, echo with
    | true, false -> one_shot_server vmid serviceid
    | true, true -> echo_server vmid serviceid
    | false, _ -> client vmid serviceid in
  Lwt_main.run t

let cmd =
  let doc = "Establish Hyper-V socket connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to a server via a Hyper-V socket and transfer data over stdin/stdout, in a similar way to 'nc'";
    `S "EXAMPLES";
    `P "To listen for an incoming connection from anywhere:";
    `P "hvcat -l 3049197C-9A4E-4FBF-9367-97F792F16994";
    `P "To connect to a service in a remote partition:";
    `P "hvcat <vmid> 3049197C-9A4E-4FBF-9367-97F792F16994";
  ] in
  Term.(pure main $ listen $ echo $ vmid $ serviceid),
  Term.info "hvcat" ~version:"0.1" ~doc ~man

let () =
let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal Sys.sigint
  (fun (_: int) ->
    Lwt.wakeup_later sigint_u ();
  ) in
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
