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

open Cmdliner

let debug_print = ref false

let debug fmt =
  Printf.ksprintf (fun s ->
    if !debug_print then begin
      output_string stderr s;
      output_string stderr "\n";
      flush stderr
    end
  ) fmt

let default_serviceid =
  Printf.sprintf "%08x-FACB-11E6-BD58-64006A7986D3" 0x5653 (* matches virtsock/cmd/sock_stress/vsock.go *)

let buffer_size = 4096

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Duration.to_f ns)
end
module Hv = Hvsock_lwt.Flow.Make(Time)(Hvsock_lwt_unix.Preemptive_detach)(Hvsock.Af_hyperv)

let rec connect i vmid serviceid =
  let fd = Hv.Socket.create () in
  Lwt.catch
    (fun () ->
      Hv.Socket.connect fd { Hvsock.Af_hyperv.vmid; serviceid }
      >>= fun () ->
      let flow = Hv.connect fd in
      Lwt.return flow
    ) (fun e ->
      debug "%d: connect raised %s: sleep 1s and retrying" i (Printexc.to_string e);
      Hv.Socket.close fd
      >>= fun () ->
      Lwt_unix.sleep 1.
      >>= fun () ->
      connect i vmid serviceid
    )

let send_receive_verify i flow max_data_length =
  let reader_sha = Sha256.init () in
  let rec reader n =
    debug "%d: send_receive_verify reader read..." i;
    Hv.read flow
    >>= function
    | Ok `Eof ->
      debug "%d: send_receive_verify reader EOF after %d bytes" i n;
      Lwt.return n
    | Ok (`Data buf) ->
      debug "%d: send_receive_verify reader read %d" i (Cstruct.len buf);
      let s = Cstruct.to_string buf in
      Sha256.update_string reader_sha s;
      reader (n + (Cstruct.len buf))
    | Error _ ->
      failwith "Flow read error" in
  let writer_sha = Sha256.init () in
  let writer n =
    let rec loop remaining =
      if remaining = 0 then begin
        (* FIXME: this really should be close *)
        Hv.shutdown_write flow
        >>= fun () ->
        debug "%d: send_receive_verify writer loop shutdown_write after %d bytes" i n;
        Lwt.return ()
      end else begin
        let this_time = min buffer_size remaining in
        let buf = Cstruct.create this_time in
        for i = 0 to Cstruct.len buf - 1 do
          Cstruct.set_uint8 buf i (Random.int 255)
        done;
        let s = Cstruct.to_string buf in
        debug "%d: send_receive_verify writer loop write..." i;
        Hv.write flow buf
        >>= function
        | Ok () ->
          debug "%d: send_receive_verify writer loop written %d" i this_time;
          Sha256.update_string writer_sha s;
          loop (remaining - this_time)
        | Error _ ->
          failwith "Flow write error"
      end in
    loop n in
  let n_written = Random.int max_data_length in
  let writer_t = writer n_written in
  let reader_t = reader 0 in
  Lwt.join [ (reader_t >>= fun _ -> Lwt.return ()); writer_t ]
  >>= fun () ->
  reader_t
  >>= fun n_read ->
  let read_sha = Sha256.(to_hex @@ finalize reader_sha) in
  let write_sha = Sha256.(to_hex @@ finalize writer_sha) in
  debug "%d: reader SHA = %s" i read_sha;
  debug "%d: writer SHA = %s" i write_sha;
  if read_sha <> write_sha
  then failwith (Printf.sprintf "Checksum does not match. Written %d (%s), read %d (%s)" n_written write_sha n_read read_sha);
  Lwt.return_unit

let client vmid p connections_remaining max_data_length =
  let connections_remaining = ref connections_remaining in
  let rec loop i =
    if !connections_remaining = 0
    then Lwt.return () else begin
      debug "connections_remaining = %d" !connections_remaining;
      decr connections_remaining;
      connect i vmid default_serviceid
      >>= fun flow ->
      debug "%d: connected" i;
      send_receive_verify i flow max_data_length
      >>= fun () ->
      debug "%d: closing" i;
      Hv.close flow
      >>= fun () ->
      loop i
    end in
  let rec threads n =
    if n = p then [] else (loop n) :: (threads (n+1)) in
  Lwt.join (threads 0)

let main c p i v l =
  debug_print := v;
  match c with
  | None ->
    Printf.fprintf stderr "Please provide a -c hvsock://<vmid> argument\n";
    exit 1
  | Some uri ->
    let u = Uri.of_string uri in
    begin match Uri.scheme u, Uri.host u with
    | Some "hvsock", Some vmid ->
      begin match Uuidm.of_string vmid with
      | Some vmid ->
        Lwt_main.run (client (Hvsock.Af_hyperv.Id vmid) p i l);
        `Ok ()
      | None ->
        Printf.fprintf stderr "Failed to parse VM GUID: %s\n" vmid;
        exit 1
      end
    | _, _ ->
      Printf.fprintf stderr "Please provide a -c hvsock://<vmid> argument\n";
      exit 1
    end

(* Note we try to keep the command-line compatible with the Go
   virtsock/cmd/sock_stress *)

let c =
  Arg.(value & opt (some string) None & info ~docv:"CLIENT" ~doc:"Run as a client" [ "c" ])

let p =
  Arg.(value & opt int 1 & info ~docv:"PARALLEL" ~doc:"Threads to run in parallel" [ "p" ])

let i =
  Arg.(value & opt int 100 & info ~docv:"CONNECTIONS" ~doc:"Total number of connections" [ "i" ])

let v =
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc:"Enable verbose debug")

let l =
  Arg.(value & opt int 65536 & info ~docv:"MAX DATA LENGTH" ~doc:"Maximum length of data" [ "l" ])

let cmd =
  let doc = "Test AF_HYPERV connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to an echo server via a Hyper-V socket, send random data, receive a response and check the data is the same. ";
    `S "EXAMPLES";
    `P "To connect to a service in a remote partition:";
    `P "sock_stress -c hvsock://<vmid>";
  ] in
  Term.(const main $ c $ p $ i $ v $ l),
  Term.info "sock_stress" ~version:"%0.1" ~doc ~exits:Term.default_exits ~man

let () =
let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal Sys.sigint
  (fun (_: int) ->
    Lwt.wakeup_later sigint_u ();
  ) in
  Term.exit @@ Term.eval cmd
