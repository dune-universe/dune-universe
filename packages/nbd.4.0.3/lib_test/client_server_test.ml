(** This module tests the NBD server and the NBD client provided by the core
    NBD library, by connecting their inputs and outputs together so that they
    communicate with each other through an in-memory pipe.
    The server and client IO is run in concurrently by Lwt, in the same
    process. *)

open Lwt.Infix

let with_channels f =
  let section = Lwt_log_core.Section.make("with_channels") in
  let make_channel name (ic, oc) =
    let read c =
      let len = Cstruct.len c in
      Lwt_log.debug_f ~section "%s read: %d" name len >>= fun () ->
      let b = Bytes.create len in
      Lwt_io.read_into_exactly ic b 0 len >>= fun () ->
      Cstruct.blit_from_bytes b 0 c 0 len;
      Lwt_log.debug_f ~section "%s read: %d: %s finished" name len (String.escaped (Cstruct.to_string c))
    in
    let write c =
      let len = Cstruct.len c in
      Lwt_log.debug_f ~section "%s write: %d: %s" name len (String.escaped (Cstruct.to_string c)) >>= fun () ->
      Lwt_io.write_from_string_exactly oc (Cstruct.to_string c) 0 len >>= fun () ->
      Lwt_log.debug_f ~section "%s write: %d: %s finished" name len (String.escaped (Cstruct.to_string c))
    in
    (write, read)
  in
  let client_to_server = Lwt_io.pipe () in
  let server_to_client = Lwt_io.pipe () in
  let client_write, server_read = make_channel "client -> server" client_to_server in
  let server_write, client_read = make_channel "server -> client" server_to_client in
  let noop () = Lwt.return_unit in
  let client_channel =
    Nbd.Channel.{ read=client_read; write=client_write; close=noop; is_tls=false }
  in
  let server_channel =
    Nbd.Channel.{ read_clear=server_read; write_clear=server_write; close_clear=noop; make_tls_channel=None }
  in
  Lwt_unix.with_timeout 0.5 (fun () -> f client_channel server_channel)

(** Run the given server and client test sequences concurrently with channels
    connecting the server and the client together. *)
let test ~server ~client () =
  Lwt_log.add_rule "*" Lwt_log.Debug;
  with_channels (fun client_channel server_channel ->
      let test_server = server server_channel in
      let cancel, _ = Lwt.task () in
      let test_server =
        Lwt.catch
          (fun () -> Lwt.pick [test_server; cancel])
          (function Lwt.Canceled -> Lwt.return_unit | e -> Lwt.fail e)
      in
      let test_client =
        client client_channel
        (* TODO: because Client.disconnect does not send NBD_CMD_DISC,
           the server loop will not stop - we have to stop it manually.
           Once this is fixed, this cancel mechanism should be removed. *)
        >|= fun () -> Lwt.cancel cancel
      in
      Lwt.join [test_server; test_client]
    )

(** We fail the test if an error occurs *)
let check msg =
  function
  | Result.Ok a -> Lwt.return a
  | Result.Error _ -> Lwt.fail_with msg

let test_connect_disconnect _switch =
  let test_block = (Cstruct.of_string "asdf") in
  test
    ~server:(fun server_channel ->
        Nbd.Server.connect server_channel () >>= fun (export_name, svr) ->
        Alcotest.(check string) "export name received by server"
          "export1" export_name;
        Nbd.Server.serve svr ~read_only:false (module Cstruct_block.Block) test_block
      )
    ~client:(fun client_channel ->
        Nbd.Client.negotiate client_channel "export1" >>= fun (t, size, _flags) ->
        Alcotest.(check int64) "size received by client"
          (Int64.of_int (Cstruct.len test_block))
          size;
        Nbd.Client.disconnect t
      )

let test_list_exports _switch =
  test
    ~server:(fun server_channel ->
        Lwt.catch
          (fun () ->
             Nbd.Server.connect ~offer:["export1";"export2"] server_channel () >>= fun _ ->
             Alcotest.fail "Server should not enter transmission mode")
          (function
            | Nbd.Server.Client_requested_abort -> Lwt.return_unit
            | e -> Lwt.fail e)
      )
    ~client:(fun client_channel ->
        Nbd.Client.list client_channel >|= fun exports ->
        Alcotest.(check (result (slist string String.compare) reject))
          "Received correct export names"
          (Ok ["export1";"export2"])
          exports
      )

let test_read_write _switch =
  let test_block = (Cstruct.of_string "asdf") in
  test
    ~server:(fun server_channel ->
        Nbd.Server.connect server_channel () >>= fun (_export_name, svr) ->
        Nbd.Server.serve svr ~read_only:false (module Cstruct_block.Block) test_block
      )
    ~client:(fun client_channel ->
        Nbd.Client.negotiate client_channel "export1" >>= fun (t, _size, _flags) ->

        let buf = Cstruct.create 2 in
        Nbd.Client.read t 1L [buf] >>= check "1st read failed" >>= fun () ->
        Alcotest.(check string) "2 bytes at offset 1" "sd" (Cstruct.to_string buf);

        let buf = Cstruct.of_string "12" in
        Nbd.Client.write t 2L [buf] >>= check "Write failed" >>= fun () ->

        let buf = Cstruct.create 2 in
        Nbd.Client.read t 2L [buf] >>= check "2nd read failed" >>= fun () ->
        Alcotest.(check string) "2 modified bytes at offset 2" "12" (Cstruct.to_string buf);

        Nbd.Client.disconnect t
      )

let tests =
  let t = Alcotest_lwt.test_case in
  "Nbd client-server connection tests",
  [ t "test_connect_disconnect" `Quick test_connect_disconnect
  ; t "test_list_exports" `Quick test_list_exports
  ; t "test_read_write" `Quick test_read_write
  ]
