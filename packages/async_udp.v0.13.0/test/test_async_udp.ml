open Core
open Poly
open Async
open Expect_test_helpers
open! Import
open Async_udp

module Ready_iter = struct
  open Private.Ready_iter

  module Ok = struct
    open Ok

    let%test_unit _ =
      List.iter all ~f:(fun t ->
        let i = to_int t in
        [%test_result: t] ~expect:t (of_int_exn i);
        [%test_result: int] ~expect:i (to_int t);
        List.iter all ~f:(fun u ->
          let j = to_int u in
          if Bool.( <> ) (compare u t = 0) (j = i)
          then
            failwiths
              "overlapping representations"
              (t, i, u, j)
              [%sexp_of: t * int * t * int]))
    ;;
  end
end

let sock sock =
  let addr = Unix.Socket.getsockname sock in
  Unix.Socket.shutdown sock `Both;
  match addr with
  | `Inet (a, p) ->
    if false then require [%here] (Unix.Inet_addr.( <> ) a Unix.Inet_addr.bind_any);
    require [%here] (p <> 0);
    return ()
  | `Unix u -> failwith u
;;

let%expect_test "bind any" = sock (bind (`Inet (Unix.Inet_addr.bind_any, 0)))
let%expect_test "bind localhost" = sock (bind (`Inet (Unix.Inet_addr.localhost, 0)))
let%expect_test "bind_any" = sock (bind_any ())

let with_socks ~expected_effects sexp_of_effect f =
  let sock1 = bind_any () in
  Monitor.protect
    ~finally:(fun () -> Fd.close (Socket.fd sock1))
    (fun () ->
       let sock2 = bind_any () in
       Monitor.protect
         ~finally:(fun () -> Fd.close (Socket.fd sock2))
         (fun () ->
            let `Inet (_host1, port1), `Inet (_host2, port2) =
              Unix.Socket.getsockname sock1, Unix.Socket.getsockname sock2
            in
            let rev_effects = ref [] in
            with_timeout
              (sec 0.1)
              (f
                 ~sock1
                 ~sock2
                 ~effect:(fun e -> rev_effects := e :: !rev_effects)
                 ~addr1:(`Inet (Unix.Inet_addr.localhost, port1))
                 ~addr2:(`Inet (Unix.Inet_addr.localhost, port2)))
            >>| fun outcome ->
            let effects = List.rev !rev_effects in
            if not (Stdlib.( = ) expected_effects effects)
            then
              failwiths
                "unexpected effects"
                [%sexp
                  ~~(outcome : [ `Result of _ | `Timeout ])
                , ~~(effects : effect list)
                , ~~(expected_effects : effect list)]
                [%sexp_of: Sexp.t]))
;;

let%expect_test "stop smoke" =
  match sendto () with
  | Error nonfatal ->
    Debug.eprints "nonfatal" nonfatal [%sexp_of: Error.t];
    [%expect.unreachable]
  | Ok sendto ->
    let prefix = [ "a"; "b" ] in
    let suffix = [ "c"; "d" ] in
    with_socks
      ~expected_effects:prefix
      [%sexp_of: string]
      (fun ~sock1 ~sock2 ~effect ~addr1:_ ~addr2 ->
         let stopped = ref false in
         let received = Bvar.create () in
         Deferred.all_unit
           [ Deferred.List.iter ~how:`Sequential (prefix @ suffix) ~f:(fun str ->
               if !stopped
               then Deferred.unit
               else
                 Deferred.all_unit
                   [ sendto (Socket.fd sock1) (Iobuf.of_string str) addr2
                   ; Bvar.wait received
                   ])
           ; (Monitor.try_with (fun () ->
                read_loop (Socket.fd sock2) (fun buf ->
                  let str = Iobuf.to_string buf in
                  effect str;
                  Bvar.broadcast received ();
                  if String.equal str (List.last_exn prefix)
                  then (
                    stopped := true;
                    failwith "Stop")))
              >>| function
              | Error _ when !stopped -> ()
              (* We don't close the socket or stop the loop in this test (yet). *)
              | Ok (Closed | Stopped) -> assert false
              | Error e -> raise e)
           ])
;;

let with_fsts send ~expected_effects sexp_of_effect receiver =
  match send with
  | Error e ->
    eprintf "%s\n" (Error.to_string_hum e);
    Deferred.unit
  | Ok send ->
    with_socks
      ~expected_effects
      sexp_of_effect
      (fun ~sock1 ~sock2 ~effect ~addr1:_ ~addr2 ->
         Deferred.List.iter expected_effects ~f:(fun (s, _) ->
           send (Socket.fd sock1) (Iobuf.of_string s) addr2)
         >>= fun () -> receiver ~sock2 ~effect)
;;

let with_send_fsts ~expected_effects sexp_of_effect receiver =
  with_fsts (sendto ()) ~expected_effects sexp_of_effect receiver
  >>= fun () ->
  with_fsts
    (Or_error.map (sendto_sync ()) ~f:(fun sendto_sync fd buf addr ->
       match Unix.Syscall_result.Unit.to_result (sendto_sync fd buf addr) with
       | Ok () -> Deferred.unit
       | Error (EWOULDBLOCK | EAGAIN | EINTR) -> assert false
       | Error e -> raise (Unix.Unix_error (e, "sendto", ""))))
    ~expected_effects
    sexp_of_effect
    receiver
;;

let%expect_test "recvfrom_loop" =
  with_send_fsts
    ~expected_effects:[ "a", 0; "bcd", 0; "efghijklmnop", 0 ]
    [%sexp_of: string * int]
    (fun ~sock2 ~effect ->
       recvfrom_loop (Socket.fd sock2) (fun b _ -> effect (Iobuf.to_string b, 0)))
;;

let%expect_test "read_loop" =
  with_send_fsts
    ~expected_effects:[ "a", 0; "bcd", 0; "efghijklmnop", 0 ]
    [%sexp_of: string * int]
    (fun ~sock2 ~effect ->
       read_loop (Socket.fd sock2) (fun b -> effect (Iobuf.to_string b, 0)))
;;

(* Queue up some packets and check that they're received all at once.  There's a tiny
   element of faith in assuming they'll be queued rather than dropped and that they're
   delivered in order. *)
let%expect_test "recvmmsg_loop" =
  match recvmmsg_loop with
  | Error err ->
    eprintf "%s\n" (Error.to_string_hum err);
    [%expect.unreachable]
  | Ok recvmmsg_loop ->
    with_send_fsts
      ~expected_effects:
        [ "Welcome", 0
        ; "to", 1
        ; "the", 2
        ; "jungle!", 3
        ; "You're", 4
        ; "gonna", 5
        ; "burn!", 6
        ]
      [%sexp_of: string * int]
      (fun ~sock2 ~effect ->
         recvmmsg_loop (Socket.fd sock2) (fun bufs ~count ->
           for i = 0 to count - 1 do
             effect (Iobuf.to_string bufs.(i), i)
           done))
;;
