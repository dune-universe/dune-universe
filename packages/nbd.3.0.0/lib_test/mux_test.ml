open OUnit
open Sexplib.Std
open Lwt

module TestPacket = struct
  type id = int

  type request_hdr = {
    req_id : int;
    req_payload : string;
  } [@@deriving sexp]

  type request_body = bytes

  type response_hdr = {
    res_id : int option;
    res_payload : string;
  } [@@deriving sexp]

  type response_body = bytes

  type seq = Request of request_hdr | Response of response_hdr

  type transport = {
    recv_cond : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    recv_queue : response_hdr Queue.t;
    mutable seq : seq list;
  }

  let record_sequence = ref true

  let recv_hdr t =
    Lwt_mutex.with_lock t.mutex (fun () ->
        let rec loop () =
          if Queue.is_empty t.recv_queue then begin
            Lwt_condition.wait ~mutex:t.mutex t.recv_cond
          end else Lwt.return () in
        loop () >>= fun () ->
        let res = Queue.pop t.recv_queue in
        if !record_sequence then t.seq <- (Response res) :: t.seq;
        Lwt.return (res.res_id, res))

  let recv_body t req_hdr rsp_hdr rsp_body =
    Bytes.blit rsp_hdr.res_payload 0 rsp_body 0 (Bytes.length rsp_hdr.res_payload);
    Lwt.return (Ok ())

  let send_one t x _ =
    Lwt_mutex.with_lock t.mutex (fun () ->
      if !record_sequence then t.seq <- (Request x) :: t.seq; Lwt.return ())

  let id_of_request r =
    r.req_id

  let handle_unrequested_packet t p =
    if p.res_payload = "exception"
    then Lwt.fail (Failure "requested exception")
    else Lwt.return ()

let create () =
  { recv_cond = Lwt_condition.create ();
    mutex = Lwt_mutex.create ();
    recv_queue = Queue.create ();
    seq = []; }

let queue_response res t =
  Lwt_mutex.with_lock t.mutex (fun () ->
      Queue.push res t.recv_queue;
      Lwt_condition.broadcast t.recv_cond ();
      Lwt.return ())
end

module T = Nbd.Mux.Make(TestPacket)

(* Some helpful packets for all tests *)
let p1 = TestPacket.{ req_id = 1; req_payload = "p1" }
let p2 = TestPacket.{ req_id = 2; req_payload = "p2" }
let r1 = TestPacket.{ res_id = Some 1; res_payload = "r1" }
let r2 = TestPacket.{ res_id = Some 2; res_payload = "r2" }

let (>>|=) m f =
  (* Check for an `Ok result in an Lwt thread, and fail the
     thread if it's an Error *)
  m >>= function
  | Ok x -> f x
  | Error x -> Lwt.fail (Failure (Nbd.Protocol.Error.to_string x))


let test_rpc =
  "Basic test of the rpc function" >:: fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response = Bytes.create 2 in
      let t1 = T.rpc p1 p1.req_payload response client in
      TestPacket.queue_response r1 transport >>= fun () ->
      t1 >>|= fun () ->
      Lwt.return (response = r1.res_payload)
    in assert_bool "RPC response correct" (Lwt_main.run t)

let test_multi_rpc =
  "Test queuing of rpc calls in the mux" >:: fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      let response2 = Bytes.create 2 in
      let t1 = T.rpc p1 p1.req_payload response1 client in
      let t2 = T.rpc p2 p2.req_payload response2 client in
      TestPacket.queue_response r1 transport >>= fun () ->
      TestPacket.queue_response r2 transport >>= fun () ->
      t1 >>|= fun () -> t2 >>|= fun () ->
      Lwt.return (response1 = r1.res_payload && response2 = r2.res_payload)
    in assert_bool "Both responses correct" (Lwt_main.run t)

let test_out_of_order_responses =
  "Test RPC functions work when responses are received out of order"
  >:: fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      let response2 = Bytes.create 2 in
      let t1 = T.rpc p1 p1.req_payload response1 client in
      let t2 = T.rpc p2 p2.req_payload response2 client in
      TestPacket.queue_response r2 transport >>= fun () ->
      TestPacket.queue_response r1 transport >>= fun () ->
      t1 >>|= fun () -> t2 >>|= fun () ->
      Lwt.return (response1 = r1.res_payload && response2 = r2.res_payload)
    in assert_bool "Both responses correct" (Lwt_main.run t)

let test_memory_leak =
  "Check the mux does not have a memory leak" >:: fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      TestPacket.record_sequence := false;
      let rec megaqueue n =
        if n=100000 then Lwt.return true
        else
          let t1 = T.rpc p1 p1.req_payload response1 client in
          TestPacket.queue_response r1 transport >>= fun () ->
          t1 >>= fun _ ->
          let ok = if n mod 10000 = 0 then begin
              Gc.compact ();
              let test = Gc.stat () in
              test.Gc.live_words < 100000
            end else true
          in
          if ok then megaqueue (n+1) else Lwt.return false
      in
      megaqueue 0
    in
    assert_bool "Memory leak" (Lwt_main.run t)

let test_exception_handling =
  "Check that exceptions raised are handled correctly" >:: fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      TestPacket.queue_response { res_id=None; res_payload="exception" } transport >>= fun () ->
      let t1 = T.rpc p1 p1.req_payload response1 client in
      TestPacket.queue_response r2 transport >>= fun () ->
      Lwt.catch  (fun () -> t1 >>= function Ok _ -> Lwt.return false | Error _ -> Lwt.return true)
        (fun e -> Printf.printf "Exception: %s\n%!" (Printexc.to_string e); Lwt.return true)
    in assert_bool "Exception handled" (Lwt_main.run t)
