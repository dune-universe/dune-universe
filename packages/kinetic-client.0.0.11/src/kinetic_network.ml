(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
 *)

open Kinetic_util
open Lwt.Infix

open Kinetic_error

let _decode_fixed32 (b:Bytes.t) off =
  let byte x =
    let pos = off + x in
    let c = Bytes.get b pos in
    int_of_char c
  in
  ((byte 3) lsl  0)
  + ((byte 2) lsl  8)
  + ((byte 1) lsl 16)
  + ((byte 0) lsl 24)

let _encode_fixed32 (s:bytes) off i =
  let get_char i shift = char_of_int ((i land (0xff lsl shift)) lsr shift) in
  Bytes.set s off (get_char i 24);
  Bytes.set s (off + 1) (get_char i 16);
  Bytes.set s (off + 2) (get_char i  8);
  Bytes.set s (off + 3) (get_char i  0)


let read_exact_generic read_f socket (buf:'a) (off:int) (len:int) =
  let t0 = Unix.gettimeofday() in
  let small_chunk = len lsr 3 in
  let rec loop t_prev off = function
    | 0   -> Lwt.return_unit
    | len -> read_f socket buf off len >>= fun bytes_read ->
             if bytes_read = 0
             then Lwt.fail End_of_file
             else
               begin
                 let t_prev' = Unix.gettimeofday() in
                 let todo' = len - bytes_read in
                 let off' = off + bytes_read in
                 (if bytes_read < small_chunk
                  then
                    let speed_inv = (t_prev' -. t_prev) /. (float len) in
                    let will_take = (float todo') *. speed_inv in
                    Lwt_unix.sleep will_take
                  else
                    Lwt.return_unit)
                 >>= fun () ->
                 loop t_prev' off' todo'
               end
  in
  loop t0 off len

let write_exact_generic write_f socket (buf:'a) off len =
  let rec loop off = function
    | 0   -> Lwt.return_unit
    | len -> write_f socket buf off len >>= fun bytes_written ->
             loop (off + bytes_written) (len - bytes_written)
  in
  loop off len

let maybe_read_generic create read_f socket v_len =
    match v_len with
    | 0 -> Lwt.return None
    | n -> let v_buf = create v_len in
           read_exact_generic read_f socket v_buf 0 v_len >>= fun () ->
           Lwt.return (Some v_buf)

let network_receive_generic
      ~timeout
      create
      read_v read_bytes socket show_socket
      trace
  =
    Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout timeout
        (fun () ->
          let msg_bytes = Bytes.create 9 in
          read_exact_generic read_bytes socket msg_bytes 0 9 >>= fun () ->
          let magic = Bytes.get msg_bytes 0 in
          let proto_ln = _decode_fixed32 msg_bytes 1 in
          let value_ln = _decode_fixed32 msg_bytes 5 in
          (*
    Lwt_io.printlf
    "magic:%C proto_ln:%i value_ln:%i" magic proto_ln value_ln
    >>= fun () ->
           *)
          assert (magic = 'F');
          let proto_raw = Bytes.create proto_ln in
          read_exact_generic read_bytes socket proto_raw 0 proto_ln >>= fun () ->
          begin
            if trace
            then Lwt_log.debug_f
                   ~section:tracing
                   "(socket:%s) received: %s"
                   (show_socket socket)
                   (to_hex proto_raw)
            else Lwt.return_unit
          end
          >>= fun () ->
          maybe_read_generic create read_v socket value_ln >>= fun vo ->
          let m = Kinetic_pb.decode_message (Pbrt.Decoder.of_bytes proto_raw) in
          Lwt_result.return (m,vo, proto_raw)
        )
    )
    (function
     | Lwt_unix.Timeout -> Error.Timeout(timeout, "network_receive_generic") |> Lwt_result.fail
     | exn -> Error.Generic(__FILE__,__LINE__, Printexc.to_string exn) |> Lwt_result.fail
    )


let network_send_generic
      write_v write_bytes socket
      proto_raw vo show_socket trace
  =
  let prelude_len = 9 in
  let proto_raw_len = Bytes.length proto_raw in
  let message_len = 9 + proto_raw_len in
  let message = Bytes.create message_len in
  Bytes.set message 0 'F';

  _encode_fixed32 message 1 proto_raw_len;
  let v_len = match vo with
    | None -> 0
    | Some (v,off,len) -> len
  in
  _encode_fixed32 message 5 v_len;
  Bytes.blit proto_raw 0 message prelude_len proto_raw_len;

  begin
    if trace
    then
      Lwt_log.debug_f ~section:tracing "(socket:%s) sending: %s\n" (show_socket socket) (to_hex proto_raw)
    else
      Lwt.return_unit
  end
    >>= fun () ->
  write_exact_generic write_bytes socket message 0 message_len   >>= fun () ->
  match vo with
  | None   -> Lwt.return_unit
  | Some (v,off,len) -> write_exact_generic write_v socket v off v_len 

