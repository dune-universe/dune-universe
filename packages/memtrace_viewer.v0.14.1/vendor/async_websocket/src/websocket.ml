open Core
open Async
module Connection_close_reason = Connection_close_reason

module Websocket_role = struct
  type t =
    | Client
    | Server
  [@@deriving sexp_of]

  (*  https://tools.ietf.org/html/rfc6455#section-5.3 *)
  let should_mask = function
    | Client -> true
    | Server -> false
  ;;
end

(* RFC 6455: The WebSocket Protocol: http://tools.ietf.org/html/rfc6455 *)

type raw =
  { reader : Reader.t
  ; writer : Writer.t
  ; closed : (Connection_close_reason.t * string * Info.t option) Ivar.t
  }

type t =
  { raw : raw
  ; pipes : string Pipe.Reader.t * string Pipe.Writer.t
  }
[@@deriving fields]

let close_cleanly ~code ~reason ~info ws =
  Ivar.fill_if_empty ws.closed (code, reason, info)
;;

module Pipes = struct
  let recv_pipe ~masked (ws : raw) =
    (* [accum_content : list string] is a list of frame contents in the current message in
       reverse order. We do this because keeping the concatenated contents would be
       O(length^2) (and noticable in potentially realistic loads). *)
    let finalise_content accum_content = String.concat (List.rev accum_content) in
    let rec read_one ~accum_content r =
      let process_frame ({ Frame.opcode; final; content } as frame) =
        match opcode with
        | Close ->
          let info =
            let partial_content =
              match accum_content with
              | [] -> None
              | _ -> Some (finalise_content accum_content)
            in
            Some
              (Info.create_s
                 [%sexp
                   { frame : Frame.t; partial_content : (string option[@sexp.omit_nil]) }])
          in
          close_cleanly
            ~code:Connection_close_reason.Normal_closure
            ~reason:"Received close message"
            ~info
            ws;
          (* flush to make sure the close frame was sent to the client *)
          let%map () = Writer.flushed ws.writer in
          `Eof
        | Ping ->
          Frame.write_frame ws.writer ~masked { frame with opcode = Pong };
          read_one ~accum_content r
        | Pong | Ctrl (_ : int) -> read_one ~accum_content r
        | Text | Binary | Nonctrl (_ : int) ->
          if List.is_empty accum_content
          then
            if final then return (`Ok content) else read_one ~accum_content:[ content ] r
          else (
            let reason = "Bad frame in the middle of a fragmented message" in
            let info =
              Some
                (Info.create_s
                   [%sexp
                     "Expecting control or continuation frame"
                   , { frame : Frame.t
                     ; interrupted_msg = (finalise_content accum_content : string)
                     }])
            in
            close_cleanly ~code:Connection_close_reason.Protocol_error ~reason ~info ws;
            (* flush to make sure the close frame was sent to the client *)
            let%map () = Writer.flushed ws.writer in
            `Eof)
        | Continuation ->
          if List.is_empty accum_content
          then (
            close_cleanly
              ~code:Connection_close_reason.Protocol_error
              ~reason:
                "Received continuation message without a previous non-control frame to \
                 continue."
              ~info:(Some (Info.create_s [%sexp { frame : Frame.t }]))
              ws;
            (* flush to make sure the close frame was sent to the client *)
            let%map () = Writer.flushed ws.writer in
            `Eof)
          else (
            let accum_content = content :: accum_content in
            if final
            then return (`Ok (finalise_content accum_content))
            else read_one ~accum_content r)
      in
      if Ivar.is_full ws.closed
      then return `Eof
      else (
        match%bind Frame.read_frame r with
        | Error { Frame.Error.code; message } ->
          let info =
            if List.is_empty accum_content
            then None
            else
              Some
                (Info.create_s
                   [%sexp { partial_content : string = finalise_content accum_content }])
          in
          close_cleanly ~code ~reason:message ~info ws;
          return `Eof
        | Ok frame -> process_frame frame)
    in
    Reader.read_all ws.reader (read_one ~accum_content:[])
  ;;

  let send_pipe ~opcode ~masked (ws : raw) =
    let write_message msg =
      Frame.write_frame ws.writer ~masked (Frame.create ~opcode msg)
    in
    let to_client_r, to_client_w = Pipe.create () in
    let to_client_closed =
      Writer.transfer ws.writer to_client_r write_message
    in
    upon to_client_closed (fun () ->
      close_cleanly
        ~code:Connection_close_reason.Normal_closure
        ~reason:"Pipe was closed"
        ~info:None
        ws);
    to_client_w
  ;;
end

let magic_string = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let sec_websocket_accept_header_value ~sec_websocket_key =
  let module C = Crypto.Cryptokit in
  C.transform_string
    (C.Base64.encode_compact ())
    (C.hash_string (C.Hash.sha1 ()) (sec_websocket_key ^ magic_string))
  (* cryptokit leaves out a trailing equals sign *)
  ^ "="
;;

let close ~code ~reason ~masked (ws : raw) =
  Frame.write_frame ws.writer ~masked (Frame.create_close ~code reason);
  (* Wait for the writer to be flushed before actually closing it,
     otherwise the closing frame won't be sent. *)
  let%bind () = Writer.flushed ws.writer in
  let%bind () = Writer.close ws.writer in
  Reader.close ws.reader
;;

let close_finished { raw = { closed; writer; reader }; pipes = _ } =
  let%bind res = Ivar.read closed in
  (* Always close writers before readers due to the way TCP writers work *)
  let%bind () = Writer.close_finished writer in
  let%map () = Reader.close_finished reader in
  res
;;

let create ?(opcode = `Text) ~(role : Websocket_role.t) reader writer =
  let opcode =
    match opcode with
    | `Text -> Opcode.Text
    | `Binary -> Opcode.Binary
  in
  let masked = Websocket_role.should_mask role in
  let closed = Ivar.create () in
  let ws = { reader; writer; closed } in
  don't_wait_for
    (let%bind code, reason, _info = Ivar.read closed in
     close ~code:(Connection_close_reason.to_int code) ~reason ~masked ws);
  let reader = Pipes.recv_pipe ~masked ws in
  let writer = Pipes.send_pipe ~opcode ~masked ws in
  { pipes = reader, writer; raw = ws }
;;

let%expect_test "partial frame handling" =
  let write_frames frames =
    let fname = "frame.txt" in
    let%bind writer = Writer.open_file ~append:false fname in
    let () = List.iter ~f:(Frame.write_frame ~masked:false writer) frames in
    let%bind () = Writer.close writer in
    let%bind contents = Reader.file_contents fname in
    let%bind () = Unix.unlink fname in
    return contents
  in
  let read_partial ~len s =
    let fname = sprintf "content-%d" len in
    let%bind () = Writer.save fname ~contents:(String.sub s ~pos:0 ~len) in
    let%bind reader = Reader.open_file fname
    and writer = Writer.open_file "/dev/null" in
    let ws = create ~role:Server reader writer in
    let r, _w = pipes ws in
    let%bind q = Pipe.read_all r in
    let%bind code, reason, info = close_finished ws in
    print_s
      [%sexp
        { input_size = (len : int)
        ; content_read = (q : string Queue.t)
        ; close_code = (code : Connection_close_reason.t)
        ; close_reason = (reason : string)
        ; other_info = (info : (Info.t option[@sexp.omit_nil]))
        }];
    let%bind () = Reader.close reader
    and () = Writer.close writer in
    Unix.unlink fname
  in
  let print_all_partials frames =
    let%bind contents = write_frames frames in
    print_s [%sexp "full_contents", (contents : string)];
    let rec loop len =
      if len >= String.length contents
      then Deferred.unit
      else (
        let%bind () = read_partial contents ~len in
        loop (len + 1))
    in
    loop 0
  in
  let print_partial ~len frames =
    let%bind contents = write_frames frames in
    print_s [%sexp "full_contents", (contents : string)];
    read_partial contents ~len
  in
  let print_frames frames =
    let%bind contents = write_frames frames in
    print_s [%sexp "full_contents", (contents : string)];
    read_partial contents ~len:(String.length contents)
  in
  let text_frame ?final txt = Frame.create ?final ~opcode:Text txt in
  let continuation_frame ?final txt = Frame.create ?final ~opcode:Continuation txt in
  let close_frame txt = Frame.create_close ~code:2 txt in
  let%bind () = print_all_partials [ text_frame "hello"; close_frame "reason" ] in
  [%expect
    {|
    (full_contents "\129\005hello\136\b\000\002reason")
    ((input_size 0) (content_read ()) (close_code Protocol_error)
     (close_reason "Expected 2 byte header, got 0"))
    ((input_size 1) (content_read ()) (close_code Protocol_error)
     (close_reason "Expected 2 byte header, got 1"))
    ((input_size 2) (content_read ()) (close_code Protocol_error)
     (close_reason "Read 0 bytes, expected 5 bytes"))
    ((input_size 3) (content_read ()) (close_code Protocol_error)
     (close_reason "Read 1 bytes, expected 5 bytes"))
    ((input_size 4) (content_read ()) (close_code Protocol_error)
     (close_reason "Read 2 bytes, expected 5 bytes"))
    ((input_size 5) (content_read ()) (close_code Protocol_error)
     (close_reason "Read 3 bytes, expected 5 bytes"))
    ((input_size 6) (content_read ()) (close_code Protocol_error)
     (close_reason "Read 4 bytes, expected 5 bytes"))
    ((input_size 7) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Expected 2 byte header, got 0"))
    ((input_size 8) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Expected 2 byte header, got 1"))
    ((input_size 9) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 0 bytes, expected 8 bytes"))
    ((input_size 10) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 1 bytes, expected 8 bytes"))
    ((input_size 11) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 2 bytes, expected 8 bytes"))
    ((input_size 12) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 3 bytes, expected 8 bytes"))
    ((input_size 13) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 4 bytes, expected 8 bytes"))
    ((input_size 14) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 5 bytes, expected 8 bytes"))
    ((input_size 15) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 6 bytes, expected 8 bytes"))
    ((input_size 16) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Read 7 bytes, expected 8 bytes")) |}];
  let%bind () = print_frames [ text_frame "hello"; close_frame "reason" ] in
  [%expect
    {|
    (full_contents "\129\005hello\136\b\000\002reason")
    ((input_size 17) (content_read (hello)) (close_code Normal_closure)
     (close_reason "Received close message")
     (other_info
      (((frame ((opcode Close) (final true) (content "\000\002reason"))))))) |}];
  let%bind () = print_frames [ text_frame "hello"; text_frame "hello" ] in
  [%expect
    {|
    (full_contents "\129\005hello\129\005hello")
    ((input_size 14) (content_read (hello hello)) (close_code Protocol_error)
     (close_reason "Expected 2 byte header, got 0")) |}];
  let%bind () =
    print_frames
      [ text_frame ~final:false "hel"; continuation_frame "lo"; close_frame "reason" ]
  in
  [%expect
    {|
    (full_contents "\001\003hel\128\002lo\136\b\000\002reason")
    ((input_size 19) (content_read (hello)) (close_code Normal_closure)
     (close_reason "Received close message")
     (other_info
      (((frame ((opcode Close) (final true) (content "\000\002reason"))))))) |}];
  let%bind () = print_frames [ text_frame ~final:false "hello"; text_frame "bye" ] in
  [%expect
    {|
    (full_contents "\001\005hello\129\003bye")
    ((input_size 12) (content_read ()) (close_code Protocol_error)
     (close_reason "Bad frame in the middle of a fragmented message")
     (other_info
      (("Expecting control or continuation frame"
        ((frame ((opcode Text) (final true) (content bye)))
         (interrupted_msg hello)))))) |}];
  let%bind () =
    print_partial ~len:8 [ text_frame ~final:false "hello"; text_frame "bye" ]
  in
  [%expect
    {|
    (full_contents "\001\005hello\129\003bye")
    ((input_size 8) (content_read ()) (close_code Protocol_error)
     (close_reason "Expected 2 byte header, got 1")
     (other_info (((partial_content hello))))) |}];
  let%bind () = print_frames [ continuation_frame "hello" ] in
  [%expect
    {|
    (full_contents "\128\005hello")
    ((input_size 7) (content_read ()) (close_code Protocol_error)
     (close_reason
      "Received continuation message without a previous non-control frame to continue.")
     (other_info
      (((frame ((opcode Continuation) (final true) (content hello))))))) |}];
  Deferred.unit
;;
