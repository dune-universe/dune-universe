open! Core
open Async

type t =
  { opcode : Opcode.t
  ; final : bool
  ; content : string
  }
[@@deriving sexp_of]

module Error = struct
  type t =
    { code : Connection_close_reason.t
    ; message : string
    }
end

(* Extensions aren't implemented *)
let create ~opcode ?(final = true) content = { opcode; final; content }

(* See rfc6455 - 5.5.1
   The Close frame MAY contain a body (the "Application data" portion of
   the frame) that indicates a reason for closing, such as an endpoint
   shutting down, an endpoint having received a frame too large, or an
   endpoint having received a frame that does not conform to the format
   expected by the endpoint.  If there is a body, the first two bytes of
   the body MUST be a 2-byte unsigned integer (in network byte order)
   representing a status code with value /code/ defined in Section 7.4.
   Following the 2-byte integer, the body MAY contain UTF-8-encoded data
   with value /reason/, the interpretation of which is not defined by
   this specification.  This data is not necessarily human readable but
   may be useful for debugging or passing information relevant to the
   script that opened the connection.  As the data is not guaranteed to
   be human readable, clients MUST NOT show it to end users. *)
let create_close ~code ?final content =
  let len = String.length content in
  let content' = Bytes.create (len + 2) in
  Bytes.From_string.blit ~src:content ~src_pos:0 ~dst:content' ~dst_pos:2 ~len;
  Binary_packing.pack_unsigned_16_big_endian ~buf:content' ~pos:0 code;
  create ~opcode:Close ?final (Bytes.to_string content')
;;

let bit_is_set idx v = (v lsr idx) land 1 = 1
let set_bit v idx b = if b then v lor (1 lsl idx) else v land lnot (1 lsl idx)
let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)
let random_bytes ~len = Bytes.init len ~f:(fun _ -> Char.of_int_exn (Random.int 128))

let xor_with_mask mask msg =
  for i = 0 to Bytes.length msg - 1 do
    Bytes.set
      msg
      i
      (Char.of_int_exn
         (Char.to_int (Bytes.get mask (i mod 4)) lxor Char.to_int (Bytes.get msg i)))
  done
;;

let write_int16 writer n =
  let buf = Bytes.create 2 in
  Binary_packing.pack_unsigned_16_big_endian ~buf ~pos:0 n;
  Writer.write_bytes ~pos:0 ~len:2 writer buf
;;

let write_int64 writer n =
  let buf = Bytes.create 8 in
  Binary_packing.pack_signed_64_big_endian ~buf ~pos:0 n;
  Writer.write_bytes ~pos:0 ~len:8 writer buf
;;

let write_frame ~masked writer frame =
  if Writer.is_closed writer
  then ()
  else (
    let content = Bytes.of_string frame.content in
    let len = Bytes.length content in
    let opcode = Opcode.to_int frame.opcode in
    let payload_length =
      match len with
      | n when n < 126 -> len
      | n when n < 1 lsl 16 -> 126
      | _ -> 127
    in
    let hdr = 0 in
    let hdr = set_bit hdr 15 frame.final in
    let hdr = hdr lor (opcode lsl 8) in
    let hdr = set_bit hdr 7 masked in
    let hdr = hdr lor payload_length in
    let buf = Bytes.create 2 in
    Binary_packing.pack_unsigned_16_big_endian ~buf ~pos:0 hdr;
    Writer.write_bytes ~len:2 ~pos:0 writer buf;
    (match len with
     | n when n < 126 -> ()
     | n when n < 1 lsl 16 -> write_int16 writer n
     | n -> write_int64 writer (Int64.of_int n));
    if masked
    then (
      let mask = random_bytes ~len:4 in
      Writer.write_bytes ~pos:0 ~len:4 writer mask;
      xor_with_mask mask content);
    Writer.write_bytes ~pos:0 ~len writer content)
;;

let error ~code ~reason = Error { Error.code; message = reason }
let error_deferred ~code ~reason = Deferred.return (error ~code ~reason)

let read_int64 reader =
  let buf = Bytes.create 8 in
  match%map Reader.really_read reader ~len:8 buf with
  | `Ok -> Ok (Binary_packing.unpack_signed_64_big_endian ~buf ~pos:0)
  | `Eof _ ->
    error
      ~code:Connection_close_reason.Protocol_error
      ~reason:"Did not receive correct length"
;;

let read_int16 reader =
  let buf = Bytes.create 2 in
  match%map Reader.really_read reader ~len:2 buf with
  | `Ok -> Ok (Int64.of_int (Binary_packing.unpack_unsigned_16_big_endian ~buf ~pos:0))
  | `Eof _ ->
    error
      ~code:Connection_close_reason.Protocol_error
      ~reason:"Did not receive correct length"
;;


let read_frame reader =
  let buf = Bytes.create 2 in
  let open Deferred.Result.Let_syntax in
  match%bind Deferred.ok (Reader.really_read reader ~len:2 buf) with
  | `Eof n ->
    error_deferred
      ~code:Connection_close_reason.Protocol_error
      ~reason:(sprintf "Expected 2 byte header, got %d" n)
  | `Ok ->
    let header_part1 = Char.to_int (Bytes.get buf 0) in
    let header_part2 = Char.to_int (Bytes.get buf 1) in
    let final = bit_is_set 7 header_part1 in
    let opcode = Opcode.of_int (int_value 0 4 header_part1) in
    let masked = bit_is_set 7 header_part2 in
    let length = int_value 0 7 header_part2 in
    let%bind payload_length =
      match length with
      | 126 -> read_int16 reader
      | 127 -> read_int64 reader
      | i when i < 126 -> return (Int64.of_int i)
      | n ->
        error_deferred
          ~code:Connection_close_reason.Protocol_error
          ~reason:(sprintf "Invalid payload length %d" n)
    in
    let payload_length = Int64.to_int_exn payload_length in
    let mask = Bytes.create 4 in
    let%bind () =
      if masked
      then (
        match%bind Deferred.ok (Reader.really_read reader ~len:4 mask) with
        | `Ok -> return ()
        | `Eof n ->
          error_deferred
            ~code:Connection_close_reason.Protocol_error
            ~reason:(sprintf "Expected 4 byte mask, got %d" n))
      else return ()
    in
    let content = Bytes.create payload_length in
    (match%bind
       if payload_length = 0
       then return `Ok
       else Deferred.ok (Reader.really_read reader ~len:payload_length content)
     with
     | `Ok ->
       if masked then xor_with_mask mask content;
       return (create ~opcode ~final (Bytes.to_string content))
     | `Eof n ->
       error_deferred
         ~code:Connection_close_reason.Protocol_error
         ~reason:(sprintf "Read %d bytes, expected %d bytes" n payload_length))
;;
