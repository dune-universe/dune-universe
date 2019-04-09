(** Netstring implementation *)
open Lwt

let zero_ascii = int_of_char '0'

(** Consume and decode a netstring from a stream *)
let decode stream =
  let rec read_size size =
    (* Read in the size until we hit a ":" *)
    Lwt_stream.next stream >>= function
    | ':' ->
        Lwt.return size
    | '0' .. '9' as c ->
        read_size ((size * 10) + int_of_char c - zero_ascii)
    | _ ->
        Lwt.fail (Failure "Non-digit encountered in length")
  in
  let%lwt size = read_size 0 in
  (* Read in the string *)
  if size > Sys.max_string_length then Lwt.fail (Failure "Too big")
  else
    let%lwt chars = Lwt_stream.nget size stream in
    Lwt_stream.get stream >>= function
    | Some ',' ->
        let b = Buffer.create size in
        List.iter (Buffer.add_char b) chars ;
        Lwt.return (Buffer.contents b)
    | Some c ->
        Lwt.fail (Failure (Printf.sprintf "Expected comma, but got %c" c))
    | None ->
        Lwt.fail (Failure "Empty when comma expected")

(** Encode a netstring *)
let encode s = string_of_int (String.length s) ^ ":" ^ s ^ ","
