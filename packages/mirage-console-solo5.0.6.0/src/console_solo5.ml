(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
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
 *)

external solo5_console_write:
  Cstruct.buffer -> int -> unit = "mirage_solo5_console_write"

(* TODO everything connects to the same console for now *)
(* TODO management service for logging *)
type t = {
  id: string;
  read_buffer: Cstruct.t;
  mutable closed: bool;
}

type 'a io = 'a Lwt.t
type buffer = Cstruct.t

type error
let pp_error _ (_:error) = assert false
type write_error = Mirage_flow.write_error
let pp_write_error = Mirage_flow.pp_write_error

let connect id =
  let read_buffer = Cstruct.create 0 in
  let closed = false in
  let t = { id; read_buffer; closed } in
  Lwt.return t

let disconnect _t = Lwt.return_unit

let read _t = Lwt.return @@ Ok `Eof

let write_one buf =
  solo5_console_write buf.Cstruct.buffer buf.Cstruct.len;
  Lwt.return (Ok ())

let write t buf =
  if t.closed then
    Lwt.return @@ Error `Closed
  else
    write_one buf

let writev t = function
  | []       -> Lwt.return (Ok ())
  | [buffer] -> write t buffer
  | buffers  ->
    write t @@ Cstruct.concat buffers

let close t =
  t.closed <- true;
  Lwt.return ()

let log t s =
  if t.closed then
    Lwt.return_unit
  else
    let buf = (Cstruct.of_string (s ^ "\n")) in
    ignore (write_one buf);
    Lwt.return_unit
