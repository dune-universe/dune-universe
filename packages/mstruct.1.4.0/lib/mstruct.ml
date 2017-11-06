(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type t = {
  mutable buffer: Cstruct.t;
}

let to_bigarray t =
  Cstruct.to_bigarray t.buffer

let of_bigarray ?off ?len ba =
  let buffer = Cstruct.of_bigarray ?off ?len ba in
  { buffer }

let of_string ?allocator s =
  let buffer = Cstruct.of_string ?allocator s in
  { buffer }

let to_string t =
  Cstruct.to_string t.buffer

let to_cstruct t = t.buffer

let of_cstruct buffer = { buffer }

let with_mstruct c f = f (of_cstruct c)

let length t =
  Cstruct.len t.buffer

let offset t =
  t.buffer.Cstruct.off

let hexdump t =
  Cstruct.hexdump t.buffer

let hexdump_to_buffer buf t =
  Cstruct.hexdump_to_buffer buf t.buffer

let debug t =
  Cstruct.debug t.buffer

exception Parse_error of string

let parse_error_buf buf fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31mParse error:\027[m %s\n" str;
      hexdump buf;
      raise (Parse_error str)
    ) fmt

let parse_error fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31mParse error:\027[m %s\n" str;
      raise (Parse_error str)
    ) fmt

let create len =
  let buffer = Cstruct.create len in
  { buffer }

let set t len fn c =
  fn t.buffer 0 c;
  t.buffer <- Cstruct.shift t.buffer len

let set_char t c =
  set t 1 Cstruct.set_char c

let set_uint8 t c =
  set t 1 Cstruct.set_uint8 c

let set_be_uint16 t c =
  set t 2 Cstruct.BE.set_uint16 c

let set_be_uint32 t c =
  set t 4 Cstruct.BE.set_uint32 c

let set_be_uint64 t c =
  set t 8 Cstruct.BE.set_uint64 c

let set_le_uint16 t c =
  set t 2 Cstruct.LE.set_uint16 c

let set_le_uint32 t c =
  set t 4 Cstruct.LE.set_uint32 c

let set_le_uint64 t c =
  set t 8 Cstruct.LE.set_uint64 c

let set_string t str =
  let len = String.length str in
  set t len (fun _ _ _ ->
      Cstruct.blit_from_string str 0 t.buffer 0 len;
    ) str

let get t n fn =
  let i = fn t.buffer 0 in
  t.buffer <- Cstruct.shift t.buffer n;
  i

let get_char t =
  get t 1 Cstruct.get_char

let get_uint8 t =
  get t 1 Cstruct.get_uint8

let get_be_uint16 t =
  get t 2 Cstruct.BE.get_uint16

let get_be_uint32 t =
  get t 4 Cstruct.BE.get_uint32

let get_be_uint64 t =
  get t 8 Cstruct.BE.get_uint64

let get_le_uint16 t =
  get t 2 Cstruct.LE.get_uint16

let get_le_uint32 t =
  get t 4 Cstruct.LE.get_uint32

let get_le_uint64 t =
  get t 8 Cstruct.LE.get_uint64

let get_string t len =
  if len = 0 then ""
  else
    let str = Bytes.create len in
    get t len (fun _ _ ->
        Cstruct.blit_to_string t.buffer 0 str 0 len;
      );
    Bytes.to_string str

let pick_string t len =
  if len = 0 then Some ""
  else if len > length t then None
  else (
    let str = Bytes.create len in
    Cstruct.blit_to_string t.buffer 0 str 0 len;
    Some (Bytes.to_string str)
  )

let index t c =
  let off = t.buffer.Cstruct.off in
  let n = t.buffer.Cstruct.len in
  let rec aux i =
    if i >= n then None
    else if t.buffer.Cstruct.buffer.{off+i} = c
    then Some i
    else aux (i+1) in
  aux 0

let sub t off len =
  let buffer = Cstruct.sub t.buffer off len in
  { buffer }

let shift t off =
  let buffer =
    Cstruct.of_bigarray
      ~off:(t.buffer.Cstruct.off + off)
      ~len:(t.buffer.Cstruct.len - off)
      t.buffer.Cstruct.buffer in
  t.buffer <- buffer

let clone t =
  let buffer = t.buffer in
  { buffer }

let with_delim t c =
  match index t c with
  | None   -> None
  | Some i -> Some (sub t 0 i)

let get_delim t0 c fn =
  match with_delim t0 c with
  | None    -> None
  | Some t1 ->
    let len = length t1 in
    let s = fn t1 in
    shift t0 (len + 1);
    Some s

let get_string_delim t c =
  get_delim t c (fun t -> get_string t (length t))
