(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(**************************************************************************)

(**
    @author Sylvain Le Gall
  *)

open GettextTypes

let int32_of_byte (a0, a1, a2, a3) =
  Int32.add
    (Int32.shift_left (Int32.of_int a0) 24)
    (Int32.of_int ((a1 lsl 16) + (a2 lsl 8) + a3))

let byte_of_int32 i =
  let one_byte = Int32.of_int 0xFF in
  let extract_byte sb =
    let mask = Int32.shift_left one_byte (sb * 8) in
    let i_masked = Int32.logand i mask in
    Int32.to_int (Int32.shift_right i_masked (sb * 8))
  in
  (extract_byte 3, extract_byte 2, extract_byte 1, extract_byte 0)

let input_int32 chn endian =
  let a0, a1, a2, a3 =
    (input_byte chn, input_byte chn, input_byte chn, input_byte chn)
  in
  match endian with
  | BigEndian -> int32_of_byte (a0, a1, a2, a3)
  | LittleEndian -> int32_of_byte (a3, a2, a1, a0)

let output_int32 chn endian vl =
  let a0, a1, a2, a3 = byte_of_int32 vl in
  let order =
    match endian with
    | BigEndian -> [ a0; a1; a2; a3 ]
    | LittleEndian -> [ a3; a2; a1; a0 ]
  in
  List.iter (output_byte chn) order

let input_int32_pair chn endian =
  let a = input_int32 chn endian in
  let b = input_int32 chn endian in
  (a, b)

let output_int32_pair chn endian (a, b) =
  output_int32 chn endian a;
  output_int32 chn endian b

let input_int32_pair_string chn endian =
  let length, offset = input_int32_pair chn endian in
  let ilength, ioffset = (Int32.to_int length, Int32.to_int offset) in
  if 0 <= ioffset + ilength && ioffset + ilength < in_channel_length chn then (
    let str = Bytes.make ilength 'X' in
    seek_in chn ioffset;
    really_input chn str 0 ilength;
    Bytes.to_string str )
  else
    (* We use this exception, because that what should happen if we try to
        read the string *)
    raise End_of_file
