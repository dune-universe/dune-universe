(* This file is translated from implementation of libcrc (https://github.com/lammertb/libcrc)
 *
 * The translation is done by Darren Ldl as part of the ocaml-SeqBox project
 *
 * The translated source code is under the same license as stated and used by the
 * original file (MIT License)
 *
 * Below is the original info text from crcccitt.c
 *
 * Library: libcrc
 * File:    src/crcccitt.c
 * Author:  Lammert Bies
 *
 * This file is licensed under the MIT License as stated below
 *
 * Copyright (c) 1999-2016 Lammert Bies
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Description
 * -----------
 * The module src/crcccitt.c contains routines which are used to calculate the
 * CCITT CRC values of a string of bytes.
 *)
open Stdint
open Fake_uint16

let crc_poly_ccitt = of_int 0x1021;;

let make_crcccitt_tab () : fuint16 array =
  let tab : fuint16 array = Array.make 256 (of_int 0) in
  let i   : fuint16 ref   = ref (of_int 0) in
  let crc : fuint16 ref   = ref (of_int 0) in
  let c   : fuint16 ref   = ref (of_int 0) in

  while !i < (of_int 256) do
    crc := of_int 0;
    c   := !i << 8;

    for _ = 0 to 7 do
      if ((!crc ^ !c) & (of_int 0x8000)) > (of_int 0) then
        crc := (!crc << 1) ^ crc_poly_ccitt
      else
        crc :=  !crc << 1;

      c := !c << 1
    done;

    tab.(to_int !i) <- !crc;

    (* increment *)
    add1 i
  done;

  tab
;;

let crc_tabccitt = make_crcccitt_tab ();;

let crc_ccitt_generic ~(input:string) ~(start_val:fuint16) : fuint16 =
  let crc         : fuint16 ref = ref start_val in
  let index       : int     ref = ref 0 in
  let len         : int         = String.length input in
  let mask_0x00FF : fuint16     = of_int 0x00FF in

  for _ = 0 to len - 1 do
    crc := (!crc << 8)
           ^
           crc_tabccitt.(
             to_int (
               ((!crc >> 8) ^ (of_char (String.get input !index)))
               &
               mask_0x00FF
             )
           );

    index := !index + 1
  done;

  !crc
;;

let crc_ccitt_generic_uint16 ~(input:string) ~(start_val:uint16) : uint16 =
  to_uint16 (
    crc_ccitt_generic ~input ~start_val:(of_uint16 start_val)
  )
;;
