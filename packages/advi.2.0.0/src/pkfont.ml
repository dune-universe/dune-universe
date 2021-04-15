(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

open Input;;

(*** Public types ***)

type bitmap =
  | Packed of int * string
  | Unpacked of string ;;

type char_def = {
    code : int ;
    tfm_width : int ;
    dx : int ;
    dy : int ;
    width : int ;
    height : int ;
    hoffset : int ;
    voffset : int ;
    (* The following field is declared as mutable for
       allowing in-place unpacking of the bitmap. *)
    mutable bitmap : bitmap
  } ;;

type t = {
    text : string ;
    design_size : int ;
    checksum : string ;
    hppp : int ;
    vppp : int ;
    defs : char_def list
  } ;;

(*** Reading a character definition ***)

let input_char_def ch flag =
  (* Loading the character parameters *)
  let (len, cdef) =
    match flag land 0x7 with
    | 0|1|2|3 ->
        (* Short format *)
	let pl = ((flag land 0x3) lsl 8) + input_uint8 ch in
	let cc = input_uint8 ch in
	let tfm = input_uint24 ch in
	let dm = input_uint8 ch in
	let w = input_uint8 ch in
	let h = input_uint8 ch in
	let hoff = input_int8 ch in
	let voff = input_int8 ch in
	let cdef =
	  { code = cc ; tfm_width = tfm ;
	    dx = dm lsl 16 ; dy = 0 ;
	    width = w ; height = h ;
	    hoffset = hoff ; voffset = voff ;
	    bitmap = Unpacked "" } in
	(pl - 8, cdef)
    | 4|5|6 ->
	(* Extended short format *)
	let pl = ((flag land 0x3) lsl 16) + input_uint16 ch in
	let cc = input_uint8 ch in
	let tfm = input_uint24 ch in
	let dm = input_uint16 ch in
	let w = input_uint16 ch in
	let h = input_uint16 ch in
	let hoff = input_int16 ch in
	let voff = input_int16 ch in
	let cdef =
	  { code = cc ; tfm_width = tfm ;
	    dx = dm lsl 16 ; dy = 0 ;
	    width = w ; height = h ;
	    hoffset = hoff ; voffset = voff ;
	    bitmap = Unpacked "" } in
	(pl - 13, cdef)
    | 7 ->
	(* Long format *)
	let pl = input_int32 ch in
	let cc = input_int32 ch in
	let tfm = input_int32 ch in
	let dx = input_int32 ch in
	let dy = input_int32 ch in
	let w = input_int32 ch in
	let h = input_int32 ch in
	let hoff = input_int32 ch in
	let voff = input_int32 ch in
	let cdef =
	  { code = cc ; tfm_width = tfm ;
	    dx = dx ; dy = dy ;
	    width = w ; height = h ;
	    hoffset = hoff ; voffset = voff ;
	    bitmap = Unpacked "" } in
	(pl - 28, cdef)
    | _ -> assert false in
  (* Loading the charater bitmap *)
  if flag lsr 4 = 14 then begin
    (* This is an unpacked bitmap *)
    if len <> (cdef.width * cdef.height + 7) lsr 3 then
      raise (Error "bad raster bitmap size") ;
    cdef.bitmap <- Unpacked(input_string ch len)
  end else begin
    (* This is a packed bitmap *)
    cdef.bitmap <- Packed(flag, input_string ch len)
  end ;
  cdef ;;

(*** Loading the body of the file ***)

let rec input_body ch =
  let flag = input_byte ch in
  if flag < 240 then begin
    let cdef = input_char_def ch flag in
    cdef :: input_body ch
  end else begin
    match flag with
    (* `pk_xxx' commands (specials).  We simply ignore them *)
    | 240 -> skip_bytes ch (input_uint8 ch) ; input_body ch
    | 241 -> skip_bytes ch (input_uint16 ch) ; input_body ch
    | 242 -> skip_bytes ch (input_uint24 ch) ; input_body ch
    | 243 -> skip_bytes ch (input_int32 ch) ; input_body ch
    (* `pk_yyy' command. Idem *)
    | 244 -> skip_bytes ch 4 ; input_body ch
    (* `pk_post' command (postamble).  This is the end *)
    | 245 ->
	begin
	  try
	    while input_byte ch = 246 do () done ;
	    raise (Error "invalid byte in the postamble")
	  with End_of_file -> []
	end
    (* `pk_no_op' command.  Nothing to do *)
    | 246 -> input_body ch
    (* `pk_pre' command (preamble).  This should not happen... *)
    | 247 -> raise (Error "preamble command found while reading the body")
    | _ -> raise (Error "unknown command")
  end ;;

(*** Loading a PK file ***)

let pk_pre = 247 ;;
let pk_id = 89 ;;

let input_font ch =
  let b = input_byte ch in
  let i = input_byte ch in
  if b <> pk_pre || i <> pk_id then
    raise (Error "not a PK file") ;
  let k = input_uint8 ch in
  let txt = input_string ch k in
  let ds = input_int32 ch in
  let cs = input_string ch 4 in
  let hppp = input_int32 ch in
  let vppp = input_int32 ch in
  let defs = input_body ch in
  { text = txt ;
    design_size = ds ;
    checksum = cs ;
    hppp = hppp ;
    vppp = vppp ;
    defs = defs } ;;

let load filename =
  let ch = open_in_bin filename in
  try
    let font = input_font ch in
    close_in ch ; font
  with e ->
    close_in ch ; raise e ;;

let find_char_def font code =
  let rec search = function
    | [] -> raise Not_found
    | cdef :: rest -> if cdef.code = code then cdef else search rest in
  search font.defs ;;

(*** Unpacking a character bitmap ***)

let unpack cdef =
  match cdef.bitmap with
  | Unpacked _ -> ()
  | Packed(flag, str) ->
      let dyn_f = flag lsr 4
      and len = String.length str
      and pos = ref 0
      and byte = ref (-1) in
      (* Reading a nybble (i.e. a 4-bit integer) *)
      let read_nyb () =
	if !byte >= 0 then begin
	  let lo = !byte land 0xf in
	  byte := -1 ; lo
	end else begin
	  if !pos = len then
	    raise (Error "nybble stream exhausted") ;
	  byte := Char.code str.[!pos] ;
	  incr pos ;
	  !byte lsr 4
	end in
      (* Reading a packed number *)
      let finish_big_pnum () =
	let j = ref 0 and k = ref 1 in
	while j := read_nyb () ; !j = 0 do incr k done ;
	while !k > 0 do j := (!j lsl 4) + read_nyb () ; decr k done ;
	!j - 15 + ((13 - dyn_f) lsl 4) + dyn_f in
      let read_pnum () =
	match read_nyb () with
	| 0 -> finish_big_pnum ()
	| 14 ->
	    let pnum =
	      match read_nyb () with
	      |	0 -> finish_big_pnum ()
	      |	14|15 -> raise (Error "two repeat counts in the same row")
	      |	i ->
		  if i <= dyn_f then i else
		  ((i - dyn_f - 1) lsl 4) + read_nyb () + dyn_f + 1 in
	    -pnum
	| 15 -> -1
	| i ->
	    if i <= dyn_f then i else
	    ((i - dyn_f - 1) lsl 4) + read_nyb () + dyn_f + 1 in
      (* The bitmap structure *)
      let w = cdef.width
      and h = cdef.height in
      let size = w * h in
      let datalen = (size + 7) lsr 3 in
      let data = Bytes.make datalen '\000'
      (* Index into the bitmap *)
      and i = ref 0 and imask = ref 0x80
      (* Index into the bitmap, [w] bits before *)
      and j = ref ((-w) asr 3)
      and jmask = ref (0x80 lsr ((-w) land 7)) in
      (* Sending a bit to the bitmap *)
      let send_bit b =
	if b then
	  Bytes.set data !i (Char.chr (Char.code (Bytes.get data !i) lor !imask)) ;
	imask := !imask lsr 1 ;
	if !imask = 0 then begin imask := 0x80 ; incr i end ;
	jmask := !jmask lsr 1 ;
	if !jmask = 0 then begin jmask := 0x80 ; incr j end in
      (* Resending n times the last line to the bitmap *)
      let resend_last_line n =
	for p = 1 to n * w do
	  if Char.code (Bytes.get data !j) land !jmask <> 0 then
	    Bytes.set data !i (Char.chr (Char.code (Bytes.get data !i) lor !imask)) ;
	  imask := !imask lsr 1 ;
	  if !imask = 0 then begin imask := 0x80 ; incr i end ;
	  jmask := !jmask lsr 1 ;
	  if !jmask = 0 then begin jmask := 0x80 ; incr j end
	done in
      (* Current coordinates into the bitmap *)
      let x = ref 0 and y = ref 0
      and black = ref ((flag land 8) = 8)
      and repeat_count = ref 0 in
      (* Filling the bitmap *)
      while !y < h do
	let pnum = read_pnum () in
	if pnum < 0 then begin
	  (* This is a repeat count *)
	  if !repeat_count > 0 then
	    raise (Error "two repeat counts in the same row") ;
	  repeat_count := -pnum
	end else begin
	  (* This is a run-length *)
	  for k = 1 to pnum do
	    if !i = datalen then
	      raise (Error "bitmap overflow") ;
	    send_bit !black ;
	    incr x ;
	    if !x = w then begin
	      x := 0 ;
	      incr y ;
	      if !repeat_count > 0 then begin
		y := !y + !repeat_count ;
		if !y > h then
		  raise (Error "bitmap overflow") ;
		resend_last_line !repeat_count ;
		repeat_count := 0
	      end
	    end
	  done ;
	  black := not !black
	end
      done ;
      cdef.bitmap <- Unpacked (Bytes.to_string data);;
