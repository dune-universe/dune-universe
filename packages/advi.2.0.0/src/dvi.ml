(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)

(* $Id$ *)

open Format;;
open Dvicommands;;

type page = {
   counters : int array ;
   commands : string;
  };;

type t = {
    preamble : preamble ;
    prelude : string ;
    pages : page array ;
    postamble : postamble ;
    font_map : (int * font_def) list
  };;

exception Error of string;;

let ident_byte = 2;;

(*** Buffer management (we keep the type [buffer] private) ***)

type buffer = {
    str : string ;
    len : int ;
    mutable pos : int
  };;

exception End_of_buffer;; (* private *)

let make_buffer str =
  { str = str ; len = String.length str ; pos = 0 };;

let read_byte buf =
  let pos = buf.pos
  and str = buf.str in
  if pos >= buf.len then
    raise End_of_buffer ;
  let n = Char.code str.[pos] in
  buf.pos <- pos + 1 ;
  n;;

let read_uint8 = read_byte;;

let read_int8 buf =
  let pos = buf.pos
  and str = buf.str in
  if pos >= buf.len then
    raise End_of_buffer ;
  let n = Char.code str.[pos] in
  buf.pos <- pos + 1 ;
  if n < 0x80 then n else n - 0x100;;

let read_uint16 buf =
  let pos = buf.pos
  and str = buf.str in
  if pos >= buf.len then
    raise End_of_buffer ;
  let n0 = Char.code str.[pos] in
  let n1 = Char.code str.[pos + 1] in
  buf.pos <- pos + 2 ;
  (n0 lsl 8) + n1;;

let read_int16 buf =
  let pos = buf.pos
  and str = buf.str in
  if pos + 2 > buf.len then
    raise End_of_buffer ;
  let n0 = Char.code str.[pos] in
  let n1 = Char.code str.[pos + 1] in
  buf.pos <- pos + 2 ;
  let n = (n0 lsl 8) + n1 in
  if n < 0x8000 then n else n - 0x10000;;

let read_uint24 buf =
  let pos = buf.pos
  and str = buf.str in
  if pos + 3 > buf.len then
    raise End_of_buffer ;
  let n0 = Char.code str.[pos] in
  let n1 = Char.code str.[pos + 1] in
  let n2 = Char.code str.[pos + 2] in
  buf.pos <- pos + 3 ;
  (n0 lsl 16) + (n1 lsl 8) + n2;;

let read_int24 buf =
  let pos = buf.pos
  and str = buf.str in
  if pos + 3 > buf.len then
    raise End_of_buffer ;
  let n0 = Char.code str.[pos] in
  let n1 = Char.code str.[pos + 1] in
  let n2 = Char.code str.[pos + 2] in
  buf.pos <- pos + 3 ;
  let n = (n0 lsl 16) + (n1 lsl 8) + n2 in
  if n < 0x800000 then n else n - 0x1000000;;

let arch32_read_int32 buf =
  let pos = buf.pos
  and str = buf.str in
  if pos + 4 > buf.len then
    raise End_of_buffer ;
  let n0 = Char.code str.[pos] in
  let n1 = Char.code str.[pos + 1] in
  let n2 = Char.code str.[pos + 2] in
  let n3 = Char.code str.[pos + 3] in
  buf.pos <- pos + 4 ;
  let n123 = (n1 lsl 16) + (n2 lsl 8) + n3 in
  match n0 lsr 6 with
  | 0|3 -> (n0 lsl 24) + n123
  | 1|2 ->
      if n0 = 0x80 && n123 = 0 then min_int
      else if n0 = 0x7f && n123 = 0xffffff then max_int
      else raise (Error "too large 32-bit integer") 
  | _ -> assert false;;

let arch64_read_int32 = 
  let v_0x80000000 = 0x8 lsl (4*7)
  and v_0x100000000 = 0x1 lsl (4*8)
  in
  fun buf ->
    let pos = buf.pos
    and str = buf.str in
    if pos + 4 > buf.len then
      raise End_of_buffer ;
    let n0 = Char.code str.[pos] in
    let n1 = Char.code str.[pos + 1] in
    let n2 = Char.code str.[pos + 2] in
    let n3 = Char.code str.[pos + 3] in
    buf.pos <- pos + 4 ;
    let n = (n0 lsl 24) + (n1 lsl 16) + (n2 lsl 8) + n3 in
    if n < v_0x80000000 then n else n - v_0x100000000;;

let read_int32 =
  match Sys.word_size with
  | 32 -> arch32_read_int32
  | 64 -> arch64_read_int32
  | _ -> assert false;;

let read_string buf len =
  let pos = buf.pos in
  if pos + len > buf.len then
    raise End_of_buffer ;
  let result = String.sub buf.str pos len in
  buf.pos <- pos + len ;
  result;;

(*** Reading the preamble ***)

let read_preamble buf =
  let num = read_int32 buf in
  let den = read_int32 buf in
  let mag = read_int32 buf in
  let len = read_uint8 buf in
  let text = read_string buf len in
  { pre_num = num ; pre_den = den ;
    pre_mag = mag ; pre_text = text };;

(*** Reading the postamble ***)

let read_postamble buf =
  let num = read_int32 buf in
  let den = read_int32 buf in
  let mag = read_int32 buf in
  let height = read_int32 buf in
  let width = read_int32 buf in
  let depth = read_uint16 buf in
  let pages = read_uint16 buf in
  { post_num = num ; post_den = den ; post_mag = mag ;
    post_height = height ; post_width = width ;
    post_depth = depth ; post_pages = pages };;

let finish_post_postamble buf =
  try while true do
    let byte = read_byte buf in
    if byte <> 223 then begin
      let msg =
        Printf.sprintf "bad post postamble byte (%d <> 223)" byte in
      raise (Error msg)
    end
  done with End_of_buffer -> ();;

(*** Reading a font definition ***)

let read_font_def buf =
  let chk = read_string buf 4 in
  let sfact = read_int32 buf in
  let dsize = read_int32 buf in
  let alen = read_uint8 buf in
  let nlen = read_uint8 buf in
  let astr = read_string buf alen in
  let nstr = read_string buf nlen in
  { checksum = chk ;
    scale_factor = sfact ; design_size = dsize ;
    area = astr ; name = nstr };;
    
(*** Reading commands ***)

let read_command buf =
  match read_byte buf with
  | n when n < 128 -> C_set n
  | 128 -> C_set (read_uint8 buf)
  | 129 -> C_set (read_uint16 buf)
  | 130 -> C_set (read_uint24 buf)
  | 131 -> C_set (read_int32 buf)
  | 132 ->
      let a = read_int32 buf in
      let b = read_int32 buf in
      C_set_rule(a, b)
  | 133 -> C_put (read_uint8 buf)
  | 134 -> C_put (read_uint16 buf)
  | 135 -> C_put (read_uint24 buf)
  | 136 -> C_put (read_int32 buf)
  | 137 ->
      let a = read_int32 buf in
      let b = read_int32 buf in
      C_put_rule(a, b)
  | 138 -> C_nop
  | 139 ->
      let c = Array.make 10 0 in
      for i = 0 to 9 do
        c.(i) <- read_int32 buf
      done ;
      C_bop(c, read_int32 buf)
  | 140 -> C_eop
  | 141 -> C_push
  | 142 -> C_pop
  | 143 -> C_right (read_int8 buf)
  | 144 -> C_right (read_int16 buf)
  | 145 -> C_right (read_int24 buf)
  | 146 -> C_right (read_int32 buf)
  | 147 -> C_w0
  | 148 -> C_w (read_int8 buf)
  | 149 -> C_w (read_int16 buf)
  | 150 -> C_w (read_int24 buf)
  | 151 -> C_w (read_int32 buf)
  | 152 -> C_x0
  | 153 -> C_x (read_int8 buf)
  | 154 -> C_x (read_int16 buf)
  | 155 -> C_x (read_int24 buf)
  | 156 -> C_x (read_int32 buf)
  | 157 -> C_down (read_int8 buf)
  | 158 -> C_down (read_int16 buf)
  | 159 -> C_down (read_int24 buf)
  | 160 -> C_down (read_int32 buf)
  | 161 -> C_y0
  | 162 -> C_y (read_int8 buf)
  | 163 -> C_y (read_int16 buf)
  | 164 -> C_y (read_int24 buf)
  | 165 -> C_y (read_int32 buf)
  | 166 -> C_z0
  | 167 -> C_z (read_int8 buf)
  | 168 -> C_z (read_int16 buf)
  | 169 -> C_z (read_int24 buf)
  | 170 -> C_z (read_int32 buf)
  | n when n >= 171 && n <= 234 -> C_fnt(n - 171)
  | 235 -> C_fnt (read_uint8 buf)
  | 236 -> C_fnt (read_uint16 buf)
  | 237 -> C_fnt (read_uint24 buf)
  | 238 -> C_fnt (read_int32 buf)
  | 239 ->
      let k = read_uint8 buf in
      C_xxx (read_string buf k)
  | 240 ->
      let k = read_uint16 buf in
      C_xxx (read_string buf k)
  | 241 ->
      let k = read_uint24 buf in
      C_xxx (read_string buf k)
  | 242 ->
      let k = read_int32 buf in
      C_xxx (read_string buf k)
  | 243 ->
      let k = read_uint8 buf in
      C_fnt_def(k, read_font_def buf)
  | 244 ->
      let k = read_uint16 buf in
      C_fnt_def(k, read_font_def buf)
  | 245 ->
      let k = read_uint24 buf in
      C_fnt_def(k, read_font_def buf)
  | 246 ->
      let k = read_int32 buf in
      C_fnt_def(k, read_font_def buf)
  | 247 ->
      let i = read_byte buf in
      if i <> ident_byte then begin
        let msg =
          Printf.sprintf
            "bad identification byte (%d <> %d) in the preamble"
            i ident_byte in
        raise (Error msg)
      end ;
      C_pre(read_preamble buf)
  | 248 ->
      let p = read_int32 buf in
      C_post(read_postamble buf, p)
  | 249 ->
      let q = read_int32 buf in
      let i = read_byte buf in
      if i <> ident_byte then begin
        let msg =
          Printf.sprintf
            "bad identification byte (%d <> %d) in the post postamble"
            i ident_byte in
        raise (Error msg)
      end ;
      finish_post_postamble buf ;          
      C_post_post q
  | 250 | 251 | 252 | 253 | 254 | 255 as byte ->
      let msg =
        Printf.sprintf "bad command opcode (%d)" byte in
      raise (Error msg)
  | _ -> assert false;;

(*** Reading an integer on an input channel ***)

let arch32_input_int32 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  let n3 = input_byte ch in
  match n0 lsr 6 with
  | 0|3 -> (n0 lsl 24) + (n1 lsl 16) + (n2 lsl 8) + n3
  | 1|2 -> raise (Error "too large 32-bit integer")
  | _ -> assert false;;

let arch64_input_int32 = 
  let v_0x80000000 = 0x8 lsl (4*7)
  and v_0x100000000 = 0x1 lsl (4*8)
  in
  fun ch ->
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  let n3 = input_byte ch in
  let n = (n0 lsl 24) + (n1 lsl 16) + (n2 lsl 8) + n3 in
  if n < v_0x80000000 then n else n - v_0x100000000;;

let input_int32 =
  match Sys.word_size with
  | 32 -> arch32_input_int32
  | 64 -> arch64_input_int32
  | _ -> assert false;;

let input_string ch len =
  let str = Bytes.create len in
  really_input ch str 0 len ;
  Bytes.to_string str;;

(*** Parsing commands ***)

let parse_string str =
  let buf = make_buffer str in
  let rec parse_rec () =
    if buf.pos < buf.len then
      read_command buf :: parse_rec ()
    else [] in
  try parse_rec ()
  with End_of_buffer ->
    raise (Error "input exhausted");;

(* parse commands of a page between C_bop and C_eop *)
let parse_page_of_commands commands =
  let buf = make_buffer commands in
  let rec parse_rec () =
    let cmd = read_command buf in
    if cmd = C_eop then [] else cmd :: parse_rec () in
  try
    match read_command buf with
    | C_bop _ -> parse_rec ()
    | _ -> raise (Error "ill-formed page")
  with End_of_buffer ->
    raise (Error "input exhausted");;

let parse_page page = parse_page_of_commands page.commands

let string_iter f str =
  let buf = make_buffer str in
  try
    while buf.pos < buf.len do
      f (read_command buf)
    done
  with End_of_buffer ->
    raise (Error "input exhausted");;

let page_iter f page =
  let buf = make_buffer page.commands in
  let rec iter_rec () =
    let cmd = read_command buf in
    if cmd <> C_eop then begin
      f cmd ;
      iter_rec ()
    end in
  try
    match read_command buf with
    | C_bop _ -> iter_rec ()
    | _ -> raise (Error "ill-formed page")
  with End_of_buffer ->
    raise (Error "input exhausted");;

let page_step f page =
  let buf = make_buffer page.commands in
  let rec iter_rec () =
    let cmd = read_command buf in
    if cmd <> C_eop then begin
      f cmd; true
    end else false
  in
  match read_command buf with
  | C_bop _ -> iter_rec
  | _ -> raise (Error "ill-formed page")
;;

(*** Reading a DVI file ***)

let input_dvi ch =
  seek_in ch 0 ;
  let file_len = in_channel_length ch in
  (* 1. We read the preamble *)
  let b = input_byte ch in
  let i = input_byte ch in
  if b <> 247 || i <> ident_byte then
    raise (Error "not a DVI file") ;
  let num = input_int32 ch in
  let den = input_int32 ch in
  let mag = input_int32 ch in
  let k = input_byte ch in
  let text = input_string ch k in
  let preamble =
    { pre_num = num ; pre_den = den ;
      pre_mag = mag ; pre_text = text }
  and prel_pos = pos_in ch in
  (* 2. We read the post-postamble *)
  let byte_at n =
    seek_in ch n ;
    input_byte ch in
  let pos = ref (file_len - 1) in
  while !pos >= prel_pos && byte_at !pos = 223 do decr pos done ;
  if !pos > file_len - 5 || !pos < prel_pos + 5 then
    raise (Error "not a DVI file") ;
  if byte_at !pos <> ident_byte then
    raise (Error "not a DVI file") ;
  seek_in ch (!pos - 5) ;
  if input_byte ch <> 249 then
    raise (Error "not a DVI file") ;
  let post_pos = input_int32 ch in
  (* 3. We read the postamble and the font map *)
  seek_in ch post_pos ;
  let post_str = input_string ch (file_len - post_pos) in
  assert (pos_in ch = in_channel_length ch) ;
  let buf = make_buffer post_str in
  let (postamble, last_page_pos) =
    match read_command buf with
    | C_post(post, p) -> (post, p)
    | _ -> raise (Error "not a DVI file") in
  let rec read_font_map () =
    match read_command buf with
    | C_nop -> read_font_map ()
    | C_fnt_def(n, def) -> (n, def) :: read_font_map ()
    | C_post_post _ -> []
    | _ -> raise (Error "command not allowed in the postamble") in
  let font_map = read_font_map () in
  (* 4. We read all the pages *)
  let stack = ref []
  and lim = ref post_pos
  and pos = ref last_page_pos in
  while !pos >= 0 do
    if !pos > !lim then
      raise (Error "not a DVI file") ;
    seek_in ch !pos ;
    let commands = input_string ch (!lim - !pos) in
    let buf = make_buffer commands in
    match read_command buf with
    | C_bop(counters, p) ->
	let page =
	  { counters = counters ;
	    commands = commands ;
          } in
        (*Misc.debug_endline page.text;*)
	stack := page :: !stack ;
	lim := !pos ;
	pos := p
    | _ -> raise (Error "not a DVI file")
  done ;
  (* 5. We read the prelude and build the structure *)
  if !pos <> -1 || !lim < prel_pos then
    raise (Error "not a DVI file") ;
  seek_in ch prel_pos ;
  let prelude = input_string ch (!lim - prel_pos) in
  let pages = Array.of_list !stack in
  { preamble = preamble; 
    prelude = prelude; 
    pages = pages; 
    postamble = postamble; 
    font_map = font_map }
;;

let magic_number len magic_string ch = 
  try 
    seek_in ch 0;
    let magic_number = input_string ch len in
(*     let convert s =  *)
(*       let l = [s.[0]; s.[1]; s.[2] ] in *)
(*       let l = List.map Char.code l in *)
(*       let l = List.map (Printf.sprintf "\\%03d") l in *)
(*       let s = String.concat " " l in *)
(*       s *)
(*     in *)
(*     Printf.eprintf "number:%s string:%s\n" *)
(*       (convert magic_number) (convert magic_string); *)
    magic_number = magic_string
  with
    End_of_file -> false

let gzipped ch = magic_number 3 "\031\139\008" ch
let bzipped ch = magic_number 3 "BZh" ch

let open_DVI filename =
  let ch = open_in_bin filename in
  let decompress = 
    if
      gzipped ch && Config.gunzip_path <> "" then
      Some (Printf.sprintf "%s -c" Config.gunzip_path)
    else if
      bzipped ch && Config.bunzip2_path <> "" then 
      Some (Printf.sprintf "%s -c" Config.bunzip2_path)
    else None in
  match decompress with
  | None ->
      ch
  | Some command ->
      close_in ch;
      let basename = Filename.basename filename in 
      let tmp_filename = Filename.temp_file basename ".dvi" in
      let cmd = Printf.sprintf "%s %s > %s" command filename tmp_filename in
      let ret = Sys.command cmd in
      if ret <> 0 then
        let () = Sys.remove tmp_filename in
        raise (Error "non DVI file cannot be decompressed")
      else
        let ch = open_in tmp_filename in
        let () = Sys.remove tmp_filename in
        ch

(*** Loading a DVI file ***)

let load filename =
  let ch = open_DVI filename in
  try
    let dvi = input_dvi ch in
    close_in ch;
    dvi
  with e ->
    close_in ch ;
    match e with
    | End_of_file -> raise (Error "not a DVI file")
    | _ -> raise e;;

(*** Pretty-printing ***)
let fprint_preamble fmt pre =
  fprintf fmt "@[<hov2>{ num = %d" pre.pre_num ;
  fprintf fmt "@ den = %d;" pre.pre_den ;
  fprintf fmt "@ mag = %d;" pre.pre_mag ;
  fprintf fmt "@ text = \"%s\" }@]" pre.pre_text;;

let fprint_postamble fmt post =
  fprintf fmt "@[<hov2>{ num = %d" post.post_num ;
  fprintf fmt "@ den = %d;" post.post_den ;
  fprintf fmt "@ mag = %d;" post.post_mag ;
  fprintf fmt "@ height = %d;" post.post_height ;
  fprintf fmt "@ width = %d;" post.post_width ;
  fprintf fmt "@ depth = %d;" post.post_depth ;
  fprintf fmt "@ pages = %d }@]" post.post_pages;;

let fprint_font_def fmt def =
  fprintf fmt "@[<hov2>{ checksum = %S;" def.checksum ;
  fprintf fmt "@ scale_factor = %d;" def.scale_factor ;
  fprintf fmt "@ design_size = %d;" def.design_size ;
  fprintf fmt "@ area = %S;" def.area ;
  fprintf fmt "@ name = %S }@]" def.name;;

let fprint_command fmt = function
  | C_set code ->
      if code >= 0 && code < 256 then
        fprintf fmt "set %C" (Char.chr code)
      else
        fprintf fmt "set %d" code
  | C_set_rule(a, b) -> fprintf fmt "set_rule %d %d" a b
  | C_put code ->
      if code >= 0 && code < 256 then
        fprintf fmt "put %C" (Char.chr code)
      else
        fprintf fmt "put %d" code
  | C_put_rule(a, b) -> fprintf fmt "put_rule %d %d" a b
  | C_nop -> fprintf fmt "nop"
  | C_bop(a, p) ->
      fprintf fmt "bop <%d, %d, %d, %d, %d, %d, %d, %d, %d, %d>"
        a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7) a.(8) a.(9) ;
      fprintf fmt " [prev=%x]" p
  | C_eop -> fprintf fmt "eop"
  | C_push -> fprintf fmt "push"
  | C_pop -> fprintf fmt "pop"
  | C_right n -> fprintf fmt "right %d" n
  | C_w0 -> fprintf fmt "w0"
  | C_w n -> fprintf fmt "w %d" n
  | C_x0 -> fprintf fmt "x0"
  | C_x n -> fprintf fmt "x %d" n
  | C_down n -> fprintf fmt "down %d" n
  | C_y0 -> fprintf fmt "y0"
  | C_y n -> fprintf fmt "y %d" n
  | C_z0 -> fprintf fmt "z0"
  | C_z n -> fprintf fmt "z %d" n
  | C_fnt n -> fprintf fmt "fnt %d" n
  | C_xxx s -> fprintf fmt "xxx %S" s
  | C_fnt_def(n, def) -> fprintf fmt "fnt_def %d %a" n fprint_font_def def
  | C_pre pre -> fprintf fmt "pre %a" fprint_preamble pre
  | C_post(post, p) ->
      fprintf fmt "post %a [prev=0x%x]"
        fprint_postamble post p
  | C_post_post q -> fprintf fmt "post_post [post=0x%x]" q;;
