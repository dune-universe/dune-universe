(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Dvi_util

type preamble = {
  pre_version : int;
  pre_num : int32;
  pre_den : int32;
  pre_mag : int32;
  pre_text : string;
}

type postamble = {
  last_page : int32;
  post_num : int32;
  post_den : int32;
  post_mag : int32;
  post_height : int32;
  post_width : int32;
  post_stack : int;
  post_pages : int;
}

type postpostamble = { postamble_pointer : int32; post_post_version : int }

type command =
  | SetChar of int32
  | SetRule of int32 * int32
  | PutChar of int32
  | PutRule of int32 * int32
  | Push
  | Pop
  | Right of int32
  | Wdefault
  | W of int32
  | Xdefault
  | X of int32
  | Down of int32
  | Ydefault
  | Y of int32
  | Zdefault
  | Z of int32
  | FontNum of int32
  | Special of string

type page = {
  counters : int32 array;
  previous : int32;
  commands : command list;
}

type fontmap = Dvi_util.font_def Int32Map.t

type t = {
  preamble : preamble;
  pages : page list;
  postamble : postamble;
  postpostamble : postpostamble;
  font_map : fontmap;
}

(* vf *)

type preamble_vf = {
  pre_vf_version : int;
  pre_vf_text : string;
  pre_vf_cs : int32;
  pre_vf_ds : float;
}

type char_desc = {
  char_code : int32;
  char_tfm : int32;
  char_commands : command list;
}

type vf = {
  vf_preamble : preamble_vf;
  vf_font_map : fontmap;
  vf_chars_desc : char_desc list;
}

(* *)

let fontmap d = d.font_map

module Print = struct
  open Format

  let preamble fmt p =
    fprintf fmt "* Preamble :\n";
    fprintf fmt "\tversion number = %d\n" p.pre_version;
    fprintf fmt "\tnumerator/denominator = %ld/%ld\n" p.pre_num p.pre_den;
    fprintf fmt "\tmagnification = %ld\n" p.pre_mag;
    fprintf fmt "\tcomment : %s\n" p.pre_text

  let fonts fmt fonts =
    fprintf fmt "* Fonts defined in this file :\n";
    Int32Map.iter (fun k f -> Print_font.font k fmt f) fonts

  let postamble fmt p =
    fprintf fmt "* Postamble :\n";
    fprintf fmt "\tlast page = %ld\n" p.last_page;
    fprintf fmt "\tnumerator/denominator = %ld/%ld\n" p.post_num p.post_den;
    fprintf fmt "\tmagnification = %ld\n" p.post_mag;
    fprintf fmt "\theight - width = %ld - %ld\n" p.post_height p.post_width;
    fprintf fmt "\tmaximum stack depth = %d\n" p.post_stack;
    fprintf fmt "\ttotal # of pages = %d\n" p.post_pages

  let postpostamble fmt p =
    fprintf fmt "* Postpostamble :\n";
    fprintf fmt "\tPostamble can be found at %ld.\n" p.postamble_pointer;
    fprintf fmt "\tDVI version : %d\n" p.post_post_version

  let commands fmt = function
    | SetChar c -> Format.fprintf fmt "Setting character %ld.@\n" c
    | SetRule (a, b) -> Format.fprintf fmt "Setting rule (w=%ld, h=%ld).@\n" a b
    | PutChar c -> Format.fprintf fmt "Putting character %ld.@\n" c
    | PutRule (a, b) -> Format.fprintf fmt "Putting rule (w=%ld, h=%ld).@\n" a b
    | Push -> Format.fprintf fmt "Push current state.@\n"
    | Pop -> Format.fprintf fmt "Pop current state.@\n"
    | Right b -> Format.fprintf fmt "Moving right %ld.@\n" b
    | Wdefault -> Format.fprintf fmt "Moving right by the default W.@\n"
    | W b -> Format.fprintf fmt "Moving right and changing W to %ld.@\n" b
    | Xdefault -> Format.fprintf fmt "Moving right by the default X.@\n"
    | X b -> Format.fprintf fmt "Moving right and changing X to %ld.@\n" b
    | Down a -> Format.fprintf fmt "Moving down %ld.@\n" a
    | Ydefault -> Format.fprintf fmt "Moving down by the default Y.@\n"
    | Y a -> Format.fprintf fmt "Moving down and changing Y to %ld.@\n" a
    | Zdefault -> Format.fprintf fmt "Moving down by the default Z.@\n"
    | Z a -> Format.fprintf fmt "Moving down and changing Z to %ld.@\n" a
    | FontNum f -> Format.fprintf fmt "Font is now set to %ld@\n" f
    | Special xxx -> Format.fprintf fmt "Special command : %s@\n" xxx

  let print_chars fmt c =
    Format.fprintf fmt "@[<hov 3>%ld : %a@]@\n" c.char_code
      (Misc.print_list Misc.newline commands)
      c.char_commands

  let print_chars_desc = Misc.print_list Misc.newline print_chars

  let print_vf fmt vf =
    Format.fprintf fmt "cs=%ld ds=%f %s@\n%a@\n%a@\n" vf.vf_preamble.pre_vf_cs
      vf.vf_preamble.pre_vf_ds vf.vf_preamble.pre_vf_text fonts vf.vf_font_map
      print_chars_desc vf.vf_chars_desc

  let page verbose fmt { counters = c; previous = prev; commands = cmds } =
    fprintf fmt "* Page number :";
    Array.iter (fun c -> fprintf fmt "%ld;" c) c;
    fprintf fmt "\n";
    fprintf fmt "\tPrevious page can be found at %ld\n" prev;
    if verbose then Misc.print_list Misc.newline commands fmt cmds
    else fprintf fmt "\t<list of commands skipped ...>"

  let pages verbose fmt =
    List.iter (fun p -> fprintf fmt "%a\n" (page verbose) p)

  let page_verb = page true

  let pages_verb = pages true

  let page = page false

  let pages = pages false

  let _doc name fmt doc =
    fprintf fmt "***********************\n";
    fprintf fmt "Reading DVI file : %s\n" name;
    fprintf fmt "%a%a%a%a%a" preamble doc.preamble pages doc.pages fonts
      doc.font_map postamble doc.postamble postpostamble doc.postpostamble
end

exception DviError of string

let dvi_error s = raise (DviError s)

let preamble bits =
  match%bitstring bits with
  | {| 247 : 8;              (* Preamble opcode *)
	version : 8;          (* DVI version *)
	num : 32 : bigendian; (* numerator *)
	den : 32 : bigendian; (* denominator *)
	mag : 32 : bigendian; (* magnification *)
	k : 8;                (* size of string x *)
	x : 8*k : string;     (* file comment *)
	bits : -1 : bitstring
      |}
    ->
      ( {
          pre_version = version;
          pre_num = num;
          pre_den = den;
          pre_mag = mag;
          pre_text = x;
        },
        bits )
  | {| _ : -1 : bitstring |} -> dvi_error "Ill-formed preamble"

let add_font k font map =
  if Int32Map.mem k map then dvi_error "Redefinition of font not allowed"
  else Int32Map.add k font map

let font_def bits =
  match%bitstring bits with
  | {| checksum : 32 : bigendian; (* checksum of the TMF file *)
	scale_factor : 32 : bigendian;    (* scale factor *)
	design_size : 32 : bigendian;   (* design size *)
	a : 8;                     (* size of the area *)
	l : 8;                     (* size of the filename *)
	name : (a+l)*8 : string;   (* the full name w/ area *)
	bits : -1 : bitstring
      |}
    ->
      ( mk_font_def ~checksum ~scale_factor ~design_size
          ~area:(String.sub name 0 a) ~name:(String.sub name a l),
        bits )
  | {| _ : -1 : bitstring |} -> dvi_error "Ill_formed font definition"

let page_counters bits =
  match%bitstring bits with
  | {| c0 : 32 : bigendian;
	c1 : 32 : bigendian;
	c2 : 32 : bigendian;
	c3 : 32 : bigendian;
	c4 : 32 : bigendian;
	c5 : 32 : bigendian;
	c6 : 32 : bigendian;
	c7 : 32 : bigendian;
	c8 : 32 : bigendian;
	c9 : 32 : bigendian;
	prev : 32 : bigendian;
	bits : -1 : bitstring
      |}
    ->
      ([| c0; c1; c2; c3; c4; c5; c6; c7; c8; c9 |], prev, bits)
  | {| _ : -1 : bitstring |} -> dvi_error "Ill-formed counters after bop"

let signed i j unsigned =
  if Int32.zero = Int32.logand unsigned i then unsigned
  else Int32.logor unsigned j

let signed_8 =
  signed
    (Int32.shift_left Int32.one 23)
    (Int32.logxor Int32.minus_one (Int32.of_int 0xff))

let signed_16 =
  signed
    (Int32.shift_left Int32.one 15)
    (Int32.logxor Int32.minus_one (Int32.of_int 0xffff))

let signed_24 =
  signed
    (Int32.shift_left Int32.one 23)
    (Int32.logxor Int32.minus_one (Int32.of_int 0xffffff))

let command bits =
  match%bitstring bits with
  (* Setting Characters *)
  | {| k : 8 ; bits : -1 : bitstring |} when 0 <= k && k <= 127 ->
      (SetChar (Int32.of_int k), bits)
  | {| 128 : 8; k : 8; bits : -1 : bitstring |} ->
      (SetChar (Int32.of_int k), bits)
  | {| 129 : 8; k : 16; bits : -1 : bitstring |} ->
      (SetChar (Int32.of_int k), bits)
  | {| 130 : 8; k : 24; bits : -1 : bitstring |} ->
      (SetChar (Int32.of_int k), bits)
  | {| 131 : 8; k : 32; bits : -1 : bitstring |} ->
      (SetChar k, bits) (* Setting a Rule *)
  | {| 132 : 8; a : 32; b: 32; bits : -1 : bitstring |} ->
      (SetRule (a, b), bits) (* Putting Characters *)
  | {| 133 : 8; k : 8; bits : -1 : bitstring |} ->
      (PutChar (Int32.of_int k), bits)
  | {| 134 : 8; k : 16; bits : -1 : bitstring |} ->
      (PutChar (Int32.of_int k), bits)
  | {| 135 : 8; k : 24; bits : -1 : bitstring |} ->
      (PutChar (Int32.of_int k), bits)
  | {| 136 : 8; k : 32; bits : -1 : bitstring |} ->
      (PutChar k, bits) (* Putting a Rule *)
  | {| 137 : 8; a : 32; b: 32; bits : -1 : bitstring |} ->
      (PutRule (a, b), bits) (* Stack operations *)
  | {| 141 : 8; bits : -1 : bitstring |} -> (Push, bits)
  | {| 142 : 8; bits : -1 : bitstring |} ->
      (Pop, bits)
      (* Moving to the right *)
      (* Must be signed but bitstring 2.0.0 fails*)
  | {| 143 : 8; b : 8 ; bits : -1 : bitstring |} ->
      (Right (signed_8 (Int32.of_int b)), bits)
  | {| 144 : 8; b : 16 ; bits : -1 : bitstring |} ->
      (Right (signed_16 (Int32.of_int b)), bits)
  | {| 145 : 8; b : 24 ; bits : -1 : bitstring |} ->
      (Right (signed_24 (Int32.of_int b)), bits)
  | {| 146 : 8; b : 32 ; bits : -1 : bitstring |} ->
      (Right b, bits) (* Moving/spacing to the right w *)
  | {| 147 : 8; bits : -1 : bitstring |} -> (Wdefault, bits)
  | {| 148 : 8; b : 8; bits : -1 : bitstring |} ->
      (W (signed_8 (Int32.of_int b)), bits)
  | {| 149 : 8; b : 16; bits : -1 : bitstring |} ->
      (W (signed_16 (Int32.of_int b)), bits)
  | {| 150 : 8; b : 24; bits : -1 : bitstring |} ->
      (W (signed_24 (Int32.of_int b)), bits)
  | {| 151 : 8; b : 32; bits : -1 : bitstring |} ->
      (W b, bits) (* Moving/spacing to the right x *)
  | {| 152 : 8; bits : -1 : bitstring |} -> (Xdefault, bits)
  | {| 153 : 8; b : 8; bits : -1 : bitstring |} ->
      (X (signed_8 (Int32.of_int b)), bits)
  | {| 154 : 8; b : 16; bits : -1 : bitstring |} ->
      (X (signed_16 (Int32.of_int b)), bits)
  | {| 155 : 8; b : 24; bits : -1 : bitstring |} ->
      (X (signed_24 (Int32.of_int b)), bits)
  | {| 156 : 8; b : 32; bits : -1 : bitstring |} ->
      (X b, bits) (* Moving down *)
  | {| 157 : 8; a : 8; bits : -1 : bitstring |} ->
      (Down (signed_8 (Int32.of_int a)), bits)
  | {| 158 : 8; a : 16; bits : -1 : bitstring |} ->
      (Down (signed_16 (Int32.of_int a)), bits)
  | {| 159 : 8; a : 24; bits : -1 : bitstring |} ->
      (Down (signed_24 (Int32.of_int a)), bits)
  | {| 160 : 8; a : 32; bits : -1 : bitstring |} ->
      (Down a, bits) (* Moving/spacing down y *)
  | {| 161 : 8; bits : -1 : bitstring |} -> (Ydefault, bits)
  | {| 162 : 8; a : 8; bits : -1 : bitstring |} ->
      (Y (signed_8 (Int32.of_int a)), bits)
  | {| 163 : 8; a : 16; bits : -1 : bitstring |} ->
      (Y (signed_16 (Int32.of_int a)), bits)
  | {| 164 : 8; a : 24; bits : -1 : bitstring |} ->
      (Y (signed_24 (Int32.of_int a)), bits)
  | {| 165 : 8; a : 32; bits : -1 : bitstring |} ->
      (Y a, bits) (* Moving/spacing down z *)
  | {| 166 : 8; bits : -1 : bitstring |} -> (Zdefault, bits)
  | {| 167 : 8; a : 8; bits : -1 : bitstring |} ->
      (Z (signed_8 (Int32.of_int a)), bits)
  | {| 168 : 8; a : 16; bits : -1 : bitstring |} ->
      (Z (signed_16 (Int32.of_int a)), bits)
  | {| 169 : 8; a : 24; bits : -1 : bitstring |} ->
      (Z (signed_24 (Int32.of_int a)), bits)
  | {| 170 : 8; a : 32; bits : -1 : bitstring |} ->
      (Z a, bits) (* Setting Fonts *)
  | {| k : 8 ; bits : -1 : bitstring |} when 171 <= k && k <= 234 ->
      (FontNum (Int32.of_int (k - 171)), bits)
  | {| 235 : 8; k : 8; bits : -1 : bitstring |} ->
      (FontNum (Int32.of_int k), bits)
  | {| 236 : 8; k : 16; bits : -1 : bitstring |} ->
      (FontNum (Int32.of_int k), bits)
  | {| 237 : 8; k : 24; bits : -1 : bitstring |} ->
      (FontNum (Int32.of_int k), bits)
  | {| 238 : 8; k : 32; bits : -1 : bitstring |} ->
      (FontNum k, bits) (* Special Commands *)
  | {| 239 : 8; k : 8; x : k * 8 : string; bits : -1 : bitstring |} ->
      (Special x, bits)
  | {| 240 : 8; k : 16; x : k * 8 : string; bits : -1 : bitstring |} ->
      (Special x, bits)
  | {| 241 : 8; k : 24; x : k * 8 : string; bits : -1 : bitstring |} ->
      (Special x, bits)
  | {| 242 : 8; k : 32; x : (Int32.to_int k) * 8 : string;
	bits : -1 : bitstring |}
    ->
      (Special x, bits)
  | {| _ : -1 : bitstring |} -> dvi_error "bad command !"

let rec page commands fonts bits =
  match%bitstring bits with
  | {| 140 : 8; bits : -1 : bitstring |} ->
      (* End of page opcode *)
      (commands, fonts, bits)
      (* nop opcode *)
  | {| 138 : 8; bits : -1 : bitstring |} ->
      page commands fonts bits (* font definitions *)
  | {| 243 : 8; k : 8; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      page commands (add_font (Int32.of_int k) font fonts) bits
  | {| 244 : 8; k : 16 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      page commands (add_font (Int32.of_int k) font fonts) bits
  | {| 245 : 8; k : 24 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      page commands (add_font (Int32.of_int k) font fonts) bits
  | {| 246 : 8; k : 32 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      page commands (add_font k font fonts) bits
      (* normal command *)
  | {| bits : -1 : bitstring |} ->
      let cmd, bits = command bits in
      page (cmd :: commands) fonts bits

let page = page []

let rec pages p fonts bits =
  match%bitstring bits with
  (* nop opcode *)
  | {| 138 : 8; bits : -1 : bitstring |} ->
      pages p fonts bits (* font definitions *)
  | {| 243 : 8; k : 8; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      pages p (add_font (Int32.of_int k) font fonts) bits
  | {| 244 : 8; k : 16 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      pages p (add_font (Int32.of_int k) font fonts) bits
  | {| 245 : 8; k : 24 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      pages p (add_font (Int32.of_int k) font fonts) bits
  | {| 246 : 8; k : 32 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      pages p (add_font k font fonts) bits
      (* begin page opcode *)
  | {| 139 : 8; bits : -1 : bitstring |} ->
      let counters, previous, bits = page_counters bits in
      let cmds, fonts, bits = page fonts bits in
      let newp = { counters; previous; commands = List.rev cmds } in
      (* Pages in reverse order *)
      pages (newp :: p) fonts bits
  | {| bits : -1 : bitstring |} -> (p, fonts, bits)

(* dvi_error "Expected : nop, font_definition, or new page" *)

let read_pages = pages []

let postamble bits =
  let rec skip_font_defs bits =
    match%bitstring bits with
    (* nop opcode *)
    | {| 138 : 8; bits : -1 : bitstring |} ->
        skip_font_defs bits (* font definitions *)
    | {| 243 : 8; _k : 8; bits : -1 : bitstring |} ->
        let _, bits = font_def bits in
        skip_font_defs bits
    | {| 244 : 8; _k : 16 : bigendian; bits : -1 : bitstring |} ->
        let _, bits = font_def bits in
        skip_font_defs bits
    | {| 245 : 8; _k : 24 : bigendian; bits : -1 : bitstring |} ->
        let _, bits = font_def bits in
        skip_font_defs bits
    | {| 246 : 8; _k : 32 : bigendian; bits : -1 : bitstring |} ->
        let _, bits = font_def bits in
        skip_font_defs bits
    | {| bits : -1 : bitstring |} -> bits
  in
  match%bitstring bits with
  | {| 248 : 8;                    (* Postamble opcode *)
	last_page : 32 : bigendian; (* DVI version *)
	num : 32 : bigendian;       (* numerator *)
	den : 32 : bigendian;       (* denominator *)
	mag : 32 : bigendian;       (* magnification *)
	height : 32 : bigendian;    (* tallest page *)
	width : 32 : bigendian;     (* widest page *)
	stack : 16 : bigendian;     (* stack depth *)
	pages : 16 : bigendian;     (* number of pages *)
	bits : -1 : bitstring
      |}
    ->
      ( {
          last_page;
          post_num = num;
          post_den = den;
          post_mag = mag;
          post_height = height;
          post_width = width;
          post_stack = stack;
          post_pages = pages;
        },
        skip_font_defs bits )
  | {| _ : -1 : bitstring |} -> dvi_error "Ill-formed postamble"

let postpostamble bits =
  let rec read_223 bits =
    match%bitstring bits with
    | {| 223 : 8;
	  rest : -1 : bitstring
	|} -> read_223 rest
    | {| rest : -1 : bitstring |} ->
        if Bitstring.bitstring_length rest = 0 then ()
        else dvi_error "Ill-formed suffix : only 223 expected."
  in
  match%bitstring bits with
  | {| 249 : 8;
	  postamble_pointer : 32 : bigendian;
	  version : 8;
	  rest : -1 : bitstring
	|}
    ->
      read_223 rest;
      { postamble_pointer; post_post_version = version }
  | {| _ : -1 : bitstring |} -> dvi_error "ill-formed postpostamble"

let read_file file =
  let bits = Bitstring.bitstring_of_file file in
  let preamble, bits = preamble bits in
  let pages, fonts, bits = read_pages Int32Map.empty bits in
  let postamble, bits = postamble bits in
  let postpostamble = postpostamble bits in
  {
    preamble;
    pages = List.rev pages;
    postamble;
    postpostamble;
    font_map = fonts;
  }

let vf_preamble bits =
  match%bitstring bits with
  | {| 247 : 8;              (* Preamble opcode *)
	version : 8;          (* VF version *)
	k : 8;                (* size of string x *)
	x : 8*k : string;     (* file comment *)
	cs : 32 : bigendian; (* denominator *)
	ds : 32 : bigendian; (* magnification *)
	bits : -1 : bitstring
      |}
    ->
      ( {
          pre_vf_version = version;
          pre_vf_text = x;
          pre_vf_cs = cs;
          pre_vf_ds = Int32.to_float ds /. (2. ** 20.);
        },
        bits )
  | {| _ : -1 : bitstring |} -> dvi_error "Ill-formed preamble"

(* Could factor the code with the one of page?*)
let rec preamble_fonts fonts bits =
  match%bitstring bits with
  (* font definitions *)
  | {| 243 : 8; k : 8; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      preamble_fonts (add_font (Int32.of_int k) font fonts) bits
  | {| 244 : 8; k : 16 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      preamble_fonts (add_font (Int32.of_int k) font fonts) bits
  | {| 245 : 8; k : 24 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      preamble_fonts (add_font (Int32.of_int k) font fonts) bits
  | {| 246 : 8; k : 32 : bigendian; bits : -1 : bitstring |} ->
      let font, bits = font_def bits in
      preamble_fonts (add_font k font fonts) bits
  | {| bits : -1 : bitstring |} -> (fonts, bits)

let rec command_list cmds bits =
  if Bitstring.bitstring_length bits = 0 then cmds
  else
    let cmd, bits = command bits in
    command_list (cmd :: cmds) bits

let rec vf_chars chars bits =
  match%bitstring bits with
  | {|248 : 8|} -> chars
  | {|242 : 8;
       pl  : 32;
       cc  : 32;
       tfm : 32;
       _ : (Int32.to_int pl)*8 : bitstring;
       bits : -1 : bitstring
      |}
    ->
      let char = { char_code = cc; char_tfm = tfm; char_commands = [] } in
      vf_chars (char :: chars) bits
  | {|pl : 8;
       cc  : 8;
       tfm : 24;
       coms : pl*8 : bitstring;
       bits : -1 : bitstring
      |}
    ->
      let coms = List.rev (command_list [] coms) in
      let char =
        {
          char_code = Int32.of_int cc;
          char_tfm = Int32.of_int tfm;
          char_commands = coms;
        }
      in
      vf_chars (char :: chars) bits
  | {|_ : 8|} -> dvi_error "vf : ill-formed character"

let print_vf = Print.print_vf

let read_vf_file file =
  let bits = Bitstring.bitstring_of_file file in
  let preamble, bits = vf_preamble bits in
  let fonts, bits = preamble_fonts Int32Map.empty bits in
  let chars_desc = List.rev (vf_chars [] bits) in
  { vf_preamble = preamble; vf_font_map = fonts; vf_chars_desc = chars_desc }

let get_conv_from_preamble p =
  let formule_magique_cm mag num den =
    Int32.to_float mag
    *. (Int32.to_float num /. Int32.to_float den)
    /. (10. ** 8.)
  in
  formule_magique_cm p.pre_mag p.pre_num p.pre_den

let get_conv doc = get_conv_from_preamble doc.preamble

let get_height_cm doc = get_conv doc *. Int32.to_float doc.postamble.post_height

let get_width_cm doc = get_conv doc *. Int32.to_float doc.postamble.post_width

let pages d = d.pages

let commands p = p.commands

module Incremental = struct
  type t = {
    mutable fonts : fontmap;
    mutable preamble : preamble;
    chan : in_channel;
    mutable bits : bytes * int * int;
  }

  let mk_t c =
    let bits = Bitstring.bitstring_of_chan c in
    let preamble, bits = preamble bits in
    let pgs, fonts, bits = read_pages Int32Map.empty bits in
    ({ fonts; preamble; bits; chan = c }, pgs)

  let next_pages t =
    let nextbits = Bitstring.bitstring_of_chan t.chan in
    let pgs, fonts, bits =
      read_pages t.fonts (Bitstring.concat [ t.bits; nextbits ])
    in
    t.fonts <- fonts;
    t.bits <- bits;
    pgs

  let get_conv i = get_conv_from_preamble i.preamble

  let font_map i = i.fonts
end
