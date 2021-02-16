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

open Format

type file_hdr = {
  lf : int;
  lh : int;
  bc : int;
  ec : int;
  nw : int;
  nh : int;
  nd : int;
  ni : int;
  nl : int;
  nk : int;
  ne : int;
  np : int;
}

type fix_word = float

type header = {
  checksum : int32;
  design_size : fix_word;
  coding_scheme : string option;
  identifier : string option;
  seven_bit_safe_flag : int option;
  face : int option;
}

type char_info_word = {
  width_index : int;
  height_index : int;
  depth_index : int;
  italic_index : int;
  tag : int;
  info_remainder : int;
}

type lig_kern_command = {
  skip_byte : int;
  next_char : int;
  op_byte : int;
  kern_remainder : int;
}

type extensible_recipe = { top : int; mid : int; bot : int; rep : int }

type body = {
  header : header;
  char_info : char_info_word array;
  width : fix_word array;
  height : fix_word array;
  depth : fix_word array;
  italic : fix_word array;
  lig_kern : lig_kern_command array;
  kern : fix_word array;
  exten : extensible_recipe array;
  param : fix_word array;
}

type t = { file_hdr : file_hdr; body : body }

module Print = struct
  let file_hdr fmt fh =
    fprintf fmt "File header :\n";
    fprintf fmt " lf=%d; lh=%d; bc=%d; ec=%d;\n" fh.lf fh.lh fh.bc fh.ec;
    fprintf fmt " nw=%d; nh=%d; nd=%d; ni=%d;\n" fh.nw fh.nh fh.nh fh.ni;
    fprintf fmt " nl=%d; nk=%d; ne=%d; np=%d;\n" fh.nl fh.nk fh.ne fh.np

  (*
  let wd fmt (wd : fix_word) =
    pp_print_float fmt wd

  let wds fmt a =
    Array.iteri (fun i c -> fprintf fmt "  %d : %a\n" i wd c) a

  let header fmt hdr =
    fprintf fmt " Header : Checksum = %lx; Design size = %a\n"
      hdr.checksum wd hdr.design_size

  let info fmt info =
    fprintf fmt " Char : Width = %d, Height = %d, Depth = %d, Italic = %d\n"
      info.width_index info.height_index info.depth_index info.italic_index;
    fprintf fmt "  tag = %d; remainder = %d\n" info.tag info.info_remainder

  let infos fmt infos =
    Array.iter (fun c -> fprintf fmt "%a" info c) infos

  let kern_cmd fmt kc =
    fprintf fmt " Lig Kern command : skip = %d, next = %d, op = %d, rem = %d\n"
      kc.skip_byte kc.next_char kc.op_byte kc.kern_remainder

  let kern_cmds fmt kcs =
    Array.iter (fun c -> fprintf fmt "%a" kern_cmd c) kcs

  let recipe fmt r =
    fprintf fmt " Recipe : top = %d, mid = %d, bot = %d, rep = %d\n"
      r.top r.mid r.bot r.rep

  let recipes fmt rs =
    Array.iter (fun c -> fprintf fmt "%a" recipe c) rs

  let body fmt body =
    fprintf fmt "Body : \n%a\n" header body.header;
    fprintf fmt "%a Widths:\n%a Heights:\n%a Depths:\n%a Italic:\n%a"
      infos body.char_info wds body.width wds body.height
      wds body.depth wds body.italic;
    fprintf fmt "%a Kerns:\n%a%a Params:\n%a"
      kern_cmds body.lig_kern wds body.kern recipes body.exten wds body.param

  let tfm name fmt {file_hdr = fh; body = bdy} =
    fprintf fmt "***********************\n";
    fprintf fmt "Reading Tex Font Metrics file : %s\n" name;
    fprintf fmt "%a%a" file_hdr fh body bdy
    *)
end

exception TfmError of string

let tfm_error s = raise (TfmError s)

let tfm_assert d a = if a then () else tfm_error d

let read_n dummy f n bits =
  let a = Array.make n dummy in
  let rec iter_until i bits =
    if i = n then bits
    else
      let wd, bits = f bits in
      a.(i) <- wd;
      iter_until (i + 1) bits
  in
  let bits = iter_until 0 bits in
  (a, bits)

let epsilon = 1. /. (2. ** 20.)

let fix_word bits =
  match%bitstring bits with
  | {| word : 32 : bigendian; bits : -1 : bitstring|} ->
      (Int32.to_float word *. epsilon, bits)
  | {| _ : -1 : bitstring |} -> tfm_error "ill-formed fix_word"

let read_n_fixwds = read_n 0. fix_word

let file_hdr bits =
  match%bitstring bits with
  | {| lf : 16 : unsigned, bigendian;
	lh : 16 : unsigned, bigendian;
	bc : 16 : unsigned, bigendian;
	ec : 16 : unsigned, bigendian;
	nw : 16 : unsigned, bigendian;
	nh : 16 : unsigned, bigendian;
	nd : 16 : unsigned, bigendian;
	ni : 16 : unsigned, bigendian;
	nl : 16 : unsigned, bigendian;
	nk : 16 : unsigned, bigendian;
	ne : 16 : unsigned, bigendian;
	np : 16 : unsigned, bigendian;
	bits : -1 : bitstring
      |}
    ->
      tfm_assert "number of characters" (bc - 1 <= ec && ec <= 255);
      tfm_assert "extensible character table too big" (ne <= 256);
      tfm_assert "total size constraint"
        (lf = 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np);
      ({ lf; lh; bc; ec; nw; nh; nd; ni; nl; nk; ne; np }, bits)
  | {| _ : -1 : bitstring |} -> tfm_error "ill formed header"

let header sz bits =
  match%bitstring bits with
  | {| checksum : 32 : bigendian; bits : -1 : bitstring |} -> (
      let design, bits = fix_word bits in
      match%bitstring bits with
      | {| _ : (sz-2)*32 : string; bits : -1 : bitstring |} ->
          ( {
              checksum;
              design_size = design;
              coding_scheme = None;
              identifier = None;
              seven_bit_safe_flag = None;
              face = None;
            },
            bits )
      | {| _ : -1 : bitstring |} -> tfm_error "ill-formed body header2" )
  | {| _ : -1 : bitstring |} -> tfm_error "ill-formed body header1"

let dummy_info =
  {
    width_index = 0;
    height_index = 0;
    depth_index = 0;
    italic_index = 0;
    tag = 0;
    info_remainder = 0;
  }

let char_info_word bits =
  match%bitstring bits with
  | {| width_idx : 8; height_idx : 4; depth_idx : 4;
	italic_idx : 6; tag : 2; remainder : 8;
	bits : -1 : bitstring
      |}
    ->
      ( {
          width_index = width_idx;
          height_index = height_idx;
          depth_index = depth_idx;
          italic_index = italic_idx;
          tag;
          info_remainder = remainder;
        },
        bits )
  | {| _ : -1 : bitstring |} -> tfm_error "ill-formed char info word"

let read_info_words = read_n dummy_info char_info_word

let kern_dummy =
  { skip_byte = 0; next_char = 0; op_byte = 0; kern_remainder = 0 }

let lig_kern_cmd bits =
  match%bitstring bits with
  | {| skip_byte : 8; next_char : 8; op_byte : 8; remainder : 8;
	bits : -1 : bitstring |}
    ->
      ({ skip_byte; next_char; op_byte; kern_remainder = remainder }, bits)
  | {| _ : -1 : bitstring |} -> tfm_error "ill-formed lig kern command"

let read_kern_cmds = read_n kern_dummy lig_kern_cmd

let recipe_dummy = { top = 0; mid = 0; bot = 0; rep = 0 }

let exten_recipe bits =
  match%bitstring bits with
  | {| top : 8; mid : 8; bot : 8; rep : 8;
	bits : -1 : bitstring |} ->
      ({ top; mid; bot; rep }, bits)
  | {| _ : -1 : bitstring |} -> tfm_error "ill-formed extensible recipe"

let read_recipes = read_n recipe_dummy exten_recipe

let body fh bits =
  let hdr, bits = header fh.lh bits in
  let infos, bits = read_info_words (fh.ec - fh.bc + 1) bits in
  let width, bits = read_n_fixwds fh.nw bits in
  let height, bits = read_n_fixwds fh.nh bits in
  let depth, bits = read_n_fixwds fh.nd bits in
  let italic, bits = read_n_fixwds fh.ni bits in
  let lig_kern, bits = read_kern_cmds fh.nl bits in
  let kern, bits = read_n_fixwds fh.nk bits in
  let exten, bits = read_recipes fh.ne bits in
  let param, bits = read_n_fixwds fh.np bits in
  if Bitstring.bitstring_length bits <> 0 then
    printf "Warning : ignored extra data after parameters.\n";
  {
    header = hdr;
    char_info = infos;
    width;
    height;
    depth;
    italic;
    lig_kern;
    kern;
    exten;
    param;
  }

let read_file file =
  let bits = Bitstring.bitstring_of_file file in
  let fh, bits = file_hdr bits in
  if Defaults.get_debug () then Print.file_hdr std_formatter fh;
  let body = body fh bits in
  { file_hdr = fh; body }

let design_size t = t.body.header.design_size
