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

(* jfm files are like tfm (tex font metric) files, but for japanese
   characters *)

open Input;;

type jfm_id = Horizontal | Vertical;;

type preamble = {
    id : jfm_id;
    nt : int; (* char_type table in words (4bytes) *)
    lf : int; (* file length *)
    lh : int; (* header length *)
    bc : int; (* minimum char code (must be 0 in jfm) *)
    ec : int; (* maximum char code *)
    nw : int; (* size of width table *)
    nh : int; (* size of height table *)
    nd : int; (* size of depth table *)
    ni : int; (* size of italic table *)
    nl : int; (* size of glue/kern table *)
    nk : int; (* size of kern table *)
    ng : int; (* size of glue table *)
    np : int; (* size of param table *)
  };;

let input_int32_as_float ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  let n3 = input_byte ch in
  float n0 *. 16777216.0 +. float n1 *. 65536.0 +.
    float n2 *. 256.0 +. float n3;;

let input_preamble ic =
  let id_int = input_int16 ic in
  let nt = input_int16 ic in
  let lf = input_int16 ic in
  let lh = input_int16 ic in
  let bc = input_int16 ic in
  let ec = input_int16 ic in
  let nw = input_int16 ic in
  let nh = input_int16 ic in
  let nd = input_int16 ic in
  let ni = input_int16 ic in
  let nl = input_int16 ic in
  let nk = input_int16 ic in
  let ng = input_int16 ic in
  let np = input_int16 ic in
  let id =
    match id_int with
    | 11 -> Horizontal
    | 9 -> Vertical
    | _ -> assert false in
  if bc <> 0 ||
     lf = 7 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ng + np
  then failwith "invalid jfm file" else
  { id=id; nt=nt; lf=lf; lh=lh; bc=bc; ec=ec; nw=nw; nh=nh; nd=nd;
    ni=ni; nl=nl; nk=nk; ng=ng; np=np };;

let input_floats ic num =
  let array = Array.make num 0.0 in
  for i = 0 to num - 1 do
    array.(i) <- input_int32_as_float ic
  done;
  array;;

let input_words ic num =
  let array = Array.make num 0 in
  for i = 0 to num - 1 do
    array.(i) <- input_int32 ic
  done;
  array;;

type weight = Medium | Bold | Light;;
type tai = Seitai | Choutai | Heitai (* ? *);;

type header = {
    checksum : string;
    design_size : int;
    coding_schema : string option;
    font_family : string option;
    seven_bit_safe_flag : int option;
    weight : weight option;
    slant : bool option;
    tai : tai option;
  };;

let string_of_header h =
  let b = Buffer.create 100 in
  Buffer.add_string b "{checksum=";
  Buffer.add_string b h.checksum;
  Buffer.add_string b "; ";
  Buffer.add_string b "design_size=";
  Buffer.add_string b (string_of_int h.design_size);
  Buffer.add_string b "; ";
  begin match h.coding_schema with
  | Some x ->
    Buffer.add_string b "coding_schema=";
    Buffer.add_string b x;
    Buffer.add_string b "; ";
  | None -> ()
  end;
  begin match h.font_family with
  | Some x ->
    Buffer.add_string b "font_family=";
    Buffer.add_string b x;
    Buffer.add_string b "; ";
  | None -> ()
  end;
  Buffer.contents b;;

let input_header ic len =
  let read = ref 0 in
  if len < 2 then failwith "too short header";
  let cs = input_string ic 4 in
  let ds = input_int32 ic in
  read := !read + 2;
  let coding_schema =
    if len >= 12 then begin
      let l = input_int8 ic in
      let s = input_string ic 39 in
      read := !read + 10;
      Some (String.sub s 0 l)
    end else None in
  let font_family =
    if len >= 17 then begin
      read := !read + 5;
      Some (input_string ic 20)
    end else None in
  let sbsf, w, s, t =
    if len >= 18 then begin
      read := !read + 1;
      let sbsf = input_int8 ic in
      let _ = input_int8 ic in
      let _ = input_int8 ic in
      let face = input_int8 ic in
      let s = face mod 2 <> 0 in
      let w =
        match face / 2 mod 3 with
        | 0 -> Medium
        | 1 -> Bold
        | 2 -> Light
        | _ -> assert false in
      let t =
        match face / 2 / 3 mod 3 with
        | 0 -> Seitai
        | 1 -> Choutai
        | 2 -> Heitai
        | _ -> assert false in
      Some sbsf, Some w, Some s, Some t
    end else None, None, None, None in
  if len - !read > 0 then ignore (input_string ic (len - !read));
  { checksum = cs;
    design_size = ds;
    coding_schema = coding_schema;
    font_family = font_family;
    seven_bit_safe_flag = sbsf;
    weight = w;
    slant = s;
    tai = t; };;

let input_char_types ic len =
  let tbl = Hashtbl.create 257 in
  for i = 0 to len - 1 do
    let kcode = input_int16 ic in
    let chartype = input_int16 ic in
    Hashtbl.add tbl kcode chartype
  done;
  tbl;;

type char_info = {
    width_index : int;
    height_index : int;
    depth_index : int;
    italic_index : int;
    tag : int;
    remainder : int;
  };;

let input_char_infos ic bc ec = (* bc must be 0 *)
  let input_char_info ic =
    let width_index = input_uint8 ic in
    let height_depth = input_uint8 ic in
    let height_index = height_depth lsr 4 in
    let depth_index = height_depth land 15 in
    let italic_tag = input_uint8 ic in
    let italic_index = italic_tag lsr 2 in
    let tag = italic_tag land 3 in
    let remainder = input_uint8 ic in
    { width_index = width_index;
      height_index = height_index;
      depth_index = depth_index;
      italic_index = italic_index;
      tag = tag;
      remainder = remainder
    } in
  Array.init (ec - bc + 1) (fun _ -> input_char_info ic);;

type jfm =
    { preamble : preamble;
      header : header;
      char_types : (int,int) Hashtbl.t;
      char_infos : char_info array;
      widths : int array;
      heights : int array;
      depths : int array;
      italics : int array;
      gluekerns : string;
      glues : int array;
      kerns : int array;
      params : string;
  };;

let input_jfm ic =
  let pre = input_preamble ic in
  let header = input_header ic pre.lh in
  let char_types = input_char_types ic pre.nt in
  let char_infos = input_char_infos ic pre.bc pre.ec in
  let widths = input_words ic pre.nw in
  let heights = input_words ic pre.nh in
  let depths = input_words ic pre.nd in
  let italics = input_words ic pre.ni in
  let gluekerns = input_string ic (pre.nl * 4) in
  let glues = input_words ic pre.nk in
  let kerns = input_words ic pre.ng in
  let params = input_string ic (pre.np * 4) in
  { preamble = pre;
    header = header;
    char_types = char_types;
    char_infos = char_infos;
    widths = widths;
    heights = heights;
    depths = depths;
    italics = italics;
    gluekerns = gluekerns;
    glues = glues;
    kerns = kerns;
    params = params
  };;

let load_jfm_file filename =
  let ic = open_in_bin filename in
  let jfm = input_jfm ic in
  begin try
    ignore (input_byte ic); raise Exit
  with
  | End_of_file -> ()
  | Exit -> failwith "jfm loading failed"
  end;
  close_in ic;
(*
  prerr_endline "done";
  Hashtbl.iter (fun k v ->
    prerr_endline (Printf.sprintf "%c%c %x %f\n"
             (char_of_int (k / 256 + 0x80)) (char_of_int (k mod 256 + 0x80))
             k
             jfm.widths.(jfm.char_infos.(v).width_index))) jfm.char_types; *)
  jfm;;

let find_width jfm kcode =
  let char_type = try Hashtbl.find jfm.char_types kcode with Not_found -> 0 in
  let char_info = jfm.char_infos.(char_type + jfm.preamble.bc) in
(* prerr_endline (Printf.sprintf "%c%c %d"
             (char_of_int (kcode / 256 + 0x80))
             (char_of_int (kcode mod 256 + 0x80))
             jfm.widths.(char_info.width_index)); *)
  jfm.widths.(char_info.width_index);;


(* x fix of monospace fonts like true type fonts.
   borrowed from vftools (@ ftp://ftp.math.s.chiba-u.ac.jp/tex).
*)
let monospace_fix = [
    0x2122,        0.0000;                (* ¡¢ *)
    0x2123,        0.0000;                (* ¡£ *)
    0x2124,        0.0000;                (* ¡¤ *)
    0x2125,        0.0000;                (* ¡¥ *)
    0x2126,        (-304.2760);        (* ¡¦ *)
    0x2127,        (-304.2760);        (* ¡§ *)
    0x2128,        (-304.2760);        (* ¡¨ *)
    0x2129,        (-229.1010);        (* ¡© *)
    0x212A,        (-304.2760);        (* ¡ª *)
    0x212B,        0.0000;                (* ¡« *)
    0x212C,        0.0000;                (* ¡¬ *)
    0x212D,        (-304.2760);        (* ¡­ *)
    0x212E,        (-304.2760);        (* ¡® *)
    0x212F,        (-229.1010);        (* ¡¯ *)
    0x2130,        (-229.1010);        (* ¡° *)
    0x2133,        (-229.1010);        (* ¡³ *)
    0x2135,        (-229.1010);        (* ¡µ *)
    0x2136,        (-107.3910);        (* ¡¶ *)
    0x2137,        (-107.3910);        (* ¡· *)
    0x2139,        (-107.3910);        (* ¡¹ *)
    0x213E,        (-304.2760);        (* ¡¾ *)
    0x2142,        (-304.2760);        (* ¡Â *)
    0x2143,        (-304.2760);        (* ¡Ã *)
    0x2146,        (-608.5510);        (* ¡Æ *)
    0x2147,        0.0000;                (* ¡Ç *)
    0x2148,        (-458.2030);        (* ¡È *)
    0x2149,        0.0000;                (* ¡É *)
    0x214A,        (-458.2030);        (* ¡Ê *)
    0x214B,        0.0000;                (* ¡Ë *)
    0x214C,        (-458.2030);        (* ¡Ì *)
    0x214D,        0.0000;                (* ¡Í *)
    0x214E,        (-458.2030);        (* ¡Î *)
    0x214F,        0.0000;                (* ¡Ï *)
    0x2150,        (-458.2030);        (* ¡Ð *)
    0x2151,        0.0000;                (* ¡Ñ *)
    0x2152,        (-458.2030);        (* ¡Ò *)
    0x2153,        0.0000;                (* ¡Ó *)
    0x2154,        (-458.2030);        (* ¡Ô *)
    0x2155,        0.0000;                (* ¡Õ *)
    0x2156,        (-458.2030);        (* ¡Ö *)
    0x2157,        0.0000;                (* ¡× *)
    0x2158,        (-458.2030);        (* ¡Ø *)
    0x2159,        0.0000;                (* ¡Ù *)
    0x215A,        (-458.2030);        (* ¡Ú *)
    0x215B,        0.0000;                (* ¡Û *)
    0x2168,        (-107.3910);        (* ¡è *)
    0x2169,        (-107.3910);        (* ¡é *)
    0x216A,        (-107.3910);        (* ¡ê *)
    0x216B,        0.0000;                (* ¡ë *)
    0x216C,        0.0000;                (* ¡ì *)
    0x216D,        0.0000;                (* ¡í *)
    0x2170,        (-107.3910);        (* ¡ð *)
    0x2171,        (-107.3910);        (* ¡ñ *)
    0x2172,        (-107.3910);        (* ¡ò *)
    0x2178,        (-229.1010);        (* ¡ø *)
    0x2421,        (-107.3910);        (* ¤¡ *)
    0x2423,        (-107.3910);        (* ¤£ *)
    0x2425,        (-107.3910);        (* ¤¥ *)
    0x2427,        (-107.3910);        (* ¤§ *)
    0x2429,        (-107.3910);        (* ¤© *)
    0x2443,        (-107.3910);        (* ¤Ã *)
    0x2463,        (-107.3910);        (* ¤ã *)
    0x2465,        (-107.3910);        (* ¤å *)
    0x2467,        (-107.3910);        (* ¤ç *)
    0x246E,        (-107.3910);        (* ¤î *)
    0x2521,        (-107.3910);        (* ¥¡ *)
    0x2523,        (-107.3910);        (* ¥£ *)
    0x2525,        (-107.3910);        (* ¥¥ *)
    0x2527,        (-107.3910);        (* ¥§ *)
    0x2529,        (-107.3910);        (* ¥© *)
    0x2543,        (-107.3910);        (* ¥Ã *)
    0x2563,        (-107.3910);        (* ¥ã *)
    0x2565,        (-107.3910);        (* ¥å *)
    0x2567,        (-107.3910);        (* ¥ç *)
    0x256E,        (-107.3910);        (* ¥î *)
    0x2575,        (-107.3910);        (* ¥õ *)
    0x2576,        (-107.3910);        (* ¥ö *)
];;
