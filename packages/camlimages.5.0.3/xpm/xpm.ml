(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xpm.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open Images
open Color
open Util

external read : string -> int * int * string array * int array
    = "read_xpm_file"

let load file _opts =
  let w, h, cmap, imap = read file in
  let cmap, transparent = colormap_parse cmap in
  (* if the colors <= 256 then we create index8,
     otherwise create index16 *)
  if Array.length cmap <= 256 then begin
    let buf = Bytes.create (w * h) in
    for i = 0 to w * h - 1 do
      buf << i & char_of_int imap.(i)
    done;
    Index8 (Index8.create_with w h []
              { map = cmap; max = 256 - 1; } transparent buf)
  end else begin
    let buf = Bytes.create (w * h * 2) in
    for i = 0 to w * h - 1 do
      let (&) = (@@) in
      buf << i * 2     & char_of_int (imap.(i) / 256);
      buf << i * 2 + 1 & char_of_int (imap.(i) mod 256)
    done;
    Index16 (Index16.create_with w h []
               { map = cmap; max = 256 * 256 - 1; }
               transparent buf)
  end

let check_header filename =
  let len = 9 in
  let ic = open_in_bin filename in
  try
    let str = Bytes.create len in
    really_input ic str 0 len;
    close_in ic;
    if Bytes.to_string str = "/* XPM */" then
      { header_width = -1;
    	header_height = -1;
    	header_infos = []; }
    else raise Wrong_file_type
  with
  | _ -> raise Wrong_file_type

let () = add_methods Xpm {
  check_header = check_header;
  load = Some load;
  save = None;
  load_sequence = None;
  save_sequence = None;
}
