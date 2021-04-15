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

type char_def = {
    code : int;
    dx : int;
    dy : int;
    width : int;
    height : int;
    hoffset : int;
    voffset : int;
    bitmap : string
  };;

type t = {
    name : string;
    dpi : int;
    table : char_def Table.t
  };;

(*** Converting PK fonts to abstract fonts ***)

let make_def_from_pk cdef =
  Pkfont.unpack cdef;
  let bitmap =
    match cdef.Pkfont.bitmap with
    | Pkfont.Unpacked s -> s
    | Pkfont.Packed _ -> assert false in
  { code = cdef.Pkfont.code;
    dx = cdef.Pkfont.dx;
    dy = cdef.Pkfont.dy;
    width = cdef.Pkfont.width;
    height = cdef.Pkfont.height;
    hoffset = cdef.Pkfont.hoffset;
    voffset = cdef.Pkfont.voffset;
    bitmap = bitmap };;

let make_font_from_pk font name dpi =
  let build code =
    make_def_from_pk (Pkfont.find_char_def font code) in
  { name = name;
    dpi = dpi;
    table = Table.make build }

(*** Finding a given font ***)

let find =
  let htable = Hashtbl.create 257 in
  fun fontname dpi ->
    try Hashtbl.find htable (fontname, dpi)
    with Not_found ->
      try
        let filename = Search.font_path fontname dpi in
        let pk_font = Pkfont.load filename in
        let font = make_font_from_pk pk_font fontname dpi in
        Hashtbl.add htable (fontname, dpi) font;
        font
      with _ -> raise Not_found;;

module Japanese = struct
  (* Temporal hack for Japanese DVI (of pTeX)
     This is really inefficient because we convert

     freetype -[rendering]-> abstract font -[grayscale]-> graymap -> screen

     even though we can create graymap directly from freetype
  *)

  type jfonttype = Mincho | Gothic

  let default_japanese_fontfiles = [
    "min", Mincho, "msmincho.ttc";
    "goth", Gothic, "msgothic.ttc"
  ]
 ;;

  let japanese_fontfiles =
    let sys_conffile = Filename.concat Config.advi_confdir "jpfonts.conf" in
    let user_conffile =
      Filename.concat Userfile.user_advi_dir "jpfonts.conf" in
    try
      let jffiles = ref [] in
      let ic,conffile =
        try open_in user_conffile, user_conffile
        with _ -> open_in sys_conffile, sys_conffile in
      try while true do
        let line = input_line ic in
        if not (String.length line = 0 || line.[0] = '#') then begin
          let tks =
            Misc.split_string line
              (function ' ' | '\t' -> true | _ -> false) 0
          in
          match tks with
          | [fname; ftype; ffile] ->
              begin try
                let ftype =
                  match ftype with
                  | "Mincho" -> Mincho
                  | "Gothic" -> Gothic
                  | _ ->
                      Misc.warning (conffile ^ ": illegal font type: " ^ line);
                      raise Exit
                in
                jffiles := (fname, ftype, ffile) :: !jffiles
              with
              | _ -> ()
              end
          | _ -> Misc.warning (conffile ^ ": parse failure: " ^ line)
        end;
      done; raise Exit with End_of_file ->
        close_in ic;
        List.rev !jffiles
    with
    | _ ->
        default_japanese_fontfiles
 ;;

  let make_font =
    let facetable = Hashtbl.create 17 in

    fun fontname dpi ->

      let face,typ,pt,jfm =
          try Hashtbl.find facetable fontname
          with Not_found ->
            let rec search = function
              | [] -> raise Not_found
              | (pref, typ, file) :: xs ->
                  try
                    let name = String.sub fontname 0 (String.length pref) in
                    let pt =
                      int_of_string (String.sub fontname (String.length name)
                        (String.length fontname - String.length name)) in
                    if name = pref then file, typ, pt
                    else raise Exit
                  with
                  | _ -> search xs
            in
            let fontfile, typ, pt = search japanese_fontfiles in
            let face =
              let path =
                try
                  Search.true_file_name [] fontfile
                with
                | e ->
                    Misc.warning
                      (Printf.sprintf "Font file %s for %s is not found"
                                      fontfile fontname);
                    raise e
              in
              try
                Ttfont.load_face path
              with
              | e ->
                  Misc.warning
                    (Printf.sprintf "Failed to load font file %s for %s"
                                     path fontname);
                  raise e
            in
            let jfmname =
              let jfm = fontname ^ ".tfm" in
              Search.true_file_name [] jfm
            in
            let jfm = Jfm.load_jfm_file jfmname in
            Hashtbl.add facetable fontname (face,typ,pt,jfm);
            face,typ,pt,jfm
      in

      let build jiscode =
        let unicode = Ttfont.jis2uni jiscode in

        (* metrics *)
        let width = Jfm.find_width jfm jiscode in
        let dx =
          Stdlib.truncate (float width *. float pt *. float dpi
                                   /. 1152.0) (* 72x16 *)
        in
        let x_fix, y_fix =
          let fix =
            try List.assoc jiscode
                  (match typ with
                   | Mincho -> Jfm.monospace_fix
                   | Gothic -> Jfm.monospace_fix)
             with _ -> 0.0
          in
          (* width min10 (mincho 10pt) is 9.62216pt
             (http://www.matsusaka-u.ac.jp/~okumura/jsclasses/jfm.html) *)
          Stdlib.truncate (float pt *. 0.962216 *.
                               float dpi /. 72.0 *. fix /. 1000.0),
          (* baseline fix is quite ad-hoc. I took 10% without reason *)
          Stdlib.truncate (float pt *. float dpi /. 72.0 *. 0.10)
          in

          (* drawing using ttfont.build *)
          let chardef = Ttfont.build face dpi pt unicode in

          { code= chardef.Ttfont.code;
            dx= chardef.Ttfont.dx + dx;
            dy= chardef.Ttfont.dy;
            width= chardef.Ttfont.width;
            height= chardef.Ttfont.height;
            hoffset= chardef.Ttfont.hoffset - x_fix;
            voffset= chardef.Ttfont.voffset - y_fix;
            bitmap= chardef.Ttfont.bitmap }
      in
      { name= fontname; dpi= dpi; table= Table.make build }
 ;;

  (* wrapper : any error returns dumb font *)
  let make_font fontname dpi =
    try make_font fontname dpi with _ -> raise Not_found
 ;;

end;;

let find =
  let htable = Hashtbl.create 257 in

  fun fontname dpi ->
    try Hashtbl.find htable (fontname, dpi) with Not_found ->
    try
      let font = Japanese.make_font fontname dpi in
      Hashtbl.add htable (fontname, dpi) font;
      font
    with Not_found ->
    try
      let filename = Search.font_path fontname dpi in
      let pk_font = Pkfont.load filename in
      let font = make_font_from_pk pk_font fontname dpi in
      Hashtbl.add htable (fontname, dpi) font;
      font
    with _ -> raise Not_found;;

(*** Searching for a char_def in a given font ***)

let find_char_def font code = Table.get font.table code;;
