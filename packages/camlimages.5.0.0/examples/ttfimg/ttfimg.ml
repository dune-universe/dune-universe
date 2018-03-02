(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ttfimg.ml,v 1.11 2004/09/21 18:15:50 weis Exp $ *)

open Images
open OImages
open Freetype
open Fttext

let fonts = ref []
let message = ref None
let csize = ref 100.0
let outfile = ref None
let fg = ref {r = 0; g = 0; b = 0}
let bg = ref {r = 255; g = 255; b = 255}
let kanjimode = ref false
let dumpmode = ref false
let verbosemode = ref false

let () = Arg.parse [
  "-o", Arg.String (fun s -> outfile := Some s), ": outputfile" ;
  "-message", Arg.String (fun s -> message := Some s), ": message" ;
  "-charsize", Arg.Float (fun f -> csize := f), ": charsize (in 72dpi)" ;
  "-bg", Arg.String (fun s -> bg := Color.color_parse s), ": color";
  "-fg", Arg.String (fun s -> fg := Color.color_parse s), ": color";
  "-k", Arg.Unit (fun () -> kanjimode := true), "(japanese mode)" ;
  "-dump", Arg.Unit (fun () -> dumpmode := true), "(dump mode)";
  "-verbose", Arg.Unit (fun () -> verbosemode := true), "(verbose mode)"; ]
  (fun s -> fonts := s :: !fonts)
  "ttfimg font"

let fonts =
  match !fonts with
  | [] -> failwith "specify font file!"
  | l -> List.rev l


let () = match !outfile, fonts with
| None, _ -> ()
| Some _x, [ _ ] -> ()
| Some _x, _ -> failwith "just one font if you specify -o !"


let treat_font font =
  if !verbosemode then prerr_endline (Printf.sprintf "Processing %s" font);
  try
    let out =
      match !outfile with
      | None ->
          let out = font ^ ".jpg" in
          prerr_endline (Printf.sprintf "%s: will be written in %s" font out);
          out
      | Some s -> s in
    let format = guess_format out in

    if !verbosemode then prerr_endline "opening font...";
    let face = new OFreetype.face font 0 in

    face#set_char_size !csize !csize 72 72;

    if !verbosemode then
      List.iter
        (fun cmap ->
           prerr_endline
            (Printf.sprintf "charmap: { platform_id = %d; encoding_id = %d}"
               cmap.platform_id cmap.encoding_id) )
        face#charmaps;

    begin
      try face#set_charmap { platform_id = 3; encoding_id = 1 } with
      | _ ->
        try face#set_charmap { platform_id = 3; encoding_id = 0 } with
        | _ -> face#set_charmap (List.hd face#charmaps)
    end;

(*
    let mbbox = get_maximum_bbox inst#face in
*)
    let smetrics = face#size_metrics in

    if !dumpmode then begin
      let plus = 2 in
      let num_glyphs = face#num_glyphs in
      let rec digit x = if x < 10 then 1 else digit (x / 10) + 1 in
      let digit_num = digit num_glyphs in
      let numid n =
        let s = string_of_int n in
        String.make (digit_num - String.length s) '0' ^ s in

      for i = 0 to num_glyphs do
        if !verbosemode then prerr_endline (Printf.sprintf "glyph #%d..." i);
        let x1,y1,x2,y2 = face#size_of_glyphs [|char_index_of_int i|] in
        let w = truncate x2 - truncate x1 + plus * 2 in
        let h = truncate y2 - truncate y1 + plus * 2 in
        let rgb = new rgb24_filled w h {r = 255; g = 255; b = 255} in
        OFreetype.draw_glyphs face Fttext.func_darken_only
          (rgb :> rgb map) (plus-(truncate x1))
          (truncate y2 + plus) [|char_index_of_int i|];
        rgb#save (out ^ "-" ^ numid i) (Some format) [Save_Quality 95]
      done

    end else begin
      match !message with
      | None ->
        let plus = 2 in
        let w = smetrics.x_ppem + plus in
        let h = smetrics.y_ppem + plus in

        let encode s =
          match Array.map face#char_index s with
          | [|glyph|] -> glyph
          | _ -> assert false in

        let num_glyphs = face#num_glyphs in
        let chars =
          if not !kanjimode then
            [ 0, 0, encode (unicode_of_latin "A");
              1, 0, encode (unicode_of_latin "B");
              2, 0, encode (unicode_of_latin "a");
              3, 0, encode (unicode_of_latin "b");
              0, 1, encode (unicode_of_latin "\224");
              1, 1, encode (unicode_of_latin "\225");
              2, 1, encode (unicode_of_latin "\231");
              3, 1, encode (unicode_of_latin "\238");
              0, 2, encode (unicode_of_latin "1");
              1, 2, encode (unicode_of_latin "2");
              2, 2, encode (unicode_of_euc_japan "¤¢");
              3, 2, encode (unicode_of_euc_japan "´Á");
              0, 3, char_index_of_int (num_glyphs-4);
              1, 3, char_index_of_int (num_glyphs-3);
              2, 3, char_index_of_int (num_glyphs-2);
              3, 3, char_index_of_int (num_glyphs-1); ]
          else
            [ 0, 0, encode (unicode_of_latin "A");
              1, 0, encode (unicode_of_latin "B");
              2, 0, encode (unicode_of_latin "a");
              3, 0, encode (unicode_of_latin "b");
              0, 1, encode (unicode_of_latin "\224");
              1, 1, encode (unicode_of_latin "\225");
              2, 1, encode (unicode_of_latin "\231");
              3, 1, encode (unicode_of_latin "\238");
              0, 2, encode (unicode_of_latin "1");
              1, 2, encode (unicode_of_latin "2");
              2, 2, encode (unicode_of_euc_japan "¤¢");
              3, 2, encode (unicode_of_euc_japan "´Á");
              0, 3, encode (unicode_of_euc_japan "¾Æ");
              1, 3, encode (unicode_of_euc_japan "Æù");
              2, 3, encode (unicode_of_euc_japan "Äê");
              3, 3, encode (unicode_of_euc_japan "¿©"); ] in

        let rgb = new rgb24_filled (w*4) (h*4) {r = 255; g = 255; b = 255} in
        List.iter
          (fun (x, y, encoded) ->
             if !verbosemode then
               prerr_endline
                (Printf.sprintf
                   "drawing glyph #%d..." (int_of_char_index encoded));
             let x1, y1, _x2, _y2 = face#size_of_glyphs [|encoded|] in
(*prerr_endline ".";*)
             OFreetype.draw_glyphs face Fttext.func_darken_only (rgb :> rgb map)
               (w * x + 1 - truncate x1)
               (h * (y + 1) - 1 + truncate y1)
               [|encoded|])
          chars;
        rgb#save out (Some format) []
      | Some s ->
          if !verbosemode then
            prerr_endline (Printf.sprintf "drawing %s..." s);
          let plus = 8 in
          let encoded =
            if !kanjimode then unicode_of_euc_japan s else unicode_of_latin s in
          let x1, y1, x2, y2 = face#size encoded in
          let h = truncate (ceil y2) - truncate y1 + 1 + plus in
          prerr_endline (Printf.sprintf "height = %d" h);

          let rgb = new rgb24_filled (truncate (x2 -. x1) + plus) h !bg in
          OFreetype.draw_text face
            (fun org level ->
               let level' = 255 - level in
               { r = (org.r * level' + !fg.r * level) / 255;
                 g = (org.g * level' + !fg.g * level) / 255;
                 b = (org.b * level' + !fg.b * level) / 255 })
            (rgb :> rgb map)
            (plus / 2 - truncate x1)
            (truncate y2 + plus / 2)
            encoded;
          let outfile =
            match !outfile with
            | Some f -> f
            | None -> prerr_endline "Output to out.png"; "out.png" in
          rgb#save outfile None []
    end
  with
  | Failure e -> prerr_endline (font ^ ": Failure " ^ e)


let () = List.iter treat_font fonts
