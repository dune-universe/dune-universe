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

(* $Id: icon.ml,v 1.22 2004/09/24 10:55:07 weis Exp $ *)

open Images
open OImages
open Geometry
open OXimage

let xvmode = ref false

let mime_icons = Hashtbl.create 107

let string_of_format = function
  | Gif -> "GIF8x"
  | Bmp -> "BMP"
  | Tiff -> "TIFF"
  | Jpeg -> "JPEG"
  | Png -> "PNG"
  | Xpm -> "XPM"
  | Ppm -> "PPM"
  | _ -> "???"

let thumb_path name =
  Filename.concat
    (Filename.concat
       (Filename.dirname name)
       (if !xvmode then ".xvpics" else ".livpics"))
    (Filename.basename name)

let create_xvthumb name =
  let thumbpath = thumb_path name in
  let thumbdir = Filename.dirname thumbpath in
  (* prerr_endline ("thumbdir: " ^ thumbdir); *)
  if not (Sys.file_exists thumbdir) then Unix.mkdir thumbdir 0o755;

  let format, _hdrs = Images.file_format name in
  let w, h, img =
    match format with
    | Jpeg ->
      OJpeg.load_thumbnail name []
        { spec_width = Pixel 80; spec_height = Pixel 60;
          spec_aspect = Keep_at_most; spec_switch = Smaller_only;
          spec_x = 0; spec_y = 0 }
    | _ ->
      let img = OImages.load name [] in
      let w, h = img#width, img#height in
      w, h, img in

  let thumb = OXvthumb.create img in
  OXvthumb.save thumbpath
    (Printf.sprintf
       "%dx%d %s file  (%d bytes)" w h (string_of_format format)
       (Unix.lstat name).Unix.st_size)
    thumb;
  img#destroy;
  thumb#destroy

let create_livthumb name =
  let thumbpath = thumb_path name in
  let thumbdir = Filename.dirname thumbpath in
  (* prerr_endline ("thumbdir: " ^ thumbdir); *)
  if not (Sys.file_exists thumbdir) then Unix.mkdir thumbdir 0o755;

  let format, _hdrs = Images.file_format name in
  let w, h, img =
    match format with
    | Jpeg ->
      OJpeg.load_thumbnail name []
        { spec_width = Pixel 80; spec_height = Pixel 60;
          spec_aspect = Keep_at_most; spec_switch = Smaller_only;
          spec_x = 0; spec_y = 0 }
    | _ ->
      let img = OImages.load name [] in
      let w, h = img#width, img#height in
      w, h, img in

  let ratiow = 80.0 /. float w in
  let ratioh = 60.0 /. float h in
  let ratio = if ratiow  < ratioh then ratiow else ratioh in
  let img =
    let img =
      match OImages.tag img with
      | Index8 i -> i#to_rgb24
      | Index16 i -> i#to_rgb24
      | Rgb24 i -> i
      | _ -> assert false in
    img#resize None
      (Pervasives.truncate (float w *. ratio))
      (Pervasives.truncate (float h *. ratio)) in
  img#save thumbpath (Some Jpeg) [Save_Quality 75];
  img#destroy

let load_icon name typ =
  let thumb = thumb_path name in
  let inf, pixmap =
    (* XVTHUMB *)
    try
      let inf, img =
        if !xvmode then
          let inf, img = OXvthumb.load thumb in
          inf, img#coerce
        else "", OImages.load thumb [] in
      inf, pixmap_of_image Gui.root_win None img
    with
    | _ ->
      let mj, mn = typ in
      try
        if mj <> "image" then raise Exit;
        let inf, img =
          if !xvmode then begin
            create_xvthumb name;
            let inf, img = OXvthumb.load thumb in
            inf, img#coerce
          end else begin
            create_livthumb name;
            "", OImages.load thumb []
          end in
        inf, pixmap_of_image Gui.root_win None img
      with
      | _ ->
        let iconpath =
          try Hashtbl.find Iconcap.table (mj, mn) with
          | Not_found -> Hashtbl.find Iconcap.table (mj,"*") in
        let iconpath =
          Pathfind.find
            [ "."; "~/.liv"; "/usr/share/icons/"; "/usr/share/pixmaps"; ]
            iconpath in

        (* prerr_endline ("loading "^iconpath); *)
        (mj ^ "/" ^ mn),
        begin
          try Hashtbl.find mime_icons iconpath with
          | Not_found ->
              let pixmap =
                pixmap_of_image Gui.root_win None (OImages.load iconpath []) in
              Hashtbl.add mime_icons iconpath pixmap;
              pixmap
        end in
  inf, pixmap
