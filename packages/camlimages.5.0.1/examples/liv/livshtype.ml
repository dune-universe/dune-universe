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

(* $Id: livshtype.ml,v 1.5 2004/09/24 10:55:08 weis Exp $ *)

open Mstring
open Unix
open Images
(* open Info *)

type typ =
   | ContentType of string
   | ContentEncoding of string
   | Special of string

let default_mime_types = "/etc/mime.types"

let suffixes = Hashtbl.create 107

let add_suffix (s, t) = Hashtbl.add suffixes s t

(* Even if we don't have a suffix file... *)
(* If the suffix file says otherwise, it will have priority *)
let default_mime_suffixes = [
  "html", ContentType "text/html";
  "htm",  ContentType "text/html";
  "txt",  ContentType "text/plain";
  "ps",   ContentType "application/postscript";
  "dvi",  ContentType "application/x-dvi";
  "advi", ContentType "application/advi";
  "gif",  ContentType "image/gif";
  "jpeg", ContentType "image/jpeg";
  "jpg",  ContentType "image/jpeg";
  "bmp",  ContentType "image/bmp";
  "png",  ContentType "image/png";
  "tiff", ContentType "image/tiff";
  "tif",  ContentType "image/tiff";
  "au",   ContentType "audio/basic";
  "snd",  ContentType "audio/basic";
  "wav",  ContentType "audio/x-wav";
  "mid",  ContentType "audio/midi";
  "mpeg", ContentType "video/mpeg";
  "mpg",  ContentType "video/mpeg";
  "avi",  ContentType "video/avi";
  "wmv",  ContentType "video/wmv";
  "fli",  ContentType "video/fli";
  "flc",  ContentType "video/fli";
  "gz",   ContentEncoding "gzip";
  "Z",    ContentEncoding "compress";
  "asc",  ContentEncoding "pgp";
  "pgp",  ContentEncoding "pgp";
  "cmo",  ContentType "application/x-caml-applet";
]

let () = List.iter add_suffix default_mime_suffixes

(* mime_types *)
let read_suffix_file f =
 try
  let ic = open_in f in
  try
    while true do
      let l = input_line ic in
      if l <> "" && l.[0] <> '#' then
        let tokens = split_str (function ' ' | '\t' -> true | _ -> false) l in
        match tokens with
        | [] -> ()
        | x :: l ->
          try
            ignore (String.index x '/');
            List.iter (function sufx -> add_suffix (sufx, ContentType x)) l
          with
          | Not_found ->
            List.iter (function sufx -> add_suffix (sufx, ContentEncoding x)) l
    done
  with End_of_file -> close_in ic
 with Sys_error _ -> ()

let guess link_as_link f =
  let from_header f =
    match Images.guess_format f with
    | Gif -> ContentType "image/gif"
    | Tiff -> ContentType "image/tiff"
    | Jpeg -> ContentType "image/jpeg"
    | Png -> ContentType "image/png"
    | Xpm -> ContentType "image/x-xpixmap"
    | Bmp -> ContentType "image/bmp"
    | Ppm -> ContentType "image/x-portable-pixmap"
    | Ps -> ContentType "application/postscript" in

  let st = if link_as_link then Unix.lstat f else Unix.stat f in
  match st.st_kind with
  | S_DIR -> Special "dir"
  | S_CHR -> Special "chr"
  | S_BLK -> Special "blk"
  | S_LNK ->
    begin
      try
        let st = Unix.stat f in
        match st.st_kind with
        | S_DIR -> Special "lnkdir"
        | _ -> begin try from_header f with _ -> Special "lnk" end
      with
      | _ -> Special "lnk"
    end
  | S_FIFO -> Special "fifo"
  | S_SOCK -> Special "sock"
  | _ ->
    begin
      try from_header f
      with
      | _ ->
        Hashtbl.find suffixes (String.lowercase (snd (Livmisc.get_extension f)))
    end

let guess = guess false

(* prerr_endline "reading suffix"; *)
let () = read_suffix_file default_mime_types
