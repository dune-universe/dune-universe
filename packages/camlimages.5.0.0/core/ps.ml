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

(* $Id: ps.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open Images
open Rgb24
open Util

type bounding_box = (int * int * int * int) option

let debug = try ignore (Sys.getenv "DEBUG_PS"); true with _ -> false

let debug_endline = if debug then prerr_endline else fun _ -> ()

let check_header filename =
 let ic = open_in_bin filename in
 try
   let s = input_line ic in
   if String.sub s 0 4 <> "%!PS" && String.sub s 0 4 <> "%PDF"
   then raise Exit;
   close_in ic;
   { header_width = -1;
     header_height = -1;
     header_infos = []; }
 with
 | _ ->
   close_in ic;
   raise Wrong_file_type

let get_bounding_box file =
  let ic = open_in_bin file in
  let bbox = ref None in
  let bbox_head = "%%BoundingBox:" in
  try
   while true do
    let line = input_line ic in
    if String.sub line 0 1 <> "%" then raise Exit;
    try
      if String.sub line 0 (String.length bbox_head) = bbox_head then begin
        let rem =
          String.sub line (String.length bbox_head)
            (String.length line - String.length bbox_head) in
        let x1, y1, x2, y2 =
          match
            List.map int_of_string
              (Mstring.split_str (function ' ' -> true | _ -> false) rem) with
          | [x1; y1; x2; y2] -> x1, y1, x2, y2
          | _ -> assert false in
        bbox := Some (x1, y1, x2, y2);
        raise Exit
      end
    with
    | _ -> ()
   done;
   None
  with
  | _ ->
    close_in ic;
    !bbox

let load_ps file bbox_opt options =
  if not Camlimages.lib_ps then failwith "ps is not supported" else
  let path_gs = match Camlimages.path_gs with Some x -> x | _ -> assert false in                                                                   
  let resx, resy =
    match load_resolution options with
    | Some (rx, ry) -> rx, ry
    | None -> 72.0, 72.0 in
  let bbox =
    match bbox_opt with
    | Some bbox -> Some bbox
    | None -> get_bounding_box file in
  let tmpfile = Tmpfile.new_tmp_file_name "temp" in
  let command =
    match bbox with
    | None ->
      Printf.sprintf
        "%s -sDEVICE=ppmraw -r%fx%f -q -dSAFER -dNOPAUSE \
         -sOutputFile=%s %s -c showpage -c quit"
        path_gs resx resy tmpfile file
    | Some (x1, y1, x2, y2) ->
      let ratiox = resx /. 72.0 in
      let ratioy = resy /. 72.0 in
      let width = truncate (float (x2 - x1 + 1) *. ratiox) in
      let height = truncate (float (y2 - y1 + 1) *. ratioy) in
      Printf.sprintf
        "%s -sDEVICE=ppmraw -r%fx%f -g%dx%d -q -dSAFER -dNOPAUSE \
         -sOutputFile=%s -c %d %d translate -f %s -c showpage -c quit"
        path_gs resx resy width height tmpfile (-x1) (-y1) file in
  debug_endline command;
  if Sys.command command <> 0 then begin
    Tmpfile.remove_tmp_file tmpfile;
    failwith "gs interpretation failed" end else
  let img = Ppm.load tmpfile [] in
  Tmpfile.remove_tmp_file tmpfile;
  img

let load file options = load_ps file None options

open Printf

type rot = Rot0 | Rot90 | Rot180 | Rot270 | RotMax

type at =
   | TopLeft of float * float
   | TopRight of float * float
   | BottomLeft of float * float
   | BottomRight of float * float
   | Center of float * float
   | A4Center

type crop = {
    mutable cx : int;
    mutable cy : int;
    mutable cw : int;
    mutable ch : int;
  }

type size =
   | A4MaxSize
   | DPI of float
   | MaxBox of float * float
   | MinBox of float * float

type conf = {
    mutable crop : crop option;
    mutable rot : rot;
    mutable size : size;
    mutable pos : at;
    mutable mirror : bool;
    mutable mono : bool;
  }

let super_save file conf comments showpage images =
  (* paper properties *)
  let paper_width = 595.0
  and paper_height = 842.0
  and border = 15.0 in

  let bbx1 = ref 0.0
  and bby1 = ref 0.0
  and bbx2 = ref 0.0
  and bby2 = ref 0.0 in

  let first_image = ref true in
  let set_bbox x1 y1 x2 y2 =
    if !first_image then begin
      bbx1 := x1; bby1 := y1; bbx2 := x2; bby2 := y2; first_image := false
    end else begin
      if !bbx1 > x1 then bbx1 := x1;
      if !bby1 > y1 then bby1 := y1;
      if !bbx2 < x2 then bbx2 := x2;
      if !bby2 < y2 then bby2 := y2;
    end in

  (* printer *)
  let oc = open_out_bin file in
  let p s = output_string oc s; output_char oc '\n' in
  let p_ = output_string oc in

  let output_image_func image =
    (* maximum printing area *)
    let limitw, limith =
      match conf.size with
      | MaxBox (w, h) -> w, h
      | MinBox (w, h) -> w, h
      | _ ->
        paper_width -. border *. 2.0,
        paper_height -. border *. 2.0 in

    (* open file just for getting image size info. *)
    let imgw, imgh = image.width, image.height in

    (* cropping area *)
    let w, h, x1, y1 =
      match conf.crop with
      | Some crop ->
        (* check cropping area *)
        if crop.cx + crop.cw > imgw then crop.cw <- imgw - crop.cx;
        if crop.cy + crop.ch > imgh then crop.ch <- imgh - crop.cy;
        crop.cw, crop.ch, crop.cx, crop.cy
      | None -> imgw, imgh, 0, 0 in

    (* auto rotation *)
    if conf.rot = RotMax then begin
      let ratio0 =
        let rw = limitw /. float w and rh = limith /. float h in
        match conf.size with
        | MinBox _ -> if rw < rh then rh else rw
        | _ -> if rw > rh then rh else rw in
      let ratio90 =
        let rw = limith /. float w and rh = limitw /. float h in
        match conf.size with
        | MinBox _ -> if rw < rh then rh else rw
        | _ -> if rw > rh then rh else rw in
      match conf.size with
      | MinBox _ -> (* smaller is better *)
        conf.rot <- if ratio0 < ratio90 then Rot0 else Rot90
      | _ -> (* larger is better *)
        conf.rot <- if ratio0 > ratio90 then Rot0 else Rot90
    end;

    (* from the point of view of the image *)
    let limitw, limith =
      match conf.rot with
      | Rot0 | Rot180 -> limitw, limith
      | Rot90 | Rot270 -> limith, limitw
      | _ -> assert false in

    (* dpi *)
    let ratio =
      match conf.size with
      | DPI dpi ->
        debug_endline (sprintf "%f dpi" dpi);
        paper_width /. 8.26 /. dpi
      | _ ->
        let ratio =
          if limitw /. float w  *. float h < limith
          then limitw /. float w
          else limith /. float h in
(*
          let dpi = paper_width /. 8.26 /. ratio in
          debug_endline (sprintf "%f dpi" dpi);
*)
          ratio in

    let rw = float w  *. ratio
    and rh = float h *. ratio in

    (* now on the paper ... *)
    let prw, prh =
      match conf.rot with
      | Rot0 | Rot180 -> rw, rh
      | Rot90 | Rot270 -> rh, rw
      | _ -> assert false in

    let paper_x1, paper_y1 =
      let x, y =
        match conf.pos with
        | BottomLeft (x, y) -> x, y
        | TopLeft (x, y) -> x, y -. prh
        | BottomRight (x, y) -> x +. prw, y
        | TopRight (x, y) -> x +. prw, y -. prh
        | Center (x, y) -> x -. prw /. 2.0, y -. prh /. 2.0
        | A4Center ->
          (paper_width -. prw) /. 2.0,
          (paper_height -. prh) /. 2.0 in
      match conf.rot with
      | Rot0 -> x, y
      | Rot180 -> prw +. x, prh +. y
      | Rot90 -> x, prh +. y
      | Rot270 -> prw +. x, y
      | _ -> assert false in

    set_bbox paper_x1 paper_y1 (paper_x1 +. prw) (paper_y1 +. prh);

    (fun () ->
      if comments then p "% Translate for offset";
      p "gsave";
      p (sprintf "%f %f translate" paper_x1 paper_y1);
      p (sprintf "%d rotate"
           begin match conf.rot with
           | Rot0 -> 0
           | Rot90 -> -90
           | Rot180 -> -180
           | Rot270 -> -270
           | _ -> assert false
           end);
      p (sprintf "%f -%f scale" rw rh);
      if comments then p "% Variable to keep one line of raster data";
      p (sprintf "/scanline %d 3 mul string def" w);
      if comments then p "% Image geometry";
      p (sprintf "%d %d 8" w h);
      if comments then p "% Transformation matrix";
      p (sprintf "[ %d 0 0 %d 0 %d ]" w h h);
      p "{ currentfile scanline readhexstring pop } false 3";
      p "colorimage";

      let prevperdec = ref (-1) in
      for y = y1 to y1 + h - 1 do
        let perdec = (y - y1) * 10 / h in
        if !prevperdec <> perdec then begin
(*
          debug_endline (sprintf "%d0 %% done" perdec);
*)
          prevperdec := perdec
        end;
        let buf = Rgb24.get_scanline image y in
        begin
          let print_pixel =
            if not conf.mono then
              function x ->
                let adrs = x * 3 in
                for i = 0 to 2 do
                  p_ (sprintf "%02x" (buf @% adrs+i))
                done
            else
              let mono r g b =
                (r * 256 / 3 + g * 256 / 2 + b * 256 / 6) / 256 in
              function x ->
                let adrs = x * 3 in
                let m =
                  mono
                   (buf @% adrs    )
                   (buf @% adrs + 1)
                   (buf @% adrs + 2) in
                for _i = 0 to 2 do
                  p_ (sprintf "%02x" m)
                done in
          if not conf.mirror then
            for x = x1 to x1 + w - 1 do print_pixel x done
          else
            for x = x1 + w - 1 downto x1 do print_pixel x done
        end;
        p ""
      done;
      debug_endline "100 % done";
      p "grestore") in

  let funcs = List.map output_image_func images in

  (* Header *)
  if comments then begin
    p "%!PS-Adobe-3.0";
    p "%%Creator: Camlimages PS writer by Jun FURUSE";
    p ("%%Title: " ^ file);
    p "%%DocumentData: Clean7Bit";
    p "%%Pages: 1";
    p (sprintf "%%%%BoundingBox: %f %f %f %f" !bbx1 !bby1 !bbx2 !bby2);
    p "%%EndComments";
    p "%%BeginProlog";
    p "% Use own dictionary to avoid conflicts";
    p "5 dict begin";
    p "%%EndProlog";
    if showpage then p "%%Page: 1 1";
  end;

  List.iter (fun f -> f ()) funcs;

  if showpage then p "showpage";
  if comments then p "%%Trailer";
  p "end";
  if comments then p "%%EOF";
  close_out oc

let default_conf = {
  crop = None;
  rot = Rot0;
  size = A4MaxSize;
  pos = A4Center;
  mirror = false;
  mono = false;
}

let save file _options im =
  match im with
  | Rgb24 img -> super_save file default_conf true false [img]
  | _ -> raise Wrong_image_type

let () = add_methods Ps
 { check_header = check_header;
   load = Some load;
   save = Some save;
   load_sequence = None;
   save_sequence = None;
}
