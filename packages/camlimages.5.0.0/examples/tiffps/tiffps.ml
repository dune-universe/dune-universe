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

(* $Id: tiffps.ml,v 1.11 2008/05/17 19:55:50 furuse Exp $ *)

open Images
open Printf

(* String utilities *)

(* split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = String.length str in
  if len = 0 then [] else
    let rec skip_sep cur =
      if cur >= len then cur else
      if char_sep str.[cur] then skip_sep (succ cur)
      else cur  in
    let rec split beg cur =
      if cur >= len then
        if beg = cur then []
        else [String.sub str beg (len - beg)] else
      if char_sep str.[cur] then
        let nextw = skip_sep cur in
        String.sub str beg (cur - beg) :: split nextw nextw else
      split beg (succ cur) in
    let wstart = skip_sep 0 in
    split wstart wstart

let inch_cm = 2.53012048193
let inch_pt = 72.0

let units = [ "mm", 0.1 /. inch_cm *. inch_pt;
              "cm", 1.0 /. inch_cm *. inch_pt;
              "pt", 1.0;
              "in", inch_pt ]

let parse_length s = (* return in pt *)
  let v =
    let l = String.length s in
    let digit, unit =
      if l > 2 then String.sub s 0 2, String.sub s (l-2) 2 else "", "" in
    try
      List.assoc (String.lowercase unit) units *. float_of_string digit
    with
    | Not_found -> (* think it is in "pt" *) float_of_string s in
  prerr_endline (Printf.sprintf "%s -> %fpt" s v);
  v

(* Scanlined loader *)
type scanlined_loader = {
    read_next_line: (bytes -> unit);
    close: (unit -> unit)
  }

let scanline_open name =
  match Images.file_format name with
  | Jpeg, _ ->
      let w,h,ic,_rev_markers = Jpeg.open_in name in
      let y = ref 0 in 
      w, h, 200.0,
      { read_next_line = (fun buf -> Jpeg.read_scanline ic buf !y; incr y);
        close = (fun () -> Jpeg.close_in ic) }
  | Tiff, _ ->
      let w, h, dpi, _, ic = Tiff.open_in name in
      let y = ref 0 in
      w, h, dpi,
      { read_next_line = (fun buf -> Tiff.read_scanline ic buf !y; incr y);
        close = (fun () -> Tiff.close_in ic) }
  | _ -> assert false

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
   mutable name : string;
   mutable crop : crop option;
   mutable rot : rot;
   mutable size : size;
   mutable pos : at;
   mutable mirror : bool;
   mutable mono : bool;
}

let main () =
  (* global options *)
  let files = ref []
  and justdpi = ref false
  and comments = ref true
  and showpage = ref true in

  let default_conf = {
    name = "noname";
    crop = None;
    rot = Rot0;
    size = A4MaxSize;
    pos = A4Center;
    mirror = false;
    mono = false;
  } in

  let conf = {
    name = "noname";
    crop = None;
    rot = Rot0;
    size = A4MaxSize;
    pos = A4Center;
    mirror = false;
    mono = false;
  } in

  let set_default () =
    conf.name <- default_conf.name;
    conf.crop <- default_conf.crop;
    conf.rot <- default_conf.rot;
    conf.size <- default_conf.size;
    conf.pos <- default_conf.pos;
    conf.mirror <- default_conf.mirror;
    conf.mono <- default_conf.mono in

  Arg.parse [
    "-justdpi", Arg.Unit (fun () -> justdpi := true),
      ": print dpi information and quit";
    "-nocomments", Arg.Unit (fun () -> comments := false),
      ": no comment lines";
    "-noshowpage", Arg.Unit (fun () -> showpage := false),
      ": no showpage command";

    "-crop", Arg.String (fun s ->
      match
        List.map int_of_string
         (split_str (function '+' | 'x' -> true | _ -> false) s) with
      | [w; h; x; y] ->
          conf.crop <- Some {cx = x; cy = y; cw = w; ch = h}
      | _ -> assert false),
      "?x?+?+? : cropping";
    "-a4max", Arg.Unit (fun () ->
      conf.size <- A4MaxSize;
      conf.pos <- A4Center),
      ": expand image to a4 paper";
    "-maxbox", Arg.String (fun s ->
      match
        List.map parse_length
         (split_str (function 'x' -> true | _ -> false ) s) with
      | [w; h] -> conf.size <- MaxBox (w, h)
      | _ -> assert false),
      "?x? : upper bound printing size";
    "-minbox", Arg.String (fun s ->
      match
        List.map parse_length
         (split_str (function 'x' -> true | _ -> false ) s) with
      | [w; h] -> conf.size <- MinBox(w, h)
      | _ -> assert false),
      "?x? : lower bound printing size";
    "-dpi", Arg.Float (fun x -> conf.size <- DPI x),
      "dpi : dpi setting";
    "-at", Arg.String (fun s ->
      match
        List.map parse_length
          (split_str (function 'x' -> true | _ -> false ) s) with
      | [x; y] -> conf.pos <- BottomLeft (x, y)
      | _ -> assert false),
      "?x? : at bottom-left";
    "-topleft", Arg.String (fun s ->
      match
        List.map parse_length
          (split_str (function 'x' -> true | _ -> false ) s) with
      | [x; y] -> conf.pos <- TopLeft (x, y)
      | _ -> assert false),
      "?x? : at top-left";
    "-bottomleft", Arg.String (fun s ->
      match
        List.map parse_length
          (split_str (function 'x' -> true | _ -> false ) s) with
      | [x; y] -> conf.pos <- BottomLeft (x, y)
      | _ -> assert false),
      "?x? : at bottom-left";
    "-topright", Arg.String (fun s ->
      match
        List.map parse_length
          (split_str (function 'x' -> true | _ -> false ) s) with
      | [x; y] -> conf.pos <- TopRight (x, y)
      | _ -> assert false),
      "?x? : at top-right";
    "-bottomright", Arg.String (fun s ->
      match
        List.map parse_length
          (split_str (function 'x' -> true | _ -> false ) s) with
      | [x; y] -> conf.pos <- BottomRight (x, y)
      | _ -> assert false),
      "?x? : at bottom-right";
    "-center", Arg.String (fun s ->
      match
         List.map parse_length
          (split_str (function 'x' -> true | _ -> false ) s) with
      | [x; y] -> conf.pos <- Center (x, y)
      | _ -> assert false),
      "?x? : at center";
    "-rot", Arg.Int (fun x ->
      let rot =
        match x with
        | 0 -> Rot0
        | 90 -> Rot90
        | 180 -> Rot180
        | 270 -> Rot270
        | _ -> invalid_arg "rotation must be 90*x" in
      conf.rot <- rot),
      "r : rotation (x90 only)";
    "-autorot", Arg.Unit (fun () -> conf.rot <- RotMax),
      ": auto rotate (to have larger image)";
    "-mono", Arg.Unit (fun () -> conf.mono <- true),
      ": monochrome";
    "-mirror", Arg.Unit (fun () -> conf.mirror <- true),
      ": mirror image"
    ]
    (fun x ->
      files := {conf with name = x} :: !files;
      set_default ()) "tiffps";

  let files = List.rev !files in

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
  let p = print_endline in

  let output_image_func conf =
    (* maximum printing area *)
    let limitw, limith =
      match conf.size with
      | MaxBox (w, h) -> w, h
      | MinBox (w, h) -> w, h
      | _ ->
        paper_width -. border *. 2.0,
        paper_height -. border *. 2.0 in

    (* open the file just to get the image size info. *)
    let imgw, imgh, _orgdpi, th = scanline_open conf.name in
    th.close ();

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
        let rw = limitw /. (float w) and rh = limith /. (float h) in
        match conf.size with
        | MinBox _ -> if rw < rh then rh else rw
        | _ -> if rw > rh then rh else rw in
      let ratio90 =
        let rw = limith /. (float w) and rh = limitw /. (float h) in
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
        prerr_endline (sprintf "%s : %f dpi" conf.name dpi);
        paper_width /. 8.26 /. dpi
      | _ ->
        let ratio =
          if limitw /. float w  *. float h < limith
          then limitw /. float w
          else limith /. float h in
        let dpi = paper_width /. 8.26 /. ratio in
        prerr_endline (sprintf "%s : %f dpi" conf.name dpi);
        ratio in

    (* if just for printing dpi, quit. *)
    if !justdpi then raise Exit;

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
        | Center (x, y) -> x -. prw /. 2.0 , y -. prh /. 2.0
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
      (* reopen file *)
      let _, _, _, th = scanline_open conf.name in

      if !comments then p "% Translate for offset";
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
      if !comments then p "% Variable to keep one line of raster data";
      p (sprintf "/scanline %d 3 mul string def" w);
      if !comments then p "% Image geometry";
      p (sprintf "%d %d 8" w h);
      if !comments then p "% Transformation matrix";
      p (sprintf "[ %d 0 0 %d 0 %d ]" w h h);
      p "{ currentfile scanline readhexstring pop } false 3";
      p "colorimage";

      let buf = Bytes.create (imgw * 3) in
      for _y = 0 to y1 - 1 do th.read_next_line buf done;
      let prevperdec = ref (-1) in
      for y = y1 to y1 + h - 1 do
        let perdec = (y - y1) * 10 / h in
        if !prevperdec <> perdec then begin
          prerr_endline (sprintf "%d0 %% done" perdec);
          prevperdec := perdec
        end;
        th.read_next_line buf;
        let print_pixel =
          if not conf.mono then
            function x ->
              let adrs = x * 3 in
              for i = 0 to 2 do
                print_string (sprintf "%02x" (Char.code (Bytes.unsafe_get buf (adrs+i))))
              done
          else
            let mono r g b =
              (r * 256 / 3 + g * 256 / 2 + b * 256 / 6) / 256 in
            function x ->
              let adrs = x * 3 in
              let m =
                mono (Char.code (Bytes.unsafe_get buf (adrs)))
                     (Char.code (Bytes.unsafe_get buf (adrs + 1)))
                     (Char.code (Bytes.unsafe_get buf (adrs + 2))) in
              for _i = 0 to 2 do print_string (sprintf "%02x" m) done in
        if not conf.mirror
        then for x = x1 to x1 + w - 1 do print_pixel x done
        else for x = x1 + w - 1 downto x1 do print_pixel x done;
        print_newline ()
      done;
      prerr_endline "100 % done";
      th.close ();
      p "grestore") in

  let funcs = List.map output_image_func files in

  (* Header *)
  if !comments then begin
    p "%!PS-Adobe-3.0";
    p "%%Creator: Tiffps by Jun FURUSE";
    p ("%%Title: "^
       (Mstring.catenate_sep "+" (List.map (fun f -> f.name) files)));
    p "%%DocumentData: Clean7Bit";
    p "%%Pages: 1";
    p (sprintf "%%%%BoundingBox: %f %f %f %f" !bbx1 !bby1 !bbx2 !bby2);
    p "%%EndComments";
    p "%%BeginProlog";
    p "% Use own dictionary to avoid conflicts";
    p "5 dict begin";
    p "%%EndProlog";
    if !showpage then p "%%Page: 1 1";
  end;

  List.iter (fun f -> f ()) funcs;

  if !showpage then p "showpage";
  if !comments then p "%%Trailer";
  p "end";
  if !comments then p "%%EOF"

let () = main ()
