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

(* $Id: crop.ml,v 1.14 2004/09/12 21:39:52 weis Exp $ *)

open Images
open OImages

(* split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = String.length str in
  if len = 0 then [] else
    let rec skip_sep cur =
      if cur >= len then cur
      else if char_sep str.[cur] then skip_sep (succ cur)
      else cur  in
    let rec split beg cur =
      if cur >= len then
        if beg = cur then []
        else [String.sub str beg (len - beg)]
      else if char_sep str.[cur]
           then
             let nextw = skip_sep cur in
              (String.sub str beg (cur - beg))
                ::(split nextw nextw)
           else split beg (succ cur) in
    let wstart = skip_sep 0 in
    split wstart wstart

let get_extension s =
  let dotpos = String.rindex s '.' in
  String.sub s (dotpos + 1) (String.length s - dotpos - 1)

let edge edgename img24 =
  (* its not a true edge detection it is "directed" *)
  let edgeimg =
    match edgename with
    | Some _ ->
        Some (new rgb24_with img24#width img24#height
                [] (Bytes.copy img24#dump))
    | None -> None in
  let edge = Array.init img24#width (fun _ -> Array.make img24#height 0) in

  (* inner kills outer *)
  let s = if img24#width < img24#height then img24#width else img24#height in
  let s = s / 2 in

  let diff x1 y1 x2 y2 =
    Color.rgb_square_distance (img24#get x1 y1) (img24#get x2 y2) > 5000 in

  (* directed edge *)
  for i = 0 to s - 1 do
    for x = i to img24#width - 1 - i do
      let y = i in
      let y' = y + 1 in
      if diff x y x y' then edge.(x).(y') <- 1;

      let y = img24#height - 1 - i in
      let y' = y - 1 in
      if diff x y x y' then edge.(x).(y') <- 1;
    done;

    for y = i to img24#height - 1 - i do
      let x = i in
      let x' = x + 1 in
      if diff x y x' y then edge.(x').(y) <- 1;

      let x = img24#width - 1 - i in
      let x' = x - 1 in
      if diff x y x' y then edge.(x').(y) <- 1;
    done;
  done;

  (* inner kills outer *)
  for i = 0 to s - 1 do
    for x = i to img24#width - 1 - i do
      let y = i in
      let y' = y + 1 in
      if edge.(x).(y') = 1 then edge.(x).(y) <- 0;

      let y = img24#height - 1 - i in
      let y' = y - 1 in
      if edge.(x).(y') = 1 then edge.(x).(y) <- 0;
    done;

    for y = i to img24#height - 1 - i do
      let x = i in
      let x' = x + 1 in
      if edge.(x').(y) = 1 then edge.(x).(y) <- 0;

      let x = img24#width - 1 - i in
      let x' = x - 1 in
      if edge.(x').(y) = 1 then edge.(x).(y) <- 0;
    done;
  done;

  let edge2 = Array.init img24#width (fun _ -> Array.make img24#height 0) in
  (* sole points are dead *)
  for x = 0 to img24#width -1 do
    for y =0 to img24#height -1 do
      let sole = ref true in
      if edge.(x).(y) > 0 then begin
        for sx = -1 to 1 do
          for sy = -1 to 1 do
            if sx * sy <> 0 || (sx = 0 && sy = 0) then () else begin
              try if edge.(x+sx).(y+sy) > 0 then sole := false
              with _ -> ()
            end
          done
        done;
      end;
      if not !sole then edge2.(x).(y) <- 1
    done
  done;

  begin match edgename, edgeimg with
  | Some _name, Some img ->
      for x = 0 to img24#width - 1 do
        for y = 0 to img24#height - 1 do
          if edge2.(x).(y) > 0 then begin
            let rgb = img#get x y in
            rgb.r <- (rgb.r + 255 * 9) / 10;
            rgb.g <- rgb.g / 10;
            rgb.b <- rgb.b / 10;
            img#set x y rgb
          end
        done
      done;
  | _ -> () end;
  edge2, edgeimg

let save def name img format =
  if format = Jpeg then begin
    let rec saver quality =
      let defsize = (Unix.stat def).Unix.st_size in
      img#save name (Some Jpeg) [Save_Quality quality];
      let newsize = (Unix.stat name).Unix.st_size in
      if newsize > defsize * 3 / 2 &&
         quality > 50 then saver (quality - 5)
      else prerr_endline (Printf.sprintf "quality= %d" quality) in
    saver 95
  end else begin
    img#save name (Some format) [Save_Quality 95];
    prerr_endline "done."
  end

type crop =
  | WHXY of int * int * int * int
  | XXYY of int * int * int * int
  | PROP of int * int

let () =
  let files = ref [] in
  let force_write = ref false in
  let test = ref false in
  let conf = ref None in
  let delim = ref 4 in
  Arg.parse [ "-force", Arg.Unit (fun () -> force_write := true),
              ": force to create a cropped file everytime";
              "-test", Arg.Unit (fun () -> test := true),
              ": test";
              "-delim", Arg.Int (fun x -> delim := x),
              ": delimit 1/x (1/4)";
              "-crop", Arg.String (fun s ->
                let whxy = ref false in
                let prop = ref false in
                let xxyy = ref false in
                match List.map int_of_string
                  (split_str (function '+' -> whxy := true; true
                                     | 'x' -> whxy := true; true
                                     | '-' -> xxyy := true; true
                                     | '/' -> prop := true; true
                                     | _ -> false) s)
                with
                | [w;h] when !prop ->
                    conf := Some (PROP (w,h))
                | [w;h;x;y] when !whxy ->
                    conf := Some (WHXY (w,h,x,y))
                | [x1;x2;y1;y2] when !xxyy ->
                    conf := Some (XXYY (x1,x2,y1,y2))
                | _ -> assert false), "?x?+?+? : explicit cropping";
    ] (fun s -> files := s :: !files) "crop files";
  
  let files = List.rev !files in
  List.iter (fun file ->
    try
      let format,_ = Images.file_format file in
      let orgimg = OImages.load file [] in
      let ext = get_extension file in
      let body =
        String.sub file 0 (String.length file - String.length ext - 1) in
      let outfile = body ^ ".crop." ^ ext in
      let edgefile = body ^ ".edge." ^ ext in
  
      let edgeimg = ref None in
      let x1, x2, y1, y2 =
        match !conf with
        | None ->
            let img =
              match OImages.tag orgimg with
              | Rgb24 i -> i
              | Index8 i -> i#to_rgb24
              | _ -> raise Wrong_image_class
            in
            let edge, eimg =
              edge (if !test then Some edgefile else None) img in
  
            edgeimg := eimg;
  
            let borderdetect w h q =
              let max = w / !delim in
              let found = ref 0 in
  
              for x = 0 to max do
  
                let rec chunk y =
                  if q x y = 1 then
                    try
                      chunk (y+1) + 1
                    with _ -> 1
                  else 0 in
  
                let rec chunks y =
                  try
                    let c = chunk y in
                    c :: chunks (y + c + 1)
                  with
                  | _ -> [] in
  
                let chunks = chunks 0 in
  
                let power =
                  List.fold_left (fun s x -> s + x) 0
                    (List.map (fun x -> x * x) chunks) in
                if power > h * h / 128 then found := x;
              done;
              !found in
            let x1 =
              borderdetect img#width img#height
                (fun x y -> edge.(x).(y)) in
            let x2 =
              borderdetect img#width img#height
                (fun x y -> edge.(img#width-x-1).(y)) in
            let y1 =
              borderdetect img#height img#width
                (fun x y -> edge.(y).(x)) in
            let y2 =
              borderdetect img#height img#width
                (fun x y -> edge.(y).(img#height-x-1)) in
            begin match !edgeimg with
            | Some img ->
                for x = x1 to img#width - 1 - x2 do
                  let f y =
                    if edge.(x).(y) > 0 then img#set x y {r=0;g=255;b=0} in
                  f y1;
                  f (img#height - 1 - y2)
                done;
                for y = y1 to img#height - 1 - y2 do
                  let f x =
                    if edge.(x).(y) > 0 then img#set x y {r=0;g=255;b=0} in
                  f x1;
                  f (img#width - 1 - x2)
                done;
  
                img#save edgefile (Some Jpeg) [Save_Quality 75];
                img#destroy
            | None -> ()
            end;
  
            x1, x2, y1, y2
        | Some (WHXY (w, h, x, y)) ->
            prerr_endline "WHXY";
            x, orgimg#width - x - w, y, orgimg#height - y - h
        | Some (XXYY (x1, x2, y1, y2)) -> x1, x2, y1, y2
        | Some (PROP (w, h)) ->
            let r = float w /. float h in
            let dx = truncate (((float orgimg#width) -.
                                  (float orgimg#height) *. r) /. 2.0)  in
            let dy = truncate (((float orgimg#height) -.
                                  (float orgimg#width) /. r) /. 2.0)  in
            if dx > 0 then dx, dx, 0, 0 else
            if dy > 0 then 0, 0, dy, dy else
            0, 0, 0, 0 in
  
      let w = orgimg#width - x1 - x2
      and h = orgimg#height - y1 - y2 in
  
      prerr_string
        (Printf.sprintf "%s: %dx%d+%d+%d (x:%d %d, y:%d %d)"
                        file w h x1 y1 x1 x2 y1 y2);
      flush stderr;
  
      if w <> orgimg#width || h <> orgimg#height then begin
        let img' = OImages.sub orgimg x1 y1 w h in
        prerr_string " saving... ";
        flush stderr;
        save file outfile img' format
      end else begin
        (* no need to create the file *)
        if !force_write then begin
          prerr_string " saving... ";
          flush stderr;
          save file outfile orgimg format
        end else prerr_endline " no need to save";
      end
    with
    | Wrong_image_class ->
        prerr_endline (Printf.sprintf "%s not supported" file)) files
  
