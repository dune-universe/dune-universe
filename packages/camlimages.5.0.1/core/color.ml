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

(* $Id: color.ml,v 1.1.2.1 2010/05/16 01:45:54 furuse Exp $*)

exception Too_many_colors

module type COLORMODEL = sig
  type t
  val square_distance : t -> t -> int
end

(***************************************************************** color map *)

type 'a map = {
    mutable max: int;
    (* maximum color index allowed in the color map (-1 = unlimited) *)
    mutable map: 'a array;
  }

let size cmap = Array.length cmap.map

let find_exact cmap c =
  let found = ref 0 in
  let map = cmap.map in
  try
    for i = 0 to Array.length map - 1 do
      let c' = map.(i) in
      if c = c' then begin found := i; raise Exit end
    done;
    raise Not_found
  with Exit -> !found

let add_color cmap c1 =
  try
    find_exact cmap c1
  with
  | Not_found ->
    let len = size cmap in
    if cmap.max >= 0 && len = cmap.max then raise Too_many_colors;
    cmap.map <- Array.append cmap.map [|c1|];
    len

let add_colors cmap cs =
  let ret, not_exist =
    List.fold_right
      (fun c (ret, not_exist) ->
         try
           let found = find_exact cmap c in
           (Some found :: ret), not_exist
         with
         | Not_found -> (None :: ret), (c :: not_exist))
      cs ([],[]) in
  let len = size cmap in
  if cmap.max >= 0 && len + List.length not_exist > cmap.max
  then raise Too_many_colors;
  cmap.map <- Array.append cmap.map (Array.of_list not_exist);
  let cntr = ref len in
  List.map
    (function
     | Some x -> x
     | None -> let x = !cntr in incr cntr; x)
    ret

let copy cmap = {
  max = cmap.max;
  map = Array.copy cmap.map;
}

module type S = sig
  type t

  val square_distance : t -> t -> int
  val plus : t -> t -> t
  val minus : t -> t -> t
  val size : t map -> int
  val find_exact : t map -> t -> int
  val add_color : t map -> t -> int
  val add_colors : t map -> t list -> int list
  val find_nearest : t map -> t -> int
end
    
module MakeMap(CM:COLORMODEL) = struct
  let size = (size : CM.t map -> int)
  let find_exact = (find_exact : CM.t map -> CM.t -> int)
  let add_color = (add_color : CM.t map -> CM.t -> int)
  let add_colors = (add_colors : CM.t map -> CM.t list -> int list)

  let find_nearest cmap c =
    let found = ref (-1) in
    let diff = ref (-1) in
    let map = cmap.map in
    for i = 0 to Array.length map - 1 do
      let c' = map.(i) in
      let d = CM.square_distance c c' in
      if !diff < 0 || d < !diff then begin
          diff := d;
          found := i
      end
    done;
    if !found = -1 then raise Not_found else !found

end

module RgbModel = struct
  type t = { mutable r : int; mutable g : int; mutable b : int; }

  let square_distance c1 c2 =
    let dr = c1.r - c2.r
    and dg = c1.g - c2.g
    and db = c1.b - c2.b in
    dr * dr + dg * dg + db * db

  let plus rgb rgb' =
    { r = rgb.r + rgb'.r;
      g = rgb.g + rgb'.g;
      b = rgb.b + rgb'.b; }

  let minus rgb rgb' =
    { r = rgb.r - rgb'.r;
      g = rgb.g - rgb'.g;
      b = rgb.b - rgb'.b; }

end

module Rgb = struct
  include RgbModel
  include MakeMap(RgbModel)
end

type rgb = Rgb.t = { mutable r : int; mutable g : int; mutable b : int; }

module RgbaModel = struct
  type t = { color : rgb; mutable alpha : int; }

  let square_distance c1 c2 =
    let ds_rgb = Rgb.square_distance c1.color c2.color in
    let da = c1.alpha - c2.alpha in
    ds_rgb + da * da

  let plus c c' =
    { color = Rgb.plus c.color c'.color;
      alpha = c.alpha + c'.alpha; }

  let minus c c' =
    { color = Rgb.minus c.color c'.color;
      alpha = c.alpha - c'.alpha; }

  let merge src dst = (* I am not sure... *)
    let check v = if v < 0 then 0 else if v > 255 then 255 else v in
    if src.alpha = 0 then dst else
    if src.alpha = 255 then src else
      let alpha' = 255 - src.alpha in
      let c = {
        r = check ((alpha' * dst.color.r * dst.alpha / 255 +
                    src.color.r * src.alpha) / 255);
        g = check ((alpha' * dst.color.g * dst.alpha / 255 +
                    src.color.g * src.alpha) / 255);
        b = check ((alpha' * dst.color.b * dst.alpha / 255 +
                    src.color.b * src.alpha) / 255);
      } in

      { color = c; alpha = check (255 - alpha' * (255 - dst.alpha) / 255); }
end

module Rgba = struct
  include RgbaModel
  include MakeMap(RgbaModel)
end

type rgba = Rgba.t = { color : rgb; mutable alpha : int; }

module CmykModel = struct
  type t = {
    mutable c : int; mutable m : int; mutable y : int; mutable k : int;
  }
       
  let square_distance c1 c2 =
    let dc = c1.c - c2.c
    and dm = c1.m - c2.m
    and dy = c1.y - c2.y
    and dk = c1.k - c2.k in
    dc * dc + dm * dm + dy * dy + dk * dk

  let plus c c' =
    { c = c.c + c'.c; m = c.m + c'.m; y = c.y + c'.y; k = c.k + c'.k; }

  let minus c c' =
    { c = c.c - c'.c; m = c.m - c'.m; y = c.y - c'.y; k = c.k - c'.k; }
end

module Cmyk = struct
  include CmykModel
  include MakeMap(CmykModel)
end

type cmyk = Cmyk.t =
    { mutable c : int; mutable m : int; mutable y : int;
      mutable k : int; }

(************************************************* RGB specialized functions *)

let rgb_square_distance = Rgb.square_distance
let plus = Rgb.plus
let minus = Rgb.minus

(*
let brightness c = (c.r * 88 + c.g * 127 + c.b * 40) / 255

XV setting
*)

let brightness c = (c.r * 54 + c.g * 182 + c.b * 19) / 255
(*
  Y = 0.212671 * R + 0.715160 * G + 0.072169 * B;

ITU-R Recommendation BT.709, Basic Parameter Values for the HDTV
Standard for the Studio and for International Programme Exchange (1990),
[formerly CCIR Rec. 709], ITU, 1211 Geneva 20, Switzerland.
*)

(********************************************************* Color name parser *)

let color_name_table = ref None

(* CR jfuruse: path_rgb_txt may not exist *)
let color_table_load () =
  let table = Hashtbl.create 107 in
  match Camlimages.path_rgb_txt with
  | None -> table
  | Some path_rgb_txt ->
      let ic = open_in path_rgb_txt in
      try
        while true do
          let s = input_line ic in
          if s.[0] <> '!' then
            let tokens =
              Mstring.split_str (function ' ' | '\t' -> true | _ -> false) s in
            match tokens with
            | r :: g :: b :: rest ->
              Hashtbl.add table (Mstring.catenate_sep " " rest)
                {r = int_of_string r; g = int_of_string g; b = int_of_string b;}
            | _ -> assert false
        done;
        raise Exit
      with
      | End_of_file ->
        close_in ic;
        color_name_table := Some table;
        table

let color_name_query c =
  let table =
    match !color_name_table with
    | Some t -> t
    | None -> color_table_load () in
  Hashtbl.find table c

let color_parse c =
  try
    if c = "none" || c = "None" then {r = -1; g = -1; b = -1} else
    if c.[0] = '#' then
      match String.length c with
      | 7 ->
        let r = int_of_string ("0x" ^ String.sub c 1 2)
        and g = int_of_string ("0x" ^ String.sub c 3 2)
        and b = int_of_string ("0x" ^ String.sub c 5 2) in
        {r = r; g = g; b = b}
      | 13 ->
        let r = int_of_string ("0x" ^ String.sub c 1 4) / 256
        and g = int_of_string ("0x" ^ String.sub c 5 4) / 256
        and b = int_of_string ("0x" ^ String.sub c 9 4) / 256 in
        {r = r; g = g; b = b}
      | _ -> raise Exit
    else color_name_query c
  with
  | _ -> failwith (Printf.sprintf "Color parse %s failed" c)

let colormap_parse cmap =
  let transparent = ref (-1) in
  let cmap = Array.map color_parse cmap in
  for i = 0 to Array.length cmap - 1 do
    let c = cmap.(i) in
    if c.r < 0 then begin
      c.r <- 0; c.g <- 255; c.b <- 0;
      transparent := i;
      prerr_endline (Printf.sprintf "transparent= %d" i);
    end
  done;
  cmap, !transparent
