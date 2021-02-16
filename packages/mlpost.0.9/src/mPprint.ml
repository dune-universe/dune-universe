(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Misc
open Concrete_types
open Types
module C = Compiled_types
module M = Matrix

let externalimage_dimension filename : float * float =
  let inch =
    Unix.open_process_in ("identify -format \"%h\\n%w\" " ^ filename)
  in
  let lh, lw =
    try
      let lh = input_line inch in
      let lw = input_line inch in
      (lh, lw)
    with End_of_file -> invalid_arg "Unknown external image"
  in
  try (float_of_string lh, float_of_string lw)
  with Failure _ -> invalid_arg "Unknown external image"

let name = pp_print_string

let piccorner fmt p =
  match corner_reduce p with
  | `Northwest -> fprintf fmt "ulcorner"
  | `Northeast -> fprintf fmt "urcorner"
  | `Southwest -> fprintf fmt "llcorner"
  | `Southeast -> fprintf fmt "lrcorner"

let position fmt p =
  match pos_reduce p with
  | `Center -> fprintf fmt ""
  | `West -> fprintf fmt ".lft"
  | `East -> fprintf fmt ".rt"
  | `North -> fprintf fmt ".top"
  | `South -> fprintf fmt ".bot"
  | `Northwest -> fprintf fmt ".ulft"
  | `Northeast -> fprintf fmt ".urt"
  | `Southwest -> fprintf fmt ".llft"
  | `Southeast -> fprintf fmt ".lrt"

let rec num fmt f =
  if f = infinity then fprintf fmt "infinity"
  else if f > 4095. then fprintf fmt "%g" 4095.
  else if abs_float f < 0.0001 then fprintf fmt "0"
  else fprintf fmt "%g" f

and float fmt = num fmt

(* One bug : rgb and transparent can collide : rgb(0.123 0.003 0.001)
   et rgba(1,1,1,1) since this is encoding lie the first one *)
and scolor fmt = function
  | RGB (r, g, b) -> fprintf fmt "(%a, %a , %a@,)" float r float g float b
  | CMYK (c, m, y, k) ->
      fprintf fmt "(%a, %a, %a, %a@,)" float c float m float y float k
  | Gray f -> fprintf fmt "%a * white" float f

and color fmt = function
  | OPAQUE c -> scolor fmt c
  | TRANSPARENT (f, c) -> fprintf fmt "transparent (1,%a,%a@,)" float f scolor c

(* 1 is the "normal" mode *)
and point fmt { Point_lib.x; y } = fprintf fmt "(%a,@ %a@,)" num x num y

and transform fmt t = fprintf fmt "transformed %s" t

and picture fmt = function
  | C.PITex s -> fprintf fmt "btex %s etex" s
  | C.PITransformed (p, tr) -> fprintf fmt "((%a) %a@,)" picture p transform tr
  | C.PIName n -> pp_print_string fmt n

and path fmt = function
  | C.PAScope p -> fprintf fmt "(%a@,)" path p
  | C.PAFullCircle -> fprintf fmt "fullcircle"
  | C.PAHalfCircle -> fprintf fmt "halfcircle"
  | C.PAQuarterCircle -> fprintf fmt "quartercircle"
  | C.PAUnitSquare -> fprintf fmt "unitsquare"
  | C.PATransformed (p, tr) -> fprintf fmt "((%a) %a@,)" path p transform tr
  | C.PAAppend (p1, j, p2) -> fprintf fmt "%a %a@ %a" path p1 joint j path p2
  | C.PACycle (d, j, p) ->
      fprintf fmt "%a %a %acycle" path p joint j direction d
  | C.PAConcat (k, j, p) -> fprintf fmt "%a %a@ %a" path p joint j knot k
  | C.PAKnot k -> knot fmt k
  | C.PACutAfter (p1, p2) -> fprintf fmt "%a cutafter (%a)@ " path p2 path p1
  | C.PACutBefore (p1, p2) -> fprintf fmt "%a cutbefore (%a)@ " path p2 path p1
  | C.PABuildCycle l ->
      fprintf fmt "buildcycle(%a@,)" (Misc.print_list comma path) l
  | C.PASub (f1, f2, p) ->
      fprintf fmt "subpath(%a,%a) of %a" num f1 num f2 name p
  | C.PABBox p -> fprintf fmt "bbox %a" picture p
  | C.PAName n -> pp_print_string fmt n

and joint fmt = function
  | C.JLine -> fprintf fmt "--"
  | C.JCurve -> fprintf fmt ".."
  | C.JCurveNoInflex -> fprintf fmt "..."
  | C.JTension (a, b) -> fprintf fmt "..tension %a and %a .." float a float b
  | C.JControls (a, b) -> fprintf fmt "..controls %a and %a .." point a point b

and direction fmt = function
  | C.NoDir -> ()
  | C.Vec p ->
      fprintf fmt "{%a}" point p
      (* Why there is not the same thing than in Num?*)
  | C.Curl f -> fprintf fmt "{curl %a}" float f

and knot fmt (d1, p, d2) =
  fprintf fmt "%a%a%a" direction d1 point p direction d2

and dash fmt = function
  | C.DEvenly -> fprintf fmt "evenly"
  | C.DWithdots -> fprintf fmt "withdots"
  | C.DScaled (s, d) -> fprintf fmt "%a scaled %a" dash d num s
  | C.DShifted (p, d) -> fprintf fmt "%a shifted %a" dash d point p
  | C.DPattern l ->
      fprintf fmt "dashpattern(";
      List.iter
        (fun p ->
          let p, n =
            match p with C.On n -> ("on", n) | C.Off n -> ("off", n)
          in
          fprintf fmt "%s %a " p num n)
        l;
      fprintf fmt ")"

and pen fmt = function
  | C.PenCircle -> fprintf fmt "pencircle"
  | C.PenSquare -> fprintf fmt "pensquare"
  | C.PenFromPath p -> fprintf fmt "makepen (%a@,)" path p
  | C.PenTransformed (p, tr) -> fprintf fmt "%a %a" pen p transform tr

and command fmt = function
  | C.CDraw (pa, c, pe, dashed) ->
      fprintf fmt "@[<hov 2>draw@ %a@,%a@,%a@,%a;@]@\n" path pa
        (print_option " withcolor " color)
        c
        (print_option " withpen " pen)
        pe
        (print_option " dashed " dash)
        dashed
  | C.CDrawArrow (pa, c, pe, dashed) ->
      fprintf fmt "drawarrow %a%a%a%a;@\n" path pa
        (print_option " withcolor " color)
        c
        (print_option " withpen " pen)
        pe
        (print_option " dashed " dash)
        dashed
  | C.CFill (pa, c) ->
      fprintf fmt "fill %a%a;@\n" path pa (print_option " withcolor " color) c
  | C.CLabel (pic, pos, p) ->
      fprintf fmt "label%a(%a,@ %a); @\n" position pos picture pic point p
  | C.CDotLabel (pic, pos, p) ->
      fprintf fmt "@[<hov 2>dotlabel%a(%a,@ %a);@]@\n" position pos picture pic
        point p
  | C.CDrawPic p -> fprintf fmt "draw %a;@\n" picture p
  | C.CSeq l -> List.iter (fun c -> command fmt c) l
  | C.CDeclPath (n, p) -> fprintf fmt "path %s ;@\n%s = %a;@\n" n n path p
  | C.CDeclPoint (n, p) -> fprintf fmt "pair %s ;@\n%s = %a;@\n" n n point p
  | C.CDeclNum (n, nm) -> fprintf fmt "numeric %s ;@\n%s = %a;@\n" n n num nm
  | C.CSimplePic (pn1, pexpr) ->
      fprintf fmt "picture %s;@\n" pn1;
      fprintf fmt "%s := %a;@\n" pn1 picture pexpr
  | C.CDefPic (pic, cmd) ->
      (* Declpic (savepic, currentpicture);
       * Assign (currentpicture, nullpicture);
       * cmd;
       * Assign (pic, currentpicture);
       * Assign (currentpicture, savepic) *)
      let savepic = Name.picture () in
      fprintf fmt "picture %s, %s ;@\n" savepic pic;
      fprintf fmt "%s = currentpicture;@\n" savepic;
      fprintf fmt "currentpicture := nullpicture;@\n";
      command fmt cmd;
      fprintf fmt "%s = currentpicture;@\n" pic;
      fprintf fmt "currentpicture := %s;@\n" savepic
  | C.CDefTrans (n, t) ->
      fprintf fmt
        "transform %s ;@\n\
         xpart %s = %a;@\n\
         ypart %s = %a;@\n\
         xxpart %s = %a;@\n\
         xypart %s = %a;@\n\
         yxpart %s = %a;@\n\
         yypart %s = %a;@\n\
         @."
        n n num t.M.x0 n num t.M.y0 n num t.M.xx n num t.M.xy n num t.M.yx n num
        t.M.yy
  | C.CClip (pic, pth) -> fprintf fmt "clip %s to %a;@\n" pic path pth
  | C.CExternalImage (filename, spec) -> (
      match spec with
      | `Exact (h, w) ->
          fprintf fmt "externalfigure \"%s\" xyscaled (%a,%a);@\n" filename num
            w num h
      | (`None as spec)
      | (`Height _ as spec)
      | (`Width _ as spec)
      | (`Inside _ as spec) -> (
          let fh, fw = externalimage_dimension filename in
          let printext h w =
            fprintf fmt "externalfigure \"%s\" xyscaled (%a,%a);@\n" filename
              num w num h
          in
          match spec with
          | `None -> printext fh fw
          | `Height h -> printext h (fw /. fh *. h)
          | `Width w -> printext (fh /. fw *. w) w
          | `Inside (h, w) ->
              let w = min (h *. (fw /. fh)) w in
              printext (fh /. fw *. w) w ) )
