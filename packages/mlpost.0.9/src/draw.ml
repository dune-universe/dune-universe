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

open Point_lib
module S = Spline_lib

let draw_tex cr tex =
  Cairo.save cr;
  Cairo.transform cr tex.Gentex.trans;
  Dvicairo.draw
    { Dvicairo.pic = cr; x_origin = 0.; y_origin = 0. }
    tex.Gentex.tex;
  Cairo.restore cr

(*;Format.printf "Gentex : %a@." print tex*)

module MetaPath = struct
  type pen = Matrix.t

  let curve_to cr s =
    let _, sb, sc, sd = Spline.explode s in
    Cairo.curve_to cr sb.x sb.y sc.x sc.y sd.x sd.y

  let draw_path cr = function
    | S.Path p ->
        ( match p.S.pl with
        | [] -> assert false
        | x :: _ as l ->
            let sa = Spline.left_point x in
            Cairo.move_to cr sa.x sa.y;
            List.iter (curve_to cr) l );
        if p.S.cycle then Cairo.Path.close cr
    | S.Point _ -> failwith "Metapost fail in that case what should I do???"

  let stroke cr pen = function
    | S.Path _ as path ->
        (*Format.printf "stroke : %a@." S.print path;*)
        draw_path cr path;
        Cairo.save cr;
        (*Matrix.set*) Cairo.transform cr pen;
        Cairo.stroke cr;
        Cairo.restore cr
    | S.Point p ->
        (*Format.printf "stroke : %a@." S.print path;*)
        Cairo.save cr;
        Cairo.transform cr (Matrix.translation p);
        Cairo.transform cr pen;
        draw_path cr (Metapath_lib.Approx.fullcircle 1.);
        Cairo.fill cr;
        Cairo.restore cr

  let fill cr path =
    draw_path cr path;
    Cairo.fill cr
end

module Picture = struct
  open Concrete_types

  exception Not_implemented of string

  let not_implemented s = raise (Not_implemented s)

  let rec color cr = function
    | OPAQUE (RGB (r, g, b)) -> Cairo.set_source_rgb cr r g b
    | OPAQUE (CMYK _) -> not_implemented "cmyk"
    | OPAQUE (Gray g) -> color cr (OPAQUE (RGB (g, g, g)))
    | TRANSPARENT (a, RGB (r, g, b)) -> Cairo.set_source_rgba cr r g b a
    | TRANSPARENT (_, CMYK _) -> not_implemented "cmyk"
    | TRANSPARENT (a, Gray g) -> color cr (TRANSPARENT (a, RGB (g, g, g)))

  let color_option cr = function None -> () | Some c -> color cr c

  let dash cr = function
    | None | Some (_, []) -> ()
    | Some (ofs, l) -> Cairo.set_dash cr (Array.of_list l) ~ofs

  let inversey cr height =
    Cairo.translate cr 0. height;
    Cairo.scale cr 1. (-1.)

  open Picture_lib

  let rec draw_aux cr = function
    | Empty -> ()
    | Transform (m, t) ->
        Cairo.save cr;
        Cairo.transform cr m;
        (*Format.printf "Transform : %a@." Matrix.print m;*)
        draw_aux cr t;
        Cairo.restore cr
    | OnTop l -> List.iter (draw_aux cr) l
    | Tex t ->
        Cairo.save cr;
        Cairo.scale cr 1. (-1.);
        draw_tex cr t;
        Cairo.restore cr
    | Stroke_path (path, c, pen, d) ->
        Cairo.save cr;
        color_option cr c;
        dash cr d;
        MetaPath.stroke cr pen path;
        Cairo.restore cr
    | Fill_path (path, c) ->
        Cairo.save cr;
        color_option cr c;
        MetaPath.fill cr path;
        Cairo.restore cr
    | Clip (com, p) ->
        Cairo.save cr;
        MetaPath.draw_path cr p;
        Cairo.clip cr;
        draw_aux cr com;
        Cairo.restore cr
    | ExternalImage (filename, height, m) ->
        Cairo.save cr;
        Cairo.transform cr m;
        inversey cr height;
        let img = Cairo.PNG.create filename in
        Cairo.set_source_surface cr img ~x:0. ~y:0.;
        Cairo.paint cr;
        Cairo.restore cr

  let draw cr _width height p =
    Cairo.save cr;
    inversey cr height;
    Cairo.set_line_width cr default_line_size;
    (* Only elliptical pens use the stroke command *)
    Cairo.set_line_cap cr Cairo.ROUND;
    Cairo.set_line_join cr Cairo.JOIN_ROUND;
    draw_aux cr (content p);
    Cairo.restore cr

  let where _cr _t (_x, _y) = not_implemented "where"

  let move _t _id _p = not_implemented "move"
end
