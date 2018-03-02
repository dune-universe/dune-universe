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

(* $Id: freetype.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $*)

type ('a, 'b) refbox = { cont : 'a; ref : 'b ref; }

type t = (Ftlow.library, unit) refbox

let init () =
  let t = {cont = Ftlow.init (); ref = ref ()} in
  Gc.finalise (fun v -> Ftlow.close v.cont) t;
  t

type face = (Ftlow.face, t) refbox

type face_info = Ftlow.face_info = {
    num_faces : int;
    num_glyphs : int;
    family_name : string;
    style_name : string;
    has_horizontal : bool;
    has_vertical : bool;
    has_kerning : bool;
    is_scalable : bool;
    is_sfnt : bool;
    is_fixed_width : bool;
    has_fixed_sizes : bool;
    has_fast_glyphs : bool;
    has_glyph_names : bool;
    has_multiple_masters : bool;
  }

let done_face face = Ftlow.done_face face.cont

let new_face t font idx =
  let face = {cont = Ftlow.new_face t.cont font idx; ref = ref t} in
  let info = Ftlow.face_info face.cont in
  Gc.finalise done_face face;
  face, info

let get_num_glyphs face = Ftlow.get_num_glyphs face.cont

let float_of_intfrac dotbits i =
  let d = float (1 lsl dotbits) in
  float i /. d

let intfrac_of_float dotbits f =
  let d = float (1 lsl dotbits) in
  truncate (f *. d)

let intfrac6_of_float = intfrac_of_float 6
let float_of_intfrac6 = float_of_intfrac 6
let intfrac16_of_float = intfrac_of_float 16
let float_of_intfrac16 = float_of_intfrac 16

let vector_float_of_intfrac6 (x, y) = float_of_intfrac6 x, float_of_intfrac6 y

let set_char_size face char_w char_h res_h res_v =
  Ftlow.set_char_size face.cont
    (intfrac6_of_float char_w)
    (intfrac6_of_float char_h)
    res_h res_v

let set_pixel_sizes face pixel_w pixel_h =
  Ftlow.set_pixel_sizes face.cont pixel_w pixel_h

type charmap = Ftlow.charmap = { platform_id : int; encoding_id : int; }

type char_index = int (* abstracted *)

let int_of_char_index = fun x -> x
let char_index_of_int = fun x -> x

let get_charmaps face = Ftlow.get_charmaps face.cont

let set_charmap face charmap = Ftlow.set_charmap face.cont charmap

let get_char_index face code = Ftlow.get_char_index face.cont code

type render_mode = Ftlow.render_mode = | Render_Normal | Render_Mono
type load_flag = Ftlow.load_flag = | Load_no_scale | Load_no_hinting

let load_glyph face index flags =
  vector_float_of_intfrac6 (Ftlow.load_glyph face.cont index flags)

let load_char face code flags =
  vector_float_of_intfrac6 (Ftlow.load_char face.cont code flags)

let render_glyph_of_face face render_mode =
  Ftlow.render_glyph_of_face face.cont render_mode

let render_glyph face index flags render_mode =
  vector_float_of_intfrac6
    (Ftlow.render_glyph face.cont index flags render_mode)

let render_char face code flags render_mode =
  vector_float_of_intfrac6
    (Ftlow.render_char face.cont code flags render_mode)

type matrix = { ft_xx : float; ft_xy : float; ft_yx : float; ft_yy : float; }
type vector = { ft_x : float; ft_y : float; }

let set_transform face mat vec =
  Ftlow.set_transform face.cont
    (intfrac16_of_float mat.ft_xx,
     intfrac16_of_float mat.ft_xy,
     intfrac16_of_float mat.ft_yx,
     intfrac16_of_float mat.ft_yy)

    (intfrac6_of_float vec.ft_x,
     intfrac6_of_float vec.ft_y)

let matrix_rotate r =
  let c = cos r
  and s = sin r in
  {ft_xx = c; ft_xy = -.s; ft_yx = s; ft_yy = c}

type bitmap_info = Ftlow.bitmap_info = {
    bitmap_left : int;
    bitmap_top : int;
    bitmap_width : int;
    bitmap_height : int;
  }

let get_bitmap_info face = Ftlow.get_bitmap_info face.cont

let read_bitmap face x y = Ftlow.read_bitmap face.cont x y

(* glyph metrics *)
type bbox = {
    xmin : float;
    ymin : float;
    xmax : float;
    ymax : float;
  }

type bearing_advance = {
    bearingx : float;
    bearingy : float;
    advance : float;
  }

type glyph_metrics = {
    gm_width : float;
    gm_height : float;
    gm_hori : bearing_advance;
    gm_vert : bearing_advance;
  }

let get_glyph_metrics face =
  let bearing_advance_float_of_intfrac6 ba =
    { bearingx = float_of_intfrac6 ba.Ftlow.bearingx;
      bearingy = float_of_intfrac6 ba.Ftlow.bearingy;
      advance = float_of_intfrac6 ba.Ftlow.advance; } in
  let gm = Ftlow.get_glyph_metrics face.cont in
  { gm_width = float_of_intfrac6 gm.Ftlow.gm_width;
    gm_height = float_of_intfrac6 gm.Ftlow.gm_height;
    gm_hori = bearing_advance_float_of_intfrac6 gm.Ftlow.gm_hori;
    gm_vert = bearing_advance_float_of_intfrac6 gm.Ftlow.gm_vert; }

(* size metrics *)

type size_metrics = {
    x_ppem : int;
    y_ppem : int;
    x_scale : float;
    y_scale : float;
  }

let get_size_metrics face =
  let low = Ftlow.get_size_metrics face.cont in
  { x_ppem = low.Ftlow.x_ppem;
    y_ppem = low.Ftlow.y_ppem;
    x_scale = float_of_intfrac16 low.Ftlow.x_scale;
    y_scale = float_of_intfrac16 low.Ftlow.y_scale; }

(* outline info *)
type outline_tag = Ftlow.outline_tag =
   | On_point | Off_point_conic | Off_point_cubic

type outline_contents = {
    n_contours : int;
    n_points : int;
    points : (float * float) array;
    tags : outline_tag array;
    contours : int array;
  }

let get_outline_contents face =
  let oc = Ftlow.get_outline_contents face.cont in
  { n_contours = oc.Ftlow.n_contours;
    n_points = oc.Ftlow.n_points;
    points =
      Array.map
        (fun (x, y) ->
           float_of_intfrac6 x, float_of_intfrac6 y)
        oc.Ftlow.points;
    tags = oc.Ftlow.tags;
    contours = oc.Ftlow.contours; }
