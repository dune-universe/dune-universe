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

let diameter_of_a_dot = 3.

let default_line_size = 1.

module BoundingBox : sig
  type pen = Spline_lib.path

  type t
  (** The type of the approximation *)

  (* val iter : (Spline.t -> unit) -> t -> unit *)
  val empty : t

  (* val create : ?base:pen -> Spline_lib.path -> t *)
  val of_path : ?base:pen -> Spline_lib.path -> t

  val union : t -> t -> t

  val transform : Matrix.t -> t -> t

  val bounding_box : t -> Point_lib.t * Point_lib.t

  val of_bounding_box : Point_lib.t * Point_lib.t -> t
end = struct
  (* A rendre plus performant ou pas*)
  (* le point correspond à un écart à prendre autour de la bounding box *)
  module S = Spline_lib
  module P = Point_lib

  type pen = S.path

  type t = (Spline.t list * pen) list

  (* let iter f l = List.iter (fun (e,_) -> List.iter (fun s -> f s) e) l *)
  let empty = []

  let create ?(base = S.Point P.zero) = function
    | S.Path p -> [ (p.S.pl, base) ]
    | S.Point p ->
        let x =
          match S.of_bounding_box (p, p) with
          | S.Path p -> p.S.pl
          | S.Point _ -> assert false
        in
        [ (x, base) ]

  let of_path = create

  let union x y = List.rev_append x y

  let transform t x =
    List.map
      (fun (x, f) ->
        ( List.map (Spline.transform t) x,
          S.transform (Matrix.remove_translation t) f ))
      x

  open P
  open P.Infix

  let bounding_box sl =
    let x_min, y_min, x_max, y_max =
      P.list_min_max_float
        (fun (e, f) ->
          let x_min, y_min, x_max, y_max =
            P.list_min_max_float Spline.precise_bounding_box e
          in
          let pen_min, pen_max = S.bounding_box f in
          let p1, p2 =
            ( { x = x_min; y = y_min } +/ pen_min,
              { x = x_max; y = y_max } +/ pen_max )
          in
          (p1.x, p1.y, p2.x, p2.y))
        sl
    in
    ({ x = x_min; y = y_min }, { x = x_max; y = y_max })

  let of_bounding_box l = create (S.of_bounding_box l)
end

module MP = Metapath_lib
module P = Point_lib
module S = BoundingBox

type transform = Matrix.t

type num = float

type dash = float * num list

type pen = transform

type color = Concrete_types.color

type path = Spline_lib.path

type id = int

type interactive =
  | IntEmpty
  | IntTransform of interactive * transform
  | IntClip of interactive * path
  | IntOnTop of interactive * interactive
  | Inter of path * id

type commands =
  | Empty
  | Transform of transform * commands
  | OnTop of commands list
  | Tex of Gentex.t
  | Stroke_path of path * color option * pen * dash option
  | Fill_path of path * color option
  | Clip of commands * path
  | ExternalImage of string * float * transform

and t = { fcl : commands; fb : BoundingBox.t; fi : interactive }

let content x = x.fcl

let empty = { fcl = Empty; fb = S.empty; fi = IntEmpty }

let tex t =
  { fcl = Tex t; fb = S.of_bounding_box (Gentex.bounding_box t); fi = IntEmpty }

let fill_path p c = { fcl = Fill_path (p, c); fb = S.of_path p; fi = IntEmpty }

let base_of_pen pen =
  Spline_lib.transform pen (MP.Approx.fullcircle default_line_size)

let stroke_path p c pen d =
  {
    fcl = Stroke_path (p, c, pen, d);
    fb = S.of_path ~base:(base_of_pen pen) p;
    fi = IntEmpty;
  }

let draw_point p =
  stroke_path
    (Spline_lib.create_point p)
    None
    (Matrix.scale diameter_of_a_dot)
    None

let clip p path =
  {
    fcl = Clip (p.fcl, path);
    fb = S.of_path path;
    (* la bounding box d'un clip est la bounding_box du chemin fermé*)
    fi = IntClip (p.fi, path);
  }

let externalimage_dimension filename : float * float =
  let inch =
    Unix.open_process_in
      (Format.sprintf "identify -format \"%%h\\n%%w\" \"%s\"" filename)
  in
  try
    let h = float_of_string (input_line inch) in
    let w = float_of_string (input_line inch) in
    (h, w)
  with End_of_file | Failure _ ->
    invalid_arg (Format.sprintf "Unknown external image %s" filename)

let external_image filename spec =
  let fh, fw = externalimage_dimension filename in
  let height, width =
    match spec with
    | `Exact (h, w) -> (h, w)
    | `None -> (fh, fw)
    | `Height h -> (h, fw /. fh *. h)
    | `Width w -> (fh /. fw *. w, w)
    | `Inside (h, w) ->
        let w = min (h *. (fw /. fh)) w in
        (fh /. fw *. w, w)
  in
  (* TODO : width/.fw pour cairo width pour mps *)
  let m = Matrix.multiply (Matrix.xscaled width) (Matrix.yscaled height) in
  {
    fcl = ExternalImage (filename, height, m);
    fb = S.of_bounding_box (P.zero, { P.x = width; y = height });
    fi = IntEmpty;
  }

let interactive path id = { fcl = Empty; fb = S.empty; fi = Inter (path, id) }

let is_empty t = t.fcl = Empty

let on_top t1 t2 =
  if is_empty t1 then t2
  else if is_empty t2 then t1
  else
    {
      fcl = OnTop [ t1.fcl; t2.fcl ];
      fb = S.union t1.fb t2.fb;
      fi = IntOnTop (t1.fi, t2.fi);
    }

let transform m t =
  {
    fcl = Transform (m, t.fcl);
    fb = S.transform m t.fb;
    fi = IntTransform (t.fi, m);
  }

let shift t w h = transform (Matrix.xy_translation w h) t

let bounding_box t = S.bounding_box t.fb

let baseline p = match p.fcl with Tex tex -> Gentex.get_bases_pt tex | _ -> []

let apply_transform_cmds t =
  let rec aux pic =
    match pic with
    | Empty -> Empty
    | OnTop l -> OnTop (List.map aux l)
    | Fill_path (p, c) -> Fill_path (path p, c)
    | Stroke_path (pa, c, pe, d) -> Stroke_path (path pa, c, pe, d)
    | Clip (cmds, p) -> Clip (aux cmds, path p)
    | Tex g -> Tex { g with Gentex.trans = Matrix.multiply t g.Gentex.trans }
    | ExternalImage (f, h, m) -> ExternalImage (f, h, Matrix.multiply t m)
    | Transform (t', l) -> Transform (Matrix.multiply t' t, l)
  and path p = Spline_lib.transform t p in
  aux

let iter f t =
  let rec aux p =
    f p;
    match p with
    | Empty | Fill_path _ | Stroke_path _ | ExternalImage _ | Tex _ -> ()
    | OnTop l -> List.iter aux l
    | Clip (c, _) -> aux c
    | Transform (_, l) -> aux l
  in
  aux (content t)

let apply_transform t p =
  {
    p with
    fcl = apply_transform_cmds t p.fcl;
    fb = BoundingBox.transform t p.fb;
  }

module Dash = struct
  type t = float * float list

  type input_dash = On of float | Off of float

  let shifted f (x, d) = (x +. f, d)

  let line = (0., [ 3.; 3. ])

  let dots = (0., [ 0.; 5. ])

  let rec on acc = function
    | [] -> [ acc ]
    | On f :: l -> on (f +. acc) l
    | Off f :: l -> acc :: off f l

  and off acc = function
    | [] -> [ acc ]
    | On f :: l -> acc :: on f l
    | Off f :: l -> off (f +. acc) l

  and to_dash = function
    | [] -> []
    | On f :: l -> on f l
    | Off f :: l -> 0. :: off f l

  let pattern l = (0., to_dash l)

  let scale f (x, l) = (x, List.map (fun z -> f *. z) l)
end

module Print = struct
  (* debug printing *)

  open Format

  let rec command fmt c =
    match c with
    | Empty -> pp_print_string fmt "empty"
    | Stroke_path (p, _, _, _) -> Spline_lib.print fmt p
    | Tex g -> Gentex.deb_print fmt g
    | OnTop cl -> Misc.print_list Misc.newline command fmt cl
    (*
    | Transform of transform * commands
    | Fill_path of path * color option
    | Clip of commands  * path
    | ExternalImage of string * float * float
*)
    | _ -> assert false

  let pic fmt p = command fmt p.fcl
end
