open Printf
open Bigarray
open Graphics
open Mesh

type mesh = LAYOUT t
type 'a vector = 'a vec     (* global vec *)
type vec = LAYOUT vector  (* local vec *)

(* FIXME: naive, remove it when 4.00.0 will be spread enough *)
let hypot x y = sqrt(x *. x +. y *. y)


(** Return the smaller box (xmin, xmax, ymin, ymax) containing the [mesh]. *)
let bounding_box (mesh: mesh) =
  let pt = mesh#point in
  let xmin = ref infinity
  and xmax = ref neg_infinity
  and ymin = ref infinity
  and ymax = ref neg_infinity in
  for i = FST to LASTCOL(pt) do
    let x = GET(pt, FST,i)
    and y = GET(pt, SND,i) in
    if x > !xmax then xmax := x;
    if x < !xmin then xmin := x;
    if y > !ymax then ymax := y;
    if y < !ymin then ymin := y;
  done;
  (!xmin, !xmax, !ymin, !ymax)

(** Contains the information to transform "mesh coordinates" into the
    graphics ones. *)
type surf = { hx: float; hy: float;  xbd: int;  ybd: int;
              xmin: float;  ymin: float }
;;
let pixel_x s x = truncate((x -. s.xmin) *. s.hx) + s.xbd
let pixel_y s y = truncate((y -. s.ymin) *. s.hy) + s.ybd

let make_surf mesh width height =
  let xmin, xmax, ymin, ymax = bounding_box mesh in
  let hx = float width /. (xmax -. xmin)
  and hy = float height /. (ymax -. ymin) in
  let (xbd, ybd) = current_point() in
  { hx = hx; hy = hy;  xbd = xbd; ybd = ybd;  xmin = xmin; ymin = ymin }

let draw ?(width=600) ?(height=600) ?(color=foreground) ?(points=true)
         ?point_idx ?triangle_idx ?voronoi
         ?point_marker_color (mesh: mesh) =
  let surf = make_surf mesh width height in
  (* Triangles and Points *)
  let pt = mesh#point
  and triangle = mesh#triangle in
  let triangle_idx = match triangle_idx with
    | None -> (fun _ _ _ _ _ _ _ -> ())
    | Some f -> (fun t x0 y0 x1 y1 x2 y2 ->
                (* Move to the incenter of the triangle. *)
                let d01 = hypot (x0 -. x1) (y0 -. y1)
                and d02 = hypot (x0 -. x2) (y0 -. y2)
                and d12 = hypot (x1 -. x2) (y1 -. y2) in
                let d = d12 +. d02 +. d01 in
                let x = (d12 *. x0 +. d02 *. x1 +. d01 *. x2) /. d
                and y = (d12 *. y0 +. d02 *. y1 +. d01 *. y2) /. d in
                moveto (pixel_x surf x) (pixel_y surf y);
                f t;
                set_color color) in
  set_color color;
  for t = FST to LASTCOL(triangle) do
    (* Draw triangle [t]. *)
    let i0 = GET(triangle, FST,t)
    and i1 = GET(triangle, SND,t)
    and i2 = GET(triangle, THIRD,t) in
    try
      let x0 = GET(pt, FST,i0) and y0 = GET(pt, SND,i0) in
      let x1 = GET(pt, FST,i1) and y1 = GET(pt, SND,i1) in
      let x2 = GET(pt, FST,i2) and y2 = GET(pt, SND,i2) in
      let px0 = pixel_x surf x0 and py0 = pixel_y surf y0 in
      let px1 = pixel_x surf x1 and py1 = pixel_y surf y1 in
      let px2 = pixel_x surf x2 and py2 = pixel_y surf y2 in
      draw_segments [| (px0, py0, px1, py1); (px1, py1, px2, py2);
                       (px2, py2, px0, py0) |];
      triangle_idx t x0 y0 x1 y1 x2 y2;
    with e ->
      eprintf "Mesh_display: triangle %i (%i,%i,%i): %s\n%!"
              t i0 i1 i2 (Printexc.to_string e)
  done;
  let marker = match point_marker_color with
    | None -> (fun _ _ _ -> ())
    | Some c -> (fun m px py ->
                  set_color c;
                  moveto px py;
                  draw_string(string_of_int m);
                  set_color color
               ) in
  let point_idx = match point_idx with
    | None -> (fun _ _ _ _ -> ())
    | Some f -> (fun _s px py i ->
                moveto px py;
                f i;
                set_color color) in
  if points then begin
    let pt_marker = mesh#point_marker in
    for i = FST to LASTCOL(pt) do
      let x = GET(pt, FST,i)  and y = GET(pt, SND,i) in
      let px = pixel_x surf x
      and py = pixel_y surf y in
      fill_circle px py 3; (* draw point *)
      point_idx surf px py i;
      marker pt_marker.{i} px py
    done;
  end;
  (* Voronoi diagram *)
  begin match voronoi with
  | None -> ()
  | Some _vor -> ()                      (* FIXME: todo *)
  end
;;

type point = { x : float; y : float }

(* For level curves, we just draw a dot. *)
let point s _i {x=x; y=y} =
  draw_rect (pixel_x s x) (pixel_y s y) 1 1

let line s color {x=x0; y=y0} {x=x1; y=y1} =
  set_color color;
  draw_segments [| (pixel_x s x0, pixel_y s y0,
                    pixel_x s x1, pixel_y s y1) |]

let triangle s color {x=x0; y=y0} {x=x1; y=y1} {x=x2; y=y2} =
  set_color color;
  fill_poly [| (pixel_x s x0, pixel_y s y0);
               (pixel_x s x1, pixel_y s y1);
               (pixel_x s x2, pixel_y s y2) |]

let rec array_of_points s pts =
  let l = List.length pts in
  let apts = Array.make l (0,0) in
  fill_array_of_points s apts 0 pts;
  apts
and fill_array_of_points s apts i = function
  | [] -> ()
  | pt :: tl ->
    apts.(i) <- (pixel_x s pt.x, pixel_y s pt.y);
    fill_array_of_points s apts (i + 1) tl

let fill_triangle = triangle

let fill_quadrilateral s color {x=x0; y=y0} {x=x1; y=y1} {x=x2; y=y2}
                       {x=x3; y=y3} =
  set_color color;
  fill_poly [| (pixel_x s x0, pixel_y s y0);
               (pixel_x s x1, pixel_y s y1);
               (pixel_x s x2, pixel_y s y2);
               (pixel_x s x3, pixel_y s y3) |]


(************************************************************************)
(* Include peformed by make_FC_code.ml *)
INCLUDE(../src/mesh_level_curvesFC.ml);;
(************************************************************************)

let level_curves ~width ~height ?(boundary=(fun _ -> Some 0))
    (mesh: mesh) (z: vec) ?level_eq levels =
  let surf = make_surf mesh width height in
  draw_levels ~boundary mesh z ?level_eq levels surf

let super_level ~width ~height ?(boundary=(fun _ -> Some 0))
    (mesh: mesh) (z: vec) level color =
  let surf = make_surf mesh width height in
  draw_super_level ~boundary mesh z level color surf

let sub_level ~width ~height ?(boundary=(fun _ -> Some 0))
    (mesh: mesh) (z: vec) level color =
  let surf = make_surf mesh width height in
  draw_sub_level ~boundary mesh z level color surf
