(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = CairoMock.S

(*
Canvas:
- http://www.w3schools.com/tags/ref_canvas.asp
- https://ocsigen.org/js_of_ocaml/2.8.4/api/Dom_html.canvasRenderingContext2D-c

Cairo:
- http://cairo.forge.ocamlcore.org/tutorial/Cairo.html
- https://github.com/Chris00/ocaml-cairo
- utop -require cairo2
*)

#include "Backend.incl.ml"

let () = Printexc.register_printer (function
  | Error status -> Some (Printf.sprintf "JsOfOCairo.Error(%s)" (status_repr status))
  | _ -> None
)

module Html = struct
  type t = Dom_html.canvasRenderingContext2D Js.t
end

module Local: sig
  type t

  val create: unit -> t

  val save: t -> unit
  val restore: t -> unit

  val transformation: t -> Matrix.t
  val set_transformation: t -> transformation:Matrix.t -> unit

  val font: t -> font
  val set_font: t -> font:font -> unit

  val source: t -> Pattern.source
  val set_source: t -> source:Pattern.source -> unit

  val fill_rule: t -> fill_rule
  val set_fill_rule: t -> fill_rule:fill_rule -> unit

  val set_start_point: t -> float * float -> unit
  val set_start_point_if_none: t -> float * float -> unit
  val reset_start_point: t -> unit
  val set_start_point_as_current_point: t -> unit

  val current_point: t -> (float * float) option
  val set_current_point: t -> float * float -> unit
  val reset_current_point: t -> unit
end = struct
  module State = struct
    type t = {
      transformation: Matrix.t;
      font: font;
      source: Pattern.source;
      fill_rule: fill_rule;
    }
  end

  type t = {
    points: Points.t;
    mutable states: State.t list;
  }

  let create () = {
    points = Points.create ();
    states = [
      {
        transformation = Matrix.init_identity ();
        font = {
          slant = Upright;
          weight = Normal;
          size = 10.;
          family = "sans-serif";
        };
        source = !(Pattern.create_rgb 0. 0. 0.);
        fill_rule = WINDING;
      };
    ];
  }

  let state context =
    List.hd context.states

  let set_state context state =
    context.states <- state::(List.tl context.states)

  let save context =
    context.states <- (state context)::context.states

  let restore context =
    let states =
      match context.states with
        | [] | [_] -> raise (Error INVALID_RESTORE)
        | _::states -> states
    in
    context.states <- states

  let transformation context =
    (state context).transformation

  let set_transformation context ~transformation =
    set_state context ({(state context) with transformation})

  let font context =
    (state context).font

  let set_font context ~font =
    set_state context ({(state context) with font})

  let source context =
    (state context).source

  let set_source context ~source =
    set_state context ({(state context) with source})

  let fill_rule context =
    (state context).fill_rule

  let set_fill_rule context ~fill_rule =
    set_state context ({(state context) with fill_rule})

  let set_start_point context (x, y) =
    let transformation = transformation context in
    Points.set_start context.points ~transformation ~x ~y

  let reset_start_point context =
    Points.reset_start context.points

  let set_start_point_if_none context (x, y) =
    let transformation = transformation context in
    Points.set_start_if_none context.points ~transformation ~x ~y

  let set_start_point_as_current_point context =
    Points.set_current_from_start context.points

  let current_point context =
    let transformation = transformation context in
    Points.current context.points ~transformation

  let set_current_point context (x, y) =
    let transformation = transformation context in
    Points.set_current context.points ~transformation ~x ~y

  let reset_current_point context =
    Points.reset_current context.points
end

type context = {
  html: Html.t;
  local: Local.t;
}


let save context =
  context.html##save;
  Local.save context.local

let restore context =
  context.html##restore;
  Local.restore context.local


let set_matrix context ({xx; xy; yx; yy; x0; y0} as transformation) =
  context.html##setTransform xx yx xy yy x0 y0;
  Local.set_transformation context.local ~transformation

let get_matrix context =
  Local.transformation context.local

let transform context m =
  set_matrix context (Matrix.multiply (Local.transformation context.local) m)

let scale context x y =
  transform context (Matrix.init_scale x y)

let translate context x y =
  transform context (Matrix.init_translate x y)

let rotate context angle =
  transform context (Matrix.init_rotate angle)

let identity_matrix context =
  set_matrix context (Matrix.init_identity ())

let device_to_user context x y =
  Matrix.transform_point (Matrix.init_inverse (Local.transformation context.local)) x y

let device_to_user_distance context dx dy =
  Matrix.transform_distance (Matrix.init_inverse (Local.transformation context.local)) ~dx ~dy

let user_to_device context x y =
  Matrix.transform_point (Local.transformation context.local) x y

let user_to_device_distance context dx dy =
  Matrix.transform_distance (Local.transformation context.local) ~dx ~dy


let make_rel context ~x:dx ~y:dy =
  match Local.current_point context.local with
    | None -> raise (Error NO_CURRENT_POINT)
    | Some (x, y) -> (x +. dx, y +. dy)

let move_to context x y =
  context.html##moveTo x y;
  Local.set_start_point context.local (x, y);
  Local.set_start_point_as_current_point context.local

let rel_move_to context x y =
  let (x, y) = make_rel context ~x ~y in
  move_to context x y

let line_to context x y =
  context.html##lineTo x y;
  Local.set_start_point_if_none context.local (x, y);
  Local.set_current_point context.local (x, y)

let rel_line_to context x y =
  let (x, y) = make_rel context ~x ~y in
  line_to context x y

let curve_to context x1 y1 x2 y2 x3 y3 =
  context.html##bezierCurveTo x1 y1 x2 y2 x3 y3;
  Local.set_start_point_if_none context.local (x1, y1);
  Local.set_current_point context.local (x3, y3)

let rel_curve_to context x1 y1 x2 y2 x3 y3 =
  let (x1, y1) = make_rel context ~x:x1 ~y:y1
  and (x2, y2) = make_rel context ~x:x2 ~y:y2
  and (x3, y3) = make_rel context ~x:x3 ~y:y3 in
  curve_to context x1 y1 x2 y2 x3 y3

let rectangle context x y ~w ~h =
  Local.set_current_point context.local (x, y);
  context.html##rect x y w h

let arc_ ~dir context x y ~r ~a1 ~a2 =
  context.html##arc x y r a1 a2 dir;
  Local.set_start_point_if_none context.local (x +. r *. (cos a1), y +. r *. (sin a1));
  Local.set_current_point context.local (x +. r *. (cos a2), y +. r *. (sin a2))

let arc = arc_ ~dir:Js._false

let arc_negative = arc_ ~dir:Js._true

module Path = struct
  let get_current_point context =
    match Local.current_point context.local with
      | None -> (0., 0.)
      | Some (x, y) -> (x, y)

  let clear context =
    context.html##beginPath;
    Local.reset_start_point context.local;
    Local.reset_current_point context.local

  let close context =
    context.html##closePath;
    Local.set_start_point_as_current_point context.local
end

let stroke_preserve context =
  context.html##stroke

let stroke context =
  stroke_preserve context;
  Path.clear context

let fill_preserve context =
  match Local.fill_rule context.local with
    | WINDING -> context.html##fill
    | EVEN_ODD -> (Js.Unsafe.coerce context.html)##fill (Js.string "evenodd")

let fill context =
  fill_preserve context;
  Path.clear context

let clip_preserve context =
  context.html##clip

let clip context =
  clip_preserve context;
  Path.clear context

let paint ?(alpha=1.) context =
  save context;
  context.html##.globalAlpha := alpha;
  identity_matrix context;
  let width = (float_of_int context.html##.canvas##.width)
  and height = (float_of_int context.html##.canvas##.height) in
  context.html##fillRect 0. 0. width height;
  restore context


let set_line_width context width =
  context.html##.lineWidth := width

let get_line_width context =
  context.html##.lineWidth

let set_dash context ?(ofs=0.) dashes =
  let html = Js.Unsafe.coerce context.html in
  html##.lineDashOffset := ofs;
  html##setLineDash (Js.array dashes)

let get_dash context =
  let html = Js.Unsafe.coerce context.html in
  (Js.to_array (html##getLineDash), html##.lineDashOffset)

let set_fill_rule context fill_rule =
  Local.set_fill_rule context.local ~fill_rule

let get_fill_rule context =
  Local.fill_rule context.local

let set_line_cap context cap =
  let cap = match cap with
    | BUTT -> "butt"
    | ROUND -> "round"
    | SQUARE -> "square"
  in
  context.html##.lineCap := Js.string cap

let get_line_cap context =
  match Js.to_string context.html##.lineCap with
    | "round" -> ROUND
    | "square" -> SQUARE
    | _ -> BUTT

let set_line_join context join =
  let join = match join with
    | JOIN_MITER ->  "miter"
    | JOIN_ROUND -> "round"
    | JOIN_BEVEL -> "bevel"
  in
  context.html##.lineJoin := Js.string join

let get_line_join context =
  match Js.to_string context.html##.lineJoin with
    | "round" -> JOIN_ROUND
    | "bevel" -> JOIN_BEVEL
    | _ -> JOIN_MITER

let set_miter_limit context l =
  context.html##.miterLimit := l

let get_miter_limit context =
  context.html##.miterLimit

let set_operator context operator =
  let operator = match operator with
    | CLEAR -> failwith "Unsupported operator CLEAR"
    | SOURCE -> failwith "Unsupported operator SOURCE"
    | OVER -> "source-over"
    | ATOP -> "source-atop"
    | IN -> "source-in"
    | OUT -> "source-out"
    | DEST_OVER -> "destination-over"
    | DEST_ATOP -> "destination-atop"
    | DEST_IN -> "destination-in"
    | DEST_OUT -> "destination-out"
    | ADD -> "lighter"
    | XOR -> "xor"
    | DEST -> failwith "Unsupported operator DEST"
    | SATURATE -> failwith "Unsupported operator SATURATE"
  in
  context.html##.globalCompositeOperation := Js.string operator

let get_operator context =
  match Js.to_string context.html##.globalCompositeOperation with
    | "over" -> OVER (* Special case for node-canvas which seems to have a wrong default value *)
    | "add" -> ADD (* Special case for node-canvas *)
    | "source-over" -> OVER
    | "source-atop" -> ATOP
    | "source-in" -> IN
    | "source-out" -> OUT
    | "destination-over" -> DEST_OVER
    | "destination-atop" -> DEST_ATOP
    | "destination-in" -> DEST_IN
    | "destination-out" -> DEST_OUT
    | "lighter" -> ADD
    | "xor" -> XOR
    | op -> failwith (Printf.sprintf "Unexpected globalCompositeOperation %S" op)


let set_source context pattern =
  let convert x = string_of_int (int_of_float (255.0 *. x)) in
  let convert_rgba r g b a = Js.string (Printf.sprintf "rgba(%s, %s, %s, %f)" (convert r) (convert g) (convert b) a) in
  let source = !pattern in
  Local.set_source context.local ~source;
  match source with
    | Pattern.Rgba (r, g, b, a) ->
      let color = convert_rgba r g b a in
      context.html##.fillStyle := color;
      context.html##.strokeStyle := color
    | Pattern.LinearGradient {points=(x0, y0, x1, y1); stop_points} ->
      let gradient = context.html##createLinearGradient x0 y0 x1 y1 in
      stop_points
      |> Pattern.StopPointList.to_list
      |> List.iter ~f:(fun (position, r, g, b, a) ->
        gradient##addColorStop position (convert_rgba r g b a)
      );
      context.html##.fillStyle_gradient := gradient;
      context.html##.strokeStyle_gradient := gradient
    | Pattern.RadialGradient {circles=(x0, y0, r0, x1, y1, r1); stop_points} ->
      let gradient = context.html##createRadialGradient x0 y0 r0 x1 y1 r1 in
      stop_points
      |> Pattern.StopPointList.to_list
      |> List.iter ~f:(fun (position, r, g, b, a) ->
        gradient##addColorStop position (convert_rgba r g b a)
      );
      context.html##.fillStyle_gradient := gradient;
      context.html##.strokeStyle_gradient := gradient
    | Pattern.TypeMismatch ->
      ()

let get_source context =
  ref (Local.source context.local)

let set_source_rgb context r g b =
  set_source context (Pattern.create_rgb r g b)

let set_source_rgba context r g b a =
  set_source context (Pattern.create_rgba r g b a)


let _set_font context ({slant; weight; size; family} as font) =
  Local.set_font context.local ~font;
  let font_style = match slant with
    | Upright -> "normal"
    | Italic -> "italic"
    | Oblique -> "oblique"
  and font_weight = match weight with
    | Normal -> "normal"
    | Bold -> "bold"
  in
  let font = Printf.sprintf "%s %s %npx %s" font_style font_weight (int_of_float size) family in
  context.html##.font := Js.string font

let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
  _set_font context {(Local.font context.local) with slant; weight; family}

let set_font_size context size =
  _set_font context {(Local.font context.local) with size}

let show_text context s =
  let (x, y) = Path.get_current_point context
  and w = (context.html##measureText (Js.string s))##.width in
  Local.set_current_point context.local (x +. w, y);
  context.html##fillText (Js.string s) x y

let font_extents context =
  let {size; _} = (Local.font context.local) in
  {
    ascent = size;
    descent = size /. 4.;
    baseline = 0.;
    max_x_advance = 2. *. size;
    max_y_advance = 0.;
  }

let text_extents context s =
  let {size; _} = (Local.font context.local)
  and w = (context.html##measureText (Js.string s))##.width in
  {
    x_bearing = 0.;
    y_bearing = 0.;
    width = w;
    height = size;
    x_advance = w;
    y_advance = 0.;
  }

let create canvas =
  let html = canvas##getContext Dom_html._2d_
  and local = Local.create () in
  let context = {
    html;
    local;
  } in
  set_line_width context 2.0;
  context
