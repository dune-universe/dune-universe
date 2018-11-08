(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

type status =
  | INVALID_RESTORE
  | INVALID_POP_GROUP
  | NO_CURRENT_POINT
  | INVALID_MATRIX
  | INVALID_STATUS
  | NULL_POINTER
  | INVALID_STRING
  | INVALID_PATH_DATA
  | READ_ERROR
  | WRITE_ERROR
  | SURFACE_FINISHED
  | SURFACE_TYPE_MISMATCH
  | PATTERN_TYPE_MISMATCH
  | INVALID_CONTENT
  | INVALID_FORMAT
  | INVALID_VISUAL
  | FILE_NOT_FOUND
  | INVALID_DASH
  | INVALID_DSC_COMMENT
  | INVALID_INDEX
  | CLIP_NOT_REPRESENTABLE
  | TEMP_FILE_ERROR
  | INVALID_STRIDE
  | FONT_TYPE_MISMATCH
  | USER_FONT_IMMUTABLE
  | USER_FONT_ERROR
  | NEGATIVE_COUNT
  | INVALID_CLUSTERS
  | INVALID_SLANT
  | INVALID_WEIGHT
  | INVALID_SIZE
  | USER_FONT_NOT_IMPLEMENTED
  | DEVICE_TYPE_MISMATCH
  | DEVICE_ERROR
  | INVALID_MESH_CONSTRUCTION
  | DEVICE_FINISHED
  | JBIG2_GLOBAL_MISSING

exception Error of status

let status_to_string = function
  | INVALID_RESTORE -> "cairo_restore() without matching cairo_save()"
  | INVALID_POP_GROUP -> "no saved group to pop, i.e. cairo_pop_group() without matching cairo_push_group()"
  | NO_CURRENT_POINT -> "no current point defined"
  | INVALID_MATRIX -> "invalid matrix (not invertible)"
  | INVALID_STATUS -> "invalid value for an input cairo_status_t"
  | NULL_POINTER -> "NULL pointer"
  | INVALID_STRING -> "input string not valid UTF-8"
  | INVALID_PATH_DATA -> "input path data not valid"
  | READ_ERROR -> "error while reading from input stream"
  | WRITE_ERROR -> "error while writing to output stream"
  | SURFACE_FINISHED -> "the target surface has been finished"
  | SURFACE_TYPE_MISMATCH -> "the surface type is not appropriate for the operation"
  | PATTERN_TYPE_MISMATCH -> "the pattern type is not appropriate for the operation"
  | INVALID_CONTENT -> "invalid value for an input cairo_content_t"
  | INVALID_FORMAT -> "invalid value for an input cairo_format_t"
  | INVALID_VISUAL -> "invalid value for an input Visual*"
  | FILE_NOT_FOUND -> "file not found"
  | INVALID_DASH -> "invalid value for a dash setting"
  | INVALID_DSC_COMMENT -> "invalid value for a DSC comment"
  | INVALID_INDEX -> "invalid index passed to getter"
  | CLIP_NOT_REPRESENTABLE -> "clip region not representable in desired format"
  | TEMP_FILE_ERROR -> "error creating or writing to a temporary file"
  | INVALID_STRIDE -> "invalid value for stride"
  | FONT_TYPE_MISMATCH -> "the font type is not appropriate for the operation"
  | USER_FONT_IMMUTABLE -> "the user-font is immutable"
  | USER_FONT_ERROR -> "error occurred in a user-font callback function"
  | NEGATIVE_COUNT -> "negative number used where it is not allowed"
  | INVALID_CLUSTERS -> "input clusters do not represent the accompanying text and glyph arrays"
  | INVALID_SLANT -> "invalid value for an input cairo_font_slant_t"
  | INVALID_WEIGHT -> "invalid value for an input cairo_font_weight_t"
  | INVALID_SIZE -> "invalid value (typically too big) for the size of the input (surface, pattern, etc.)"
  | USER_FONT_NOT_IMPLEMENTED -> "user-font method not implemented"
  | DEVICE_TYPE_MISMATCH -> "the device type is not appropriate for the operation"
  | DEVICE_ERROR -> "an operation to the device caused an unspecified error"
  | INVALID_MESH_CONSTRUCTION -> "invalid operation during mesh pattern construction"
  | DEVICE_FINISHED -> "the target device has been finished"
  | JBIG2_GLOBAL_MISSING -> "CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID used but no CAIRO_MIME_TYPE_JBIG2_GLOBAL data provided"

let status_repr = function
  | INVALID_RESTORE -> "INVALID_RESTORE"
  | NO_CURRENT_POINT -> "NO_CURRENT_POINT"
  (*BISECT-IGNORE-BEGIN*) (* I don't know how to make Cairo raise these exceptions *)
  | INVALID_POP_GROUP -> "INVALID_POP_GROUP"
  | INVALID_MATRIX -> "INVALID_MATRIX"
  | INVALID_STATUS -> "INVALID_STATUS"
  | NULL_POINTER -> "NULL_POINTER"
  | INVALID_STRING -> "INVALID_STRING"
  | INVALID_PATH_DATA -> "INVALID_PATH_DATA"
  | READ_ERROR -> "READ_ERROR"
  | WRITE_ERROR -> "WRITE_ERROR"
  | SURFACE_FINISHED -> "SURFACE_FINISHED"
  | SURFACE_TYPE_MISMATCH -> "SURFACE_TYPE_MISMATCH"
  | PATTERN_TYPE_MISMATCH -> "PATTERN_TYPE_MISMATCH"
  | INVALID_CONTENT -> "INVALID_CONTENT"
  | INVALID_FORMAT -> "INVALID_FORMAT"
  | INVALID_VISUAL -> "INVALID_VISUAL"
  | FILE_NOT_FOUND -> "FILE_NOT_FOUND"
  | INVALID_DASH -> "INVALID_DASH"
  | INVALID_DSC_COMMENT -> "INVALID_DSC_COMMENT"
  | INVALID_INDEX -> "INVALID_INDEX"
  | CLIP_NOT_REPRESENTABLE -> "CLIP_NOT_REPRESENTABLE"
  | TEMP_FILE_ERROR -> "TEMP_FILE_ERROR"
  | INVALID_STRIDE -> "INVALID_STRIDE"
  | FONT_TYPE_MISMATCH -> "FONT_TYPE_MISMATCH"
  | USER_FONT_IMMUTABLE -> "USER_FONT_IMMUTABLE"
  | USER_FONT_ERROR -> "USER_FONT_ERROR"
  | NEGATIVE_COUNT -> "NEGATIVE_COUNT"
  | INVALID_CLUSTERS -> "INVALID_CLUSTERS"
  | INVALID_SLANT -> "INVALID_SLANT"
  | INVALID_WEIGHT -> "INVALID_WEIGHT"
  | INVALID_SIZE -> "INVALID_SIZE"
  | USER_FONT_NOT_IMPLEMENTED -> "USER_FONT_NOT_IMPLEMENTED"
  | DEVICE_TYPE_MISMATCH -> "DEVICE_TYPE_MISMATCH"
  | DEVICE_ERROR -> "DEVICE_ERROR"
  | INVALID_MESH_CONSTRUCTION -> "INVALID_MESH_CONSTRUCTION"
  | DEVICE_FINISHED -> "DEVICE_FINISHED"
  | JBIG2_GLOBAL_MISSING -> "JBIG2_GLOBAL_MISSING"
  (*BISECT-IGNORE-END*)

exception Unavailable

type matrix = {
  mutable xx: float;
  mutable yx: float;
  mutable xy: float;
  mutable yy: float;
  mutable x0: float;
  mutable y0: float;
}

module Matrix = struct
  type t = matrix

  (*
    2 by 2 matrix with (x0, y0) offset is equivalent to the following 3 by 3 matrix:
    / xx xy x0 \   / x \   / xx*x + xy*y + x0 \
    | yx yy y0 | * | y | = | yx*x + yy*y + y0 |
    \  0  0  1 /   \ 1 /   \                1 /
  *)

  let init_identity () =
    {xx=1.; xy=0.; yx=0.; yy=1.; x0=0.; y0=0.}

  let init_translate x y =
    {xx=1.; xy=0.; yx=0.; yy=1.; x0=x; y0=y}

  let init_scale x y =
    {xx=x; xy=0.; yx=0.; yy=y; x0=0.; y0=0.}

  let init_rotate angle =
    {
      xx = cos angle;
      xy = -.sin angle;
      yx = sin angle;
      yy = cos angle;
      x0 = 0.;
      y0 = 0.;
    }

  let init_inverse {xx; xy; yx; yy; x0; y0} =
    let d = xx *. yy -. xy *. yx in
    if d = 0. then raise (Error INVALID_MATRIX);
    let xx = yy /. d
    and xy = -. xy /. d
    and yx = -. yx /. d
    and yy = xx /. d
    and x0 = (xy *. y0 -. yy *. x0) /. d
    and y0 = (yx *. x0 -. xx *. y0) /. d in
    {xx; xy; yx; yy; x0; y0}

  let multiply {xx; xy; yx; yy; x0; y0} {xx=xx'; xy=xy'; yx=yx'; yy=yy'; x0=x0'; y0=y0'} =
    let xx = xx *. xx' +. xy *. yx'
    and xy = xx *. xy' +. xy *. yy'
    and yx = yx *. xx' +. yy *. yx'
    and yy = yx *. xy' +. yy *. yy'
    and x0 = xx *. x0' +. xy *. y0' +. x0
    and y0 = yx *. x0' +. yy *. y0' +. y0 in
    {xx; xy; yx; yy; x0; y0}

  let transform_point {xx; xy; yx; yy; x0; y0} x y =
    (xx *. x +. xy *. y +. x0, yx *. x +. yy *. y +. y0)

  let transform_distance {xx; xy; yx; yy; x0=_; y0=_} ~dx ~dy =
    (xx *. dx +. xy *. dy, yx *. dx +. yy *. dy)

  let set m {xx; xy; yx; yy; x0; y0} =
    m.xx <- xx;
    m.xy <- xy;
    m.yx <- yx;
    m.yy <- yy;
    m.x0 <- x0;
    m.y0 <- y0

  let scale m x y =
    set m (multiply m (init_scale x y))

  let translate m x y =
    set m (multiply m (init_translate x y))

  let rotate m angle =
    set m (multiply m (init_rotate angle))

  let invert m =
    set m (init_inverse m)
end

type slant =
  | Upright
  | Italic
  | Oblique

type weight =
  | Normal
  | Bold

type font = {
  slant: slant;
  weight: weight;
  size: float;
  family: string;
}

module Pattern = struct
  (* We store tuples (instead of more explicit record types) because the public interface uses tuples
  (get_color_stop_rgba, get_rgba, get_linear_points, get_radial_circles) *)

  type stop_point = float * float * float * float * float (* (position, r, g, b, a) *)

  module StopPointList: sig
    type t
    val empty: t
    val add: t -> stop_point -> t
    val size: t -> int
    val get: t -> i:int -> stop_point
    val to_list: t -> stop_point list
  end = struct
    module Element = struct
      type t = {
        added: int;
        stop_point: stop_point;
      }

      let compare {stop_point=(position_a, _, _, _, _); added=added_a; _} {stop_point=(position_b, _, _, _, _); added=added_b; _} =
        match compare position_a position_b with
          | 0 -> compare added_a added_b
          | n -> n
    end

    type t = Element.t list

    let empty = []

    let size = List.length

    let add xs stop_point =
      let element = {Element.stop_point; added=List.length xs} in
      let rec aux = function
        | [] -> [element]
        | x::xs as xxs -> match Element.compare element x with
          | -1 -> element::xxs
          | _ -> x::(aux xs)
      in aux xs

    let to_list xs =
      List.map xs ~f:(fun {Element.stop_point; _} -> stop_point)

    let get xs ~i =
      let {Element.stop_point; _} = List.nth xs i in
      stop_point
  end

  (* This is just to cover StopPointList.to_list, which is used only in JsOfOCairo, whose test coverage isn't measured. *)
  let _ = StopPointList.(add empty (0., 0., 0., 0., 0.) |> to_list)

  (* When we drop support of OCaml 4.02, we can use [inline records](https://caml.inria.fr/pub/docs/manual-ocaml-4.06/extn.html#s%3Ainline-records) *)
  type linear_gradient = {
    points: float * float * float * float; (* (x0, y0, x1, y1) *)
    stop_points: StopPointList.t;
  }

  type radial_gradient = {
    circles: float * float * float * float * float * float; (* (x0, y0, r0, x1, y1, r1) *)
    stop_points: StopPointList.t;
  }

  type source =
    | Rgba of (float * float * float * float) (* (r, g, b, a) *)
    | LinearGradient of linear_gradient
    | RadialGradient of radial_gradient
    | TypeMismatch

  type 'a t = source ref constraint 'a = [<`Solid | `Surface | `Gradient | `Linear | `Radial]

  type any = [`Solid | `Surface | `Gradient | `Linear | `Radial] t

  let create_rgba r g b a =
    ref (Rgba (r, g, b, a))

  let create_rgb r g b =
    create_rgba r g b 1.

  let get_rgba pattern =
    match !pattern with
      | Rgba color -> color
      | LinearGradient _
      | RadialGradient _
      | TypeMismatch -> raise (Error PATTERN_TYPE_MISMATCH)

  let create_linear ~x0 ~y0 ~x1 ~y1 =
    ref (LinearGradient {points=(x0, y0, x1, y1); stop_points=StopPointList.empty})

  let get_linear_points pattern =
    match !pattern with
      | LinearGradient {points; _} -> points
      | Rgba _
      | RadialGradient _
      | TypeMismatch -> raise (Error PATTERN_TYPE_MISMATCH)

  let create_radial ~x0 ~y0 ~r0 ~x1 ~y1 ~r1 =
    ref (RadialGradient {circles=(x0, y0, r0, x1, y1, r1); stop_points=StopPointList.empty})

  let get_radial_circles pattern =
    match !pattern with
      | RadialGradient {circles; _} -> circles
      | LinearGradient _
      | Rgba _
      | TypeMismatch -> raise (Error PATTERN_TYPE_MISMATCH)

  let add_color_stop_rgba pattern ?(ofs=0.) r g b a =
    let stop_point = (ofs, r, g, b, a) in
    match !pattern with
      | LinearGradient gradient ->
        pattern := LinearGradient {gradient with stop_points=(StopPointList.add gradient.stop_points stop_point)}
      | RadialGradient gradient ->
        pattern := RadialGradient {gradient with stop_points=(StopPointList.add gradient.stop_points stop_point)}
      | Rgba _ -> pattern := TypeMismatch
      | TypeMismatch -> ()

  let add_color_stop_rgb pattern ?ofs r g b =
    add_color_stop_rgba pattern ?ofs r g b 1.

  let get_color_stop_count pattern =
    match !pattern with
      | LinearGradient {stop_points; _} | RadialGradient {stop_points; _} -> StopPointList.size stop_points
      | Rgba _
      | TypeMismatch -> raise (Error PATTERN_TYPE_MISMATCH)

  let get_color_stop_rgba pattern ~idx =
    match !pattern with
      | LinearGradient {stop_points; _} | RadialGradient {stop_points; _} -> StopPointList.get stop_points ~i:idx
      | Rgba _
      | TypeMismatch -> raise (Error PATTERN_TYPE_MISMATCH)
end

type fill_rule =
  | WINDING
  | EVEN_ODD

type line_cap =
  | BUTT
  | ROUND
  | SQUARE

type line_join =
  | JOIN_MITER
  | JOIN_ROUND
  | JOIN_BEVEL

type font_extents = {
  ascent: float;
  descent: float;
  baseline: float;
  max_x_advance: float;
  max_y_advance: float;
}

type text_extents = {
  x_bearing: float;
  y_bearing: float;
  width: float;
  height: float;
  x_advance: float;
  y_advance: float;
}

type operator =
  | CLEAR
  | SOURCE
  | OVER
  | IN
  | OUT
  | ATOP
  | DEST
  | DEST_OVER
  | DEST_IN
  | DEST_OUT
  | DEST_ATOP
  | XOR
  | ADD
  | SATURATE

module Points: sig
  type t

  val create: unit -> t

  val set_start: t -> transformation:Matrix.t -> x:float -> y:float -> unit
  val set_start_if_none: t -> transformation:Matrix.t -> x:float -> y:float -> unit
  val reset_start: t -> unit

  val set_current: t -> transformation:Matrix.t -> x:float -> y:float -> unit
  val set_current_from_start: t -> unit
  val reset_current: t -> unit
  val current: t -> transformation:Matrix.t -> (float * float) option
end = struct
  type t = {
    mutable start: (float * float) option;
    mutable current: (float * float) option;
  }

  let create () = {
    start = None;
    current = None;
  }

  let set_start points ~transformation ~x ~y =
    points.start <- Some (Matrix.transform_point transformation x y)

  let set_start_if_none points ~transformation ~x ~y =
    if points.start = None then
    points.start <- Some (Matrix.transform_point transformation x y)

  let reset_start points =
    points.start <- None

  let set_current points ~transformation ~x ~y =
    points.current <- Some (Matrix.transform_point transformation x y)

  let set_current_from_start points =
    points.current <- points.start

  let reset_current points =
    points.current <- None

  let current points ~transformation =
    match points.current with
      | None -> None
      | Some (x, y) -> Some (Matrix.transform_point (Matrix.init_inverse transformation) x y)
end
