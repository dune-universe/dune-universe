(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = sig
  #include "S.incl.mli"
end

module Mock = struct
  #include "Backend.incl.ml"

  let () = Printexc.register_printer (function
    | Error status -> Some (Printf.sprintf "CairoMock.Error(%s)" (status_repr status))
    | _ -> None
  )

  module State = struct
    type t = {
      dashes: float array;
      fill_rule: fill_rule;
      font: font;
      line_cap: line_cap;
      line_join: line_join;
      line_width: float;
      miter_limit: float;
      ofs: float;
      operator: operator;
      source: Pattern.source;
      transformation: Matrix.t;
    }
  end

  type context = {
    mutable states: State.t list;
    points: Points.t;
  }

  let create () = {
    states = [
      {
        dashes = [||];
        fill_rule = WINDING;
        font = {
          slant = Upright;
          weight = Normal;
          size = 10.;
          family = "sans-serif";
        };
        line_cap = BUTT;
        line_join = JOIN_MITER;
        line_width = 2.;
        miter_limit = 10.;
        ofs = 0.;
        operator = OVER;
        source = !(Pattern.create_rgb 0. 0. 0.);
        transformation = Matrix.init_identity ();
      };
    ];
    points = Points.create ();
  }

  let state {states; _} =
    List.hd states

  let mutate_state context f =
    let state = f (state context) in
    context.states <- state::(List.tl context.states)


  let save context =
    context.states <- (state context)::context.states

  let restore context =
    let states =
      match context.states with
        | [] (*BISECT-IGNORE*) (* This cannot happen: restore is the only function removing states and it refuses to remove the last one. *)
        | [_] -> raise (Error INVALID_RESTORE)
        | _::states -> states
    in
    context.states <- states


  let set_matrix context transformation =
    mutate_state context (fun s -> {s with transformation})

  let get_matrix context =
    (state context).transformation

  let transform_ m s =
    {s with State.transformation=Matrix.multiply s.State.transformation m}

  let transform context m =
    mutate_state context (transform_ m)

  let scale context x y =
    mutate_state context (transform_ (Matrix.init_scale x y))

  let translate context x y =
    mutate_state context (transform_ (Matrix.init_translate x y))

  let rotate context angle =
    mutate_state context (transform_ (Matrix.init_rotate angle))

  let identity_matrix context =
    mutate_state context (fun s -> {s with transformation=Matrix.init_identity ()})

  let device_to_user context x y =
    Matrix.transform_point (Matrix.init_inverse (state context).transformation) x y

  let device_to_user_distance context dx dy =
    Matrix.transform_distance (Matrix.init_inverse (state context).transformation) ~dx ~dy

  let user_to_device context x y =
    Matrix.transform_point (state context).transformation x y

  let user_to_device_distance context dx dy =
    Matrix.transform_distance (state context).transformation ~dx ~dy


  let mutate_points context ~start ~current =
    let transformation = (state context).transformation in
    let make_relative ~dx ~dy =
      let (x, y) =
        match Points.current context.points ~transformation with
          | None -> raise (Error NO_CURRENT_POINT)
          | Some (x, y) -> (x, y)
      in
      (x +. dx, y +. dy)
    in
    begin match start with
      | `None -> ()
      | `Reset -> Points.reset_start context.points
      | `IfNone (x, y) -> Points.set_start_if_none context.points ~transformation ~x ~y
      | `Set (x, y) -> Points.set_start context.points ~transformation ~x ~y
      | `Relative (dx, dy) ->
        let (x, y) = make_relative ~dx ~dy in
        Points.set_start context.points ~transformation ~x ~y
    end;
    begin match current with
      | `Reset -> Points.reset_current context.points
      | `FromStart -> Points.set_current_from_start context.points
      | `Set (x, y) -> Points.set_current context.points ~transformation ~x ~y
      | `Relative (dx, dy) ->
        let (x, y) = make_relative ~dx ~dy in
        Points.set_current context.points ~transformation ~x ~y
    end


  let move_to context x y =
    mutate_points context ~start:(`Set (x, y)) ~current:`FromStart

  let rel_move_to context x y =
    mutate_points context ~start:(`Relative (x, y)) ~current:`FromStart

  let line_to context x y =
    mutate_points context ~start:(`IfNone (x, y)) ~current:(`Set (x, y))

  let rel_line_to context x y =
    mutate_points context ~start:(`IfNone (x, y)) ~current:(`Relative (x, y))

  let curve_to context x1 y1 _ _ x3 y3 =
    mutate_points context ~start:(`IfNone (x1, y1)) ~current:(`Set (x3, y3))

  let rel_curve_to context x1 y1 _ _ x3 y3 =
    mutate_points context ~start:(`IfNone (x1, y1)) ~current:(`Relative (x3, y3))

  let rectangle context x y ~w:_ ~h:_ =
    mutate_points context ~start:`None ~current:(`Set (x, y))

  let arc context x y ~r ~a1 ~a2 =
    mutate_points context ~start:(`IfNone (x +. r *. (cos a1), y +. r *. (sin a1))) ~current:(`Set (x +. r *. (cos a2), y +. r *. (sin a2)))

  let arc_negative context x y ~r ~a1 ~a2 =
    mutate_points context ~start:(`IfNone (x +. r *. (cos a1), y +. r *. (sin a1))) ~current:(`Set (x +. r *. (cos a2), y +. r *. (sin a2)))

  module Path = struct
    let get_current_point context =
      let transformation = (state context).transformation in
      match Points.current ~transformation context.points with
        | None -> (0., 0.)
        | Some (x, y) -> (x, y)

    let clear context =
      mutate_points context ~start:`Reset ~current:`Reset

    let close context =
      mutate_points context ~start:`None ~current:`FromStart
  end

  let stroke_preserve _ =
    ()

  let stroke context =
    mutate_points context ~start:`Reset ~current:`Reset

  let fill_preserve _ =
    ()

  let fill context =
    mutate_points context ~start:`Reset ~current:`Reset

  let clip_preserve _ =
    ()

  let clip context =
    mutate_points context ~start:`Reset ~current:`Reset

  let paint ?alpha:_ _ =
    ()


  let set_line_width context line_width =
    mutate_state context (fun s -> {s with line_width})

  let get_line_width context =
    (state context).line_width

  let set_dash context ?(ofs=0.) dashes =
    mutate_state context (fun s -> {s with dashes; ofs})

  let get_dash context =
    let state = state context in
    (state.dashes, state.ofs)

  let set_fill_rule context fill_rule =
    mutate_state context (fun s -> {s with fill_rule})

  let get_fill_rule context =
    (state context).fill_rule

  let set_line_cap context line_cap =
    mutate_state context (fun s -> {s with line_cap})

  let get_line_cap context =
    (state context).line_cap

  let set_line_join context line_join =
    mutate_state context (fun s -> {s with line_join})

  let get_line_join context =
    (state context).line_join

  let set_miter_limit context miter_limit =
    mutate_state context (fun s -> {s with miter_limit})

  let get_miter_limit context =
    (state context).miter_limit

  let set_operator context operator =
    mutate_state context (fun s -> {s with operator})

  let get_operator context =
    (state context).operator


  let set_source context pattern =
    let source = !pattern in
    mutate_state context (fun s -> {s with source})

  let get_source context =
    ref (state context).source

  let set_source_rgb context r g b =
    let source = !(Pattern.create_rgb r g b) in
    mutate_state context (fun s -> {s with source})

  let set_source_rgba context r g b a =
    let source = !(Pattern.create_rgba r g b a) in
    mutate_state context (fun s -> {s with source})


  let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
    mutate_state context (fun s -> {s with font={s.font with slant; weight; family}})

  let set_font_size context size =
    mutate_state context (fun s -> {s with font={s.font with size}})

  let show_text context s =
    let (x, y) =
      Path.get_current_point context
    and width =
      (state context).font.size *. 0.8 *. (float_of_int (String.length s))
    in
    mutate_points context ~start:`None ~current:(`Set (x +. width, y))

  let font_extents context =
    let ascent = (state context).font.size in
    {
      ascent;
      descent=ascent /. 4.;
      baseline=0.;
      max_x_advance=2. *. ascent;
      max_y_advance=0.;
    }

  let text_extents context s =
    let width =
      (state context).font.size *. 0.8 *. (float_of_int (String.length s))
    and height =
      (state context).font.size
    in
    {x_bearing=0.; y_bearing=0.; width; height; x_advance=width; y_advance=0.}
end

module Decorate(C: S) = struct
  open StdLabels

  type fill_rule = C.fill_rule =
    | WINDING
    | EVEN_ODD

  type line_cap = C.line_cap =
    | BUTT
    | ROUND
    | SQUARE

  type line_join = C.line_join =
    | JOIN_MITER
    | JOIN_ROUND
    | JOIN_BEVEL

  type matrix = C.matrix= {
    mutable xx: float;
    mutable yx: float;
    mutable xy: float;
    mutable yy: float;
    mutable x0: float;
    mutable y0: float;
  }

  type text_extents = C.text_extents = {
    x_bearing : float;
    y_bearing : float;
    width : float;
    height : float;
    x_advance : float;
    y_advance : float;
  }

  type font_extents = C.font_extents = {
    ascent: float;
    descent: float;
    baseline: float;
    max_x_advance: float;
    max_y_advance: float;
  }

  type operator = C.operator =
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

  type weight = C.weight =
    | Normal
    | Bold

  type slant = C.slant =
    | Upright
    | Italic
    | Oblique

  type status = C.status =
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

  module Pattern = C.Pattern

  module Matrix = C.Matrix

  exception Error = C.Error

  exception Unavailable = C.Unavailable

  let status_to_string = C.status_to_string

  module P = struct
    open C

    let unit () =
      ""

    let float x =
      Printf.sprintf "%.2f" x

    let matrix {xx; xy; yx; yy; x0; y0} =
      Printf.sprintf "{xx=%.2f; xy=%.2f; yx=%.2f; yy=%.2f; x0=%.2f; y0=%.2f}" xx xy yx yy x0 y0

    let coords (x, y) =
      Printf.sprintf "(%.2f, %.2f)" x y

    let dashes dashes =
      dashes
      |> Array.to_list
      |> List.map ~f:(Printf.sprintf "%.2f")
      |> String.concat ~sep:"; "
      |> Printf.sprintf "[|%s|]"

    let dashes_ofs (ds, ofs) =
      Printf.sprintf "(%s, %.2f)" (dashes ds) ofs

    let fill_rule = function
      | WINDING -> "WINDING"
      | EVEN_ODD -> "EVEN_ODD"

    let line_cap = function
      | BUTT -> "BUTT"
      | ROUND -> "ROUND"
      | SQUARE -> "SQUARE"

    let line_join = function
      | JOIN_MITER -> "JOIN_MITER"
      | JOIN_ROUND -> "JOIN_ROUND"
      | JOIN_BEVEL -> "JOIN_BEVEL"

    let operator = function
      | CLEAR -> "CLEAR"
      | SOURCE -> "SOURCE"
      | OVER -> "OVER"
      | IN -> "IN"
      | OUT -> "OUT"
      | ATOP -> "ATOP"
      | DEST -> "DEST"
      | DEST_OVER -> "DEST_OVER"
      | DEST_IN -> "DEST_IN"
      | DEST_OUT -> "DEST_OUT"
      | DEST_ATOP -> "DEST_ATOP"
      | XOR -> "XOR"
      | ADD -> "ADD"
      | SATURATE -> "SATURATE"

    let source source = Pattern.(
      let get get =
        try Some (get source) with Error PATTERN_TYPE_MISMATCH -> None
      and stop_points () =
        let count = get_color_stop_count source in
        let stop_points = Array.make count (0., 0., 0., 0., 0.) in
        for idx = 0 to count - 1 do
          stop_points.(idx) <- get_color_stop_rgba source ~idx
        done;
        stop_points
        |> Array.to_list
        |> List.map ~f:(fun (position, r, g, b, a) -> Printf.sprintf "{position=%.2f; r=%.2f; g=%.2f; b=%.2f; a=%.2f}" position r g b a)
        |> String.concat ~sep:"; "
        |> Printf.sprintf "[%s]"
      in
      match get get_rgba with
        | Some (r, g, b, a) ->
          Printf.sprintf "(Rgba {r=%.2f; g=%.2f; b=%.2f; a=%.2f})" r g b a
        | None -> match get get_linear_points with
          | Some (x1, y1, x2, y2) ->
            Printf.sprintf "(LinearGradient {x0=%.2f; y0=%.2f; x1=%.2f; y1=%.2f; stop_points=%s})" x1 y1 x2 y2 (stop_points ())
          | None ->
            let (x1, y1, r1, x2, y2, r2) = get_radial_circles source in
            Printf.sprintf "(RadialGradient {x0=%.2f; y0=%.2f; r0=%.2f; x1=%.2f; y1=%.2f; r1%.2f; stop_points=%s})" x1 y1 r1 x2 y2 r2 (stop_points ())
    )

    let font_extents {ascent; descent; baseline; max_x_advance; max_y_advance} =
      Printf.sprintf "{ascent=%.2f; descent=%.2f; baseline=%.2f; max_x_advance=%.2f; max_y_advance=%.2f}" ascent descent baseline max_x_advance max_y_advance

    let text_extents {x_bearing; y_bearing; width; height; x_advance; y_advance} =
      Printf.sprintf "{x_bearing=%.2f; y_bearing=%.2f; width=%.2f; height=%.2f; x_advance=%.2f; y_advance=%.2f}" x_bearing y_bearing width height x_advance y_advance

    let slant = function
      | Upright -> "Upright"
      | Italic -> "Italic"
      | Oblique -> "Oblique"

    let weight = function
      | Normal -> "Normal"
      | Bold -> "Bold"
  end

  module A = struct
    let matrix () =
      P.matrix

    let operator () =
      P.operator

    let fill_rule () =
      P.fill_rule

    let line_cap () =
      P.line_cap

    let line_join () =
      P.line_join

    let source () =
      P.source

    let dashes () =
      P.dashes

    let option name p () = function
      | None -> ""
      | Some x -> Printf.sprintf " ~%s:%s" name (p x)
  end

  module Context: sig
    type t

    val create: C.context -> t
    val call: t -> ('a, unit, string, ('b -> string) -> (C.context -> 'b) -> 'b) format4 -> 'a
    val calls: t -> string list
  end = struct
    type t = {
      c: C.context;
      mutable calls: string list;
    }

    let create c =
      {c; calls=[]}

    let calls {calls; _} =
      List.rev calls

    let call context format =
      Printf.ksprintf (fun call print_ret f ->
        try begin
          let ret = f context.c in
          let printed_ret = print_ret ret in
          let call =
            if printed_ret <> "" then
              Printf.sprintf "%s -> %s" call printed_ret
            else
              call
          in
          context.calls <- call::context.calls;
          ret
        end with
          | ex -> begin
            let call = Printf.sprintf "%s -> raise (%s)" call (Printexc.to_string ex) in
            context.calls <- call::context.calls;
            raise ex
          end
      ) format
  end

  type context = Context.t

  let create = Context.create

  let calls = Context.calls

  #define CC Context.call context


  let save context =
    CC "save" P.unit C.save

  let restore context =
    CC "restore" P.unit C.restore


  let set_matrix context m =
    CC "set_matrix %a" A.matrix m P.unit (fun c -> C.set_matrix c m)

  let get_matrix context =
    CC "get_matrix" P.matrix C.get_matrix

  let transform context m =
    CC "transform %a" A.matrix m P.unit (fun c -> C.transform c m)

  let scale context x y =
    CC "scale %.2f %.2f" x y P.unit (fun c -> C.scale c x y)

  let translate context x y =
    CC "translate %.2f %.2f" x y P.unit (fun c -> C.translate c x y)

  let rotate context angle =
    CC "rotate %.2f" angle P.unit (fun c -> C.rotate c angle)

  let identity_matrix context =
    CC "identity_matrix" P.unit C.identity_matrix

  let device_to_user context x y =
    CC "device_to_user %.2f %.2f" x y P.coords (fun c -> C.device_to_user c x y)

  let device_to_user_distance context x y =
    CC "device_to_user_distance %.2f %.2f" x y P.coords (fun c -> C.device_to_user_distance c x y)

  let user_to_device context x y =
    CC "user_to_device %.2f %.2f" x y P.coords (fun c -> C.user_to_device c x y)

  let user_to_device_distance context x y =
    CC "user_to_device_distance %.2f %.2f" x y P.coords (fun c -> C.user_to_device_distance c x y)


  let move_to context x y =
    CC "move_to %.2f %.2f" x y P.unit (fun c -> C.move_to c x y)

  let rel_move_to context x y =
    CC "rel_move_to %.2f %.2f" x y P.unit (fun c -> C.rel_move_to c x y)

  let line_to context x y =
    CC "line_to %.2f %.2f" x y P.unit (fun c -> C.line_to c x y)

  let rel_line_to context x y =
    CC "rel_line_to %.2f %.2f" x y P.unit (fun c -> C.rel_line_to c x y)

  let curve_to context x1 y1 x2 y2 x3 y3 =
    CC "curve_to %.2f %.2f %.2f %.2f %.2f %.2f" x1 y1 x2 y2 x3 y3 P.unit (fun c -> C.curve_to c x1 y1 x2 y2 x3 y3)

  let rel_curve_to context x1 y1 x2 y2 x3 y3 =
    CC "rel_curve_to %.2f %.2f %.2f %.2f %.2f %.2f" x1 y1 x2 y2 x3 y3 P.unit (fun c -> C.rel_curve_to c x1 y1 x2 y2 x3 y3)

  let rectangle context x y ~w ~h =
    CC "rectangle %.2f %.2f ~w:%.2f ~h:%.2f" x y w h P.unit (fun c -> C.rectangle c x y ~w ~h)

  let arc context x y ~r ~a1 ~a2 =
    CC "arc %.2f %.2f ~r:%.2f ~a1:%.2f ~a2:%.2f" x y r a1 a2 P.unit (fun c -> C.arc c x y ~r ~a1 ~a2)

  let arc_negative context x y ~r ~a1 ~a2 =
    CC "arc_negative %.2f %.2f ~r:%.2f ~a1:%.2f ~a2:%.2f" x y r a1 a2 P.unit (fun c -> C.arc_negative c x y ~r ~a1 ~a2)

  module Path = struct
    let get_current_point context =
      CC "Path.get_current_point" P.coords C.Path.get_current_point

    let clear context =
      CC "Path.clear" P.unit C.Path.clear

    let close context =
      CC "Path.close" P.unit C.Path.close
  end

  let stroke_preserve context =
    CC "stroke_preserve" P.unit C.stroke_preserve

  let stroke context =
    CC "stroke" P.unit C.stroke

  let fill_preserve context =
    CC "fill_preserve" P.unit C.fill_preserve

  let fill context =
    CC "fill" P.unit C.fill

  let clip_preserve context =
    CC "clip_preserve" P.unit C.clip_preserve

  let clip context =
    CC "clip" P.unit C.clip

  let paint ?alpha context =
    CC "paint%a" (A.option "alpha" P.float) alpha P.unit (C.paint ?alpha)


  let set_line_width context line_width =
    CC "set_line_width %.2f" line_width P.unit (fun c -> C.set_line_width c line_width)

  let get_line_width context =
    CC "get_line_width" P.float C.get_line_width

  let set_dash context ?ofs dashes =
    CC "set_dash%a %a" (A.option "ofs" P.float) ofs A.dashes dashes P.unit (fun c -> C.set_dash c ?ofs dashes)

  let get_dash context =
    CC "get_dash" P.dashes_ofs C.get_dash

  let set_fill_rule context fill_rule =
    CC "set_fill_rule %a" A.fill_rule fill_rule P.unit (fun c -> C.set_fill_rule c fill_rule)

  let get_fill_rule context =
    CC "get_fill_rule" P.fill_rule C.get_fill_rule

  let set_line_cap context line_cap =
    CC "set_line_cap %a" A.line_cap line_cap P.unit (fun c -> C.set_line_cap c line_cap)

  let get_line_cap context =
    CC "get_line_cap" P.line_cap C.get_line_cap

  let set_line_join context line_join =
    CC "set_line_join %a" A.line_join line_join P.unit (fun c -> C.set_line_join c line_join)

  let get_line_join context =
    CC "get_line_join" P.line_join C.get_line_join

  let set_miter_limit context miter_limit =
    CC "set_miter_limit %.2f" miter_limit P.unit (fun c -> C.set_miter_limit c miter_limit)

  let get_miter_limit context =
    CC "get_miter_limit" P.float C.get_miter_limit

  let set_operator context operator =
    CC "set_operator %a" A.operator operator P.unit (fun c -> C.set_operator c operator)

  let get_operator context =
    CC "get_operator" P.operator C.get_operator


  let set_source context source =
    CC "set_source %a" A.source (Obj.magic source) P.unit (fun c -> C.set_source c source)

  let get_source context =
    CC "get_source" P.source C.get_source

  let set_source_rgb context r g b =
    CC "set_source_rgb %.2f %.2f %.2f" r g b P.unit (fun c -> C.set_source_rgb c r g b)

  let set_source_rgba context r g b a =
    CC "set_source_rgba %.2f %.2f %.2f %.2f" r g b a P.unit (fun c -> C.set_source_rgba c r g b a)


  let select_font_face context ?slant ?weight family =
    CC "select_font_face%a%a %S" (A.option "slant" P.slant) slant (A.option "weight" P.weight) weight family P.unit (fun c -> C.select_font_face c ?slant ?weight family)

  let set_font_size context font_size =
    CC "set_font_size %.2f" font_size P.unit (fun c -> C.set_font_size c font_size)

  let show_text context s =
    CC "show_text %S" s P.unit (fun c -> C.show_text c s)

  let font_extents context =
    CC "font_extents" P.font_extents C.font_extents

  let text_extents context s =
    CC "text_extents %S" s P.text_extents (fun c -> C.text_extents c s)
end

include Decorate(Mock)

let create () =
  Context.create (Mock.create ())
