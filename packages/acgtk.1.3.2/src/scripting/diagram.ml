open Cairo

(* Type definitions *)

type draw_op = Stroke | Fill

type toy_font = [ `Toy ] Font_face.t

type text_opts = { contents : string;
                   face : toy_font;
                   size : float; }

type diagram = Blend of diagram * diagram
             | Setup of (context -> unit) * diagram
             | Reframe of rectangle * diagram
             | Draw of Path.t * draw_op
             | Text of text_opts

type vector = float * float

type point = vector

type color = float * float * float * float


(* Utilities for the sake of implementation *)

let pi = 4. *. atan 1.

let (>>) (f : 'a -> 'b) (g : 'b -> 'c) : 'a -> 'c =
  fun x -> g (f x)

(** A generalization of List.fold_left from monoids to semigroups, where we
    don't have any neutral element. The supplied default is returned in
    the case of an empty input list. *)
let fold_def (f : 'a -> 'a -> 'a) (def : 'a) (xs : 'a list) : 'a =
  match xs with
  | [] -> def
  | head :: tail -> List.fold_left f head tail

let protect (prepare : unit -> unit) (cleanup : unit -> unit)
            (task : 'a -> 'b) (arg : 'a) : 'b =
  prepare ();
  try let result = task arg in
    cleanup ();
    result
  with e -> cleanup ();
            raise e

let with_cr (cr : context) (task : context -> 'a) : 'a =
  protect (fun () -> save cr) (fun () -> restore cr) task cr

let phony_cr : context =
  create (Image.create Image.ARGB32 1 1)

(* The following commented code would use a mutex to ensure that only
   one thread is using the phony_cr context at a time. However, that would
   only be relevant (and possible) when compiling with threads. *)
(*
let phony_cr_lock : Mutex.t =
  Mutex.create ()

let with_phony_cr (task : context -> 'a) : 'a =
  protect (fun () -> Mutex.lock phony_cr_lock; save cr)
          (fun () -> restore cr; Mutex.unlock phony_cr_lock)
          task phony_cr
*)

let with_phony_cr (task : context -> 'a) : 'a =
  with_cr phony_cr task


(* Paths *)

let set_path (cr : context) (p : Path.t) : unit =
  Path.clear cr;
  Path.append cr p

let empty_path : Path.t =
  Path.of_array [| |]

let trace_path (instructions : context -> unit) : Path.t =
  with_phony_cr (fun cr ->
    Path.clear cr;
    instructions cr;
    Path.copy cr)

let path_of_line ((x1, y1) : point) ((x2, y2) : point) : Path.t =
  trace_path (fun cr ->
    move_to cr x1 y1;
    line_to cr x2 y2)

let path_of_rectangle (rect : rectangle) : Path.t =
  trace_path (fun cr ->
    rectangle cr rect.x rect.y rect.w rect.h)

let path_of_circle ((xc, yc) : point) (radius : float) : Path.t =
  trace_path (fun cr ->
    arc cr xc yc radius 0. (2. *. pi))

let path_of_text (opts : text_opts) : Path.t =
  trace_path (fun cr ->
    Font_face.set cr opts.face;
    set_font_size cr opts.size;
    move_to cr 0. 0.;
    Path.text cr opts.contents)


(* Rectangle business *)

let rect_at_orig (width : float) (height : float) : rectangle =
  { x = 0.;
    y = 0.;
    w = width;
    h = height;
  }

let join_rect (r1 : rectangle) (r2 : rectangle) =
  let x_l = min r1.x r2.x in
  let y_l = min r1.y r2.y in
  let x_h = max (r1.x +. r1.w) (r2.x +. r2.w) in
  let y_h = max (r1.y +. r1.h) (r2.y +. r2.h) in
  { x = x_l;
    y = y_l;
    w = x_h -. x_l;
    h = y_h -. y_l;
  }

let extend_rect_to_point (r : rectangle) ((x, y) : point) : rectangle =
  let x_min = min x r.x in
  let x_max = max x (r.x +. r.w) in
  let y_min = min y r.y in
  let y_max = max y (r.y +. r.h) in
  { x = x_min;
    y = y_min;
    w = x_max -. x_min;
    h = y_max -. y_min;
  }

let envelope_of_points (ps : point list) : rectangle =
  match ps with
  | [] -> { x = 0.; y = 0.; w = 0.; h = 0.; }
  | (x, y) :: ps -> List.fold_left extend_rect_to_point
                                   { x = x; y = y; w = 0.; h = 0.; }
                                   ps

let transform_rectangle (m : Matrix.t) (r : rectangle) : rectangle =
  let r_ps = [(r.x, r.y);
              (r.x +. r.w, r.y);
              (r.x, r.y +. r.h);
              (r.x +. r.w, r.y +. r.h)] in
  let r_new_ps = List.map (fun (x, y) -> Matrix.transform_point m x y) r_ps in
  envelope_of_points r_new_ps


(* Kernel *)

let rec draw (cr : context) (d : diagram) : unit =
  match d with
  | Blend (d1, d2) -> draw cr d1;
                      draw cr d2
  | Setup (setup_fn, d_) -> with_cr cr (fun cr ->
                              setup_fn cr;
                              draw cr d_)
  | Reframe (exts, d_) -> draw cr d_
  | Draw (p, op) -> with_cr cr (fun cr ->
                      set_path cr p;
                      match op with
                      | Stroke -> stroke cr
                      | Fill -> fill cr)
  | Text opts -> with_cr cr (fun cr ->
                   Font_face.set cr opts.face;
                   set_font_size cr opts.size;
                   move_to cr 0. 0.;
                   show_text cr opts.contents)

let rec extents (d : diagram) : rectangle =
  match d with
  | Blend (d1, d2) -> join_rect (extents d1) (extents d2)
  | Setup (setup_fn, d_) -> with_phony_cr (fun cr ->
                              setup_fn cr;
                              extents d_)
  | Reframe (exts, d_) -> with_phony_cr (fun cr ->
                            transform_rectangle (get_matrix cr) exts)
  | Draw (p, op) -> with_phony_cr (fun cr ->
                      set_path cr p;
                      let user_space_rect = match op with
                      | Stroke -> stroke_extents cr
                      | Fill -> fill_extents cr in
                      transform_rectangle (get_matrix cr) user_space_rect)
  | Text opts -> with_phony_cr (fun cr ->
                   Font_face.set cr opts.face;
                   set_font_size cr opts.size;
                   let te = text_extents cr opts.contents in
                   let fe = font_extents cr in
                   let user_space_rect = { x = 0.;
                                           y = fe.descent -. fe.baseline;
                                           w = te.x_advance;
                                           h = fe.baseline; } in
                   transform_rectangle (get_matrix cr) user_space_rect)

let rec tighten_text (d : diagram) : diagram =
  match d with
  | Blend (d1, d2) -> Blend (tighten_text d1, tighten_text d2)
  | Setup (setup_fn, d_) -> Setup (setup_fn, tighten_text d_)
  | Reframe (exts, d_) -> Reframe (exts, tighten_text d_)
  | Draw (p, op) -> Draw (p, op)
  | Text opts -> Draw (path_of_text opts, Fill)

let stroke (p : Path.t) : diagram =
  Draw (p, Stroke)

let fill (p : Path.t) : diagram =
  Draw (p, Fill)

let empty_diagram : diagram =
  stroke empty_path

let blend (ds : diagram list) : diagram =
  fold_def (fun d1 d2 -> Blend (d1, d2)) empty_diagram ds

let setup (setup_fn : context -> unit) (d : diagram) : diagram =
  Setup (setup_fn, d)

let reframe (reframe_fn : rectangle -> rectangle) (d : diagram) : diagram =
  Reframe (reframe_fn (extents d), d)


(* Transforming diagrams *)

let transform (m : Matrix.t) : diagram -> diagram =
  setup (fun cr -> Cairo.transform cr m)

let translate ((dx, dy) : vector) : diagram -> diagram =
  transform (Matrix.init_translate dx dy)

let translateX (dx : float) : diagram -> diagram =
  translate (dx, 0.)

let translateY (dy : float) : diagram -> diagram =
  translate (0., dy)

let scale ((sx, sy) : vector) : diagram -> diagram =
  transform (Matrix.init_scale sx sy)

let scaleX (sx : float) : diagram -> diagram =
  scale (sx, 1.)

let scaleY (sy : float) : diagram -> diagram =
  scale (1., sy)

let uscale (s : float) : diagram -> diagram =
  scale (s, s)

let rotate (angle : float) : diagram -> diagram =
  transform (Matrix.init_rotate (angle *. 2. *. pi))


(* Aligning diagrams *)

let centerX (d : diagram) : diagram =
  let b = extents d in
  let cX = b.x +. b.w /. 2. in
  translateX (-. cX) d

let centerY (d : diagram) : diagram =
  let b = extents d in
  let cY = b.y +. b.h /. 2. in
  translateY (-. cY) d

let center : diagram -> diagram =
  centerX >> centerY

let alignL (d : diagram) : diagram =
  let e = extents d in
  let l = e.x in
  translateX (-. l) d

let alignR (d : diagram) : diagram =
  let e = extents d in
  let r = e.x +. e.w in
  translateX (-. r) d

let alignT (d : diagram) : diagram =
  let e = extents d in
  let t = e.y in
  translateY (-. t) d

let alignB (d : diagram) : diagram =
  let e = extents d in
  let b = e.y +. e.h in
  translateY (-. b) d


(* Diagram combinators *)

let (|||) (d1 : diagram) (d2 : diagram) : diagram =
  let e1 = extents d1 in
  let e2 = extents d2 in
  let r1 = e1.x +. e1.w in
  let l2 = e2.x in
  blend [ d1; translateX (r1 -. l2) d2 ]
 
let (===) (d1 : diagram) (d2 : diagram) : diagram =
  let e1 = extents d1 in
  let e2 = extents d2 in
  let b1 = e1.y +. e1.h in
  let t2 = e2.y in
  blend [ d1; translateY (b1 -. t2) d2 ]
 
let hcat (ds : diagram list) : diagram =
  fold_def (|||) empty_diagram ds

let vcat (ds : diagram list) : diagram =
  fold_def (===) empty_diagram ds


(* Text support *)

let font ?(slant : slant = Upright) ?(weight : weight = Normal)
          (family : string) : toy_font =
  Font_face.create ~family slant weight

let default_font : toy_font =
  font ""

let text ?(face : toy_font = default_font) ?(size : float = 10.)
          (contents : string) : diagram =
  fill (path_of_text { contents = contents;
                       face = face;
                       size = size; })
  |> center

let text_ ?(face : toy_font = default_font) ?(size : float = 10.)
           (contents : string) : diagram =
  Text { contents = contents;
         face = face;
         size = size; }

let get_font_extents (face : toy_font) (size : float) : font_extents =
  with_phony_cr (fun cr ->
    Font_face.set cr face;
    set_font_size cr size;
    font_extents cr)

(* Diagrams of basic shapes *)

let line (p1 : point) (p2 : point) : diagram =
  stroke (path_of_line p1 p2)

let rectangle_outline (width : float) (height : float) : diagram =
  stroke (path_of_rectangle (rect_at_orig width height))
  |> center

let rectangle_full (width : float) (height : float) : diagram =
  fill (path_of_rectangle (rect_at_orig width height))
  |> center

let circle_outline (radius : float) : diagram =
  stroke (path_of_circle (0., 0.) radius)

let circle_full (radius : float) : diagram =
  fill (path_of_circle (0., 0.) radius)


(* Coloring diagrams *)

let color ((r, g, b, a) : color) (d : diagram) : diagram =
  setup (fun cr -> set_source_rgba cr r g b a) d

let bg_color (c : color) (d : diagram) : diagram =
  let exts = extents d in
  blend [ rectangle_full exts.w exts.h
            |> translate (exts.x +. exts.w /. 2., exts.y +. exts.h /. 2.)
            |> color c;
          d ]

let empty_color : color =
  (0., 0., 0., 0.)

let black : color =
  (0., 0., 0., 1.)

let blue : color =
  (0., 0., 1., 1.)

let green : color =
  (0., 1., 0., 1.)

let lightgrey : color =
  (0.83, 0.83, 0.83, 1.)

let red : color =
  (1., 0., 0., 1.)


(* Whitespace diagrams *)

let setup_bounds_for_invisible_lines (d : diagram) : diagram =
  Setup ((fun cr -> set_dash cr [| |];
                    set_line_cap cr BUTT;
                    set_line_join cr JOIN_ROUND;
                    set_line_width cr 0.1),
         d)

let hspace (size : float) : diagram =
  line (0., 0.) (size, 0.)
  |> setup_bounds_for_invisible_lines
  |> color empty_color

let vspace (size : float) : diagram =
  line (0., 0.) (0., size)
  |> setup_bounds_for_invisible_lines
  |> color empty_color

let pad_abs ?(all : float = 0.)
            ?(horizontal : float = 0.) ?(vertical : float = 0.)
            ?(left : float = 0.) ?(right : float = 0.)
            ?(top : float = 0.) ?(bottom : float = 0.)
             (d : diagram) : diagram =
  let b = extents d in
  let envelope = { x = b.x -. all -. horizontal -. left;
                   y = b.y -. all -. vertical -. top;
                   w = b.w +. 2. *. all +. 2. *. horizontal +. left +. right;
                   h = b.h +. 2. *. all +. 2. *. vertical +. top +. bottom;
                 } in
  let wrapper = fill (path_of_rectangle envelope)
                |> color empty_color in
  blend [d; wrapper]

let pad_rel ?(all : float = 0.)
            ?(horizontal : float = 0.) ?(vertical : float = 0.)
            ?(left : float = 0.) ?(right : float = 0.)
            ?(top : float = 0.) ?(bottom : float = 0.)
             (d : diagram) : diagram =
  let e = extents d in
  pad_abs ~horizontal:((horizontal +. all) *. e.w)
          ~vertical:((vertical +. all) *. e.h)
          ~left:(left *. e.w) ~right:(right *. e.w)
          ~top:(top *. e.h) ~bottom:(bottom *. e.h)
          d
  

(* Convenience functionality *)

let to_svg (filename : string) (d : diagram) : unit =
  let exts = extents d in
  let surface = SVG.create filename exts.w exts.h in
  let cr = create surface in
  let d_repositioned = translate (-. exts.x, -. exts.y) d in
  draw cr d_repositioned;
  Surface.finish surface

let frame (d : diagram) : diagram =
  let exts = extents d in
  blend [ d;
          rectangle_outline exts.w exts.h
            |> translate (exts.x +. exts.w /. 2., exts.y +. exts.h /. 2.)
        ]
  
let show_origin (d : diagram) : diagram =
  blend [ d;
          circle_full 1.5
            |> color red;
          circle_outline 1.5
            |> color black
            |> setup (fun cr -> set_line_width cr 0.3); ]

let show_extents (d : diagram) : diagram =
  let exts = extents d in
  blend [ d;
          rectangle_outline exts.w exts.h
            |> translate (exts.x +. exts.w /. 2., exts.y +. exts.h /. 2.)
            |> color red
            |> setup (fun cr -> set_line_width cr 0.5) ]
    |> reframe (fun _ -> exts)
