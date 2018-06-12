open Common
open State

(**

   We rename all useful Wall modules.

*)
module WRend = Wall.Renderer
module WImg = Wall.Image
module WPath = Wall.Path
module WPaint = Wall.Paint
module WText = Wall_text
module WFont = Wall_text.Font
module WTransform = Wall.Transform
module WOutline = Wall.Outline
module WColor = Wall.Color

let deja_vu_sans_mono =
  lazy
    (let length =
       List.fold_left
         ~f:(fun c s -> c + String.length s + 1)
         ~init:0 Deja_vu_sans_mono.ttf
     in
     let buffer =
       Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout length
     in
     let index = ref 0 in
     List.iter Deja_vu_sans_mono.ttf ~f:(fun s ->
         String.iter s ~f:(fun c ->
             Bigarray.Array1.set buffer !index (int_of_char c) ;
             incr index ) ;
         Bigarray.Array1.set buffer !index (int_of_char '\n') ;
         incr index ) ;
     let offset = List.hd_exn (Stb_truetype.enum buffer) in
     match Stb_truetype.init buffer offset with
     | None -> assert false
     | Some font -> font)

module Mouse = struct
  type t = {x: float; y: float}

  let of_ints ix iy = {x= float ix; y= float iy}

  let to_string {x; y} = sprintf "(%.2f, %.2f)" x y

  let within_rectangle t ~x ~y ~w ~h =
    x <= t.x && t.x <= x +. w && y <= t.y && t.y <= y +. h
end

module Screen = struct
  type t = {w: float; h: float; margin: float}

  let of_ints ~margin iw ih = {w= float iw; h= float ih; margin}

  let v2 {w; h; _} = Gg.V2.v w h
end

let draw_text paint font ~x ~y text =
  WImg.paint paint (WText.simple_text font ~x ~y text)

let draw_debug_info ~(* ~task *) screen ~mouse ~text_size t ~y =
  let label = "Debug" in
  let font_size = 20. in
  let font = WFont.make ~size:font_size (Lazy.force deja_vu_sans_mono) in
  let text_width = WFont.text_width font label in
  let margin = 3. in
  let open Float in
  let w = text_width + (2. * margin) in
  let h = font_size + (2. * margin) in
  let x = screen.Screen.w - w - screen.Screen.margin in
  let expanded = Mouse.within_rectangle mouse ~x ~y ~w ~h in
  t.debug <- expanded ;
  let alpha = if expanded then 0.9 else 0.2 in
  let panel =
    if expanded then
      let info =
        [ sprintf "Mouse: %s" (Mouse.to_string mouse)
        ; sprintf "BPM: %d" t.bpm
        ; sprintf "Beat progress: %f" t.beat_progress
        ; sprintf "Beat: %s"
            (Option.value_map ~default:"-" t.beat ~f:(sprintf "%d"))
        ; sprintf "Bar progress: %f" t.bar_progress
        ; sprintf "Bar structure: [%s]"
            ( List.init t.bar_structure_length ~f:(fun i ->
                  if List.mem i t.bar_structure_strongs then "S" else "W" )
            |> String.concat ~sep:"|" )
        ; sprintf "Bar structure strongs: [%s]"
            ( List.map t.bar_structure_strongs ~f:(sprintf "%d")
            |> String.concat ~sep:", " )
        ; sprintf "Text size: %.4f -> %.2f" t.text_size text_size
        ; sprintf "Text lines: %d" (Array.length t.text_lines) ]
      in
      let max_width =
        List.fold info ~init:0. ~f:(fun p v -> max p (WFont.text_width font v))
      in
      let x =
        screen.Screen.w - max_width - screen.Screen.margin - (2. * margin)
      in
      let y = y + h in
      WImg.seq
        ( WImg.paint
            (WPaint.rgba 0.3 0. 0. 0.4)
            (WImg.fill_path (fun ctx ->
                 let w = max_width + (2. * margin) in
                 let h = ((List.length info |> float) *. font_size) + margin in
                 WPath.round_rect ctx ~x ~y ~w ~h ~r:6. ))
        :: List.mapi info ~f:(fun index s ->
               let x = x + margin in
               let y =
                 y + ((float index + 1.) * (font_size + (0. * margin)))
               in
               draw_text (WPaint.rgba 1. 1. 0. 0.5) font ~x ~y s ) )
    else WImg.empty
  in
  WImg.seq
    [ WImg.fill_path (fun ctx -> WPath.round_rect ctx ~x ~y ~w ~h ~r:6.)
      |> WImg.paint (WPaint.rgba 0.7 0. 0. alpha)
    ; draw_text
        (WPaint.rgba 0. 0. 1. alpha)
        font ~x:(x +. margin)
        ~y:Float.(y + h - (2. * margin))
        label
    ; panel ]

let display_text_lines (t: t) ~text_size ~x ~y =
  let open Float in
  let margin = 10. in
  let acc = ref [] in
  Array.iteri t.text_lines ~f:(fun index msg ->
      let y = y + text_size + (float index * text_size) in
      acc :=
        draw_text
          (WPaint.rgba 0.2 0.2 0.2 1.)
          (WFont.make (* ~line_height:10. *) ~size:text_size
             (Lazy.force deja_vu_sans_mono))
          ~x:(x + margin) ~y msg
        :: !acc ) ;
  WImg.seq !acc

(*
   Drawed in a more mathy [-1; 1] x [-1; 1] frame.

*)
let display_metronome ~display_tempo t ~x ~y ~w ~h =
  (* The 2D transform from the [-1, 1]² orthonomal to the screen
     rectangle (x,y,w,h) (which is upside down).  *)
  let transform =
    let open Float in
    let open WTransform in
    identity |> translate ~x:1. ~y:1.
    |> translate ~x:((x / 1.) + (w / 2.)) ~y:((y / 1.) + (h / 2.))
    |> rescale ~sx:1. ~sy:~-.1.
    |> rescale ~sx:(w / 2.) ~sy:(h / 2.)
  in
  (* `alpha` transforms [0, bar-length] floats into angles (rad): *)
  let alpha i =
    let open Float in
    let ii = float t.bar_structure_length - i in
    (pi / 2.) + (ii * 2. * pi / float t.bar_structure_length)
  in
  let absolute_text_size rel_size =
    abs_float WTransform.(py transform 0. rel_size -. py transform 0. 0.)
  in
  (* The distance between the corner-balls and the center of the area. *)
  let dist_from_center = 0.85 in
  WImg.seq
    [ (* debug display: *)
      ( if t.debug then
        WImg.paint
          (WPaint.rgba 0.9 0.9 0.9 0.3)
          (WImg.fill_path @@ fun t -> WPath.rect t ~x ~y ~w ~h)
      else WImg.empty )
    ; (* The BPM: *)
      ( if display_tempo then
        let rel_size = 0.2 in
        let size = absolute_text_size rel_size in
        let font = WFont.make ~size (Lazy.force deja_vu_sans_mono) in
        let label = sprintf "%d" t.bpm in
        let text_width = WFont.text_width font label in
        let x, y =
          let ax, ay = (0., 0.40) in
          ( WTransform.px transform ax ay -. (text_width /. 2.)
          , WTransform.py transform ax ay )
        in
        draw_text (WPaint.rgba 0.2 0.2 0.2 0.9) font ~x ~y label
      else WImg.empty )
    ; (* This is the skeleton of the metronome; the continuous polygon below the balls *)
      WImg.transform transform
        (WImg.paint
           (Wall.Paint.rgba 0.3 0.3 0.3 1.)
           ( WImg.stroke_path Wall.Outline.{default with stroke_width= 0.02}
           @@ fun ctx ->
           let open WPath in
           move_to ctx 0. dist_from_center ;
           for i = 1 to t.bar_structure_length do
             line_to ctx
               (dist_from_center *. cos (alpha (float i)))
               (dist_from_center *. sin (alpha (float i)))
           done ))
      (*
      WImg.paint
        (WPaint.rgba 0.3 0.3 0.3 0.5)
        (WImg.stroke_path
           WOutline.{default with stroke_width= 2.5}
           (fun ctx ->
             let open WPath in
             let zx, zy =
               ( WTransform.px transform 0. dist_from_center
               , WTransform.py transform 0. dist_from_center )
             in
             move_to ctx zx zy ;
             for i = 1 to t.bar_structure_length do
               let ux, uy =
                 ( dist_from_center *. cos (alpha (float i))
                 , dist_from_center *. sin (alpha (float i)) )
               in
               let x, y =
                 (WTransform.px transform ux uy, WTransform.py transform ux uy)
               in
               line_to ctx ~x ~y
             done ))
       *)
    ; (* Here we draw the static bar balls: *)
      WImg.seq
      @@ List.init t.bar_structure_length (fun i ->
             let is_strong = List.mem i ~set:t.bar_structure_strongs in
             let fi = float i in
             let cx, cy =
               let open Float in
               ( dist_from_center * cos (alpha fi)
               , dist_from_center * sin (alpha fi) )
             in
             let rx = if is_strong then 0.1 else 0.05 in
             let ball =
               WImg.transform transform
               @@ WImg.paint
                    (* Wall_canvas.draw' task transform *)
                    (let sx, sy, ex, ey =
                       Float.(cx - rx, cy - rx, cx + rx, cy + rx)
                     in
                     WPaint.linear_gradient ~sx ~sy ~ex ~ey
                       ~inner:(WColor.gray 0.7) ~outer:(WColor.gray 0.1))
                    ( WImg.fill_path
                    @@ fun ctx -> WPath.ellipse ctx ~cx ~cy ~rx ~ry:rx )
             in
             (* The numbers on the balls: *)
             let size = absolute_text_size (1.4 *. rx) in
             let font = WFont.make ~size (Lazy.force deja_vu_sans_mono) in
             let label = sprintf "%d" (i + 1) in
             let {WFont.width; height; _} = WFont.text_measure font label in
             let x, y =
               ( WTransform.px transform cx cy -. (width /. 2.)
               , WTransform.py transform cx cy +. (height /. 2.) )
             in
             (* WTransform.py transform cx cy +. (size *. 0.3)) in *)
             WImg.seq
               [ball; draw_text (WPaint.rgba 0.8 0.8 0.9 0.9) font ~x ~y label]
         )
    ; (* The current beat number in the Center: *)
      (let intra_beat = truncate t.bar_progress + 1 in
       let beat, beats_intra_beat =
         let b = ref 0 in
         let bib = ref 0 in
         for i = 0 to intra_beat - 1 do
           if List.mem ~set:t.bar_structure_strongs i then (
             incr b ;
             bib := i )
         done ;
         (!b, !bib)
       in
       let alpha = 1. in
       (* Float.(1. - (t.bar_progress - floor t.bar_progress) ** 1.4) in *)
       let rel_size = 0.5 in
       let cx, cy = (0., 0.) in
       let size = absolute_text_size rel_size in
       let font = WFont.make ~size (Lazy.force deja_vu_sans_mono) in
       let label =
         sprintf "%s%d·%d"
           ( if List.length t.bar_structure_strongs >= 10 && beat < 10 then "0"
           else "" )
           beat
           (intra_beat - beats_intra_beat)
       in
       (* Option.value_map ~default:(sprintf "%d" beat) t.beat ~f:(sprintf "%d") in *)
       let {WFont.width; height; _} = WFont.text_measure font label in
       let x, y =
         let open Float in
         ( WTransform.px transform cx cy - (width / 2.)
         , WTransform.py transform cx cy + (height / 2.) )
       in
       WImg.seq
         [ ( if t.debug then
             (* Debug square: *)
             WImg.paint
               (WPaint.rgba 0. 0. 0. alpha)
               ( WImg.stroke_path WOutline.{default with stroke_width= 1.03}
               @@ fun ctx ->
               WPath.rect ctx ~x ~y:(y -. height) ~w:width ~h:height )
           else WImg.empty )
         ; draw_text (WPaint.rgba 0. 0. 0.1 alpha) font ~x ~y label ])
    ; ((* The Bouncing Ball:  *)
       let bouncing_ball ?(main= false) progress =
         let transform =
           (* To simplify the trigonometry we apply the
                           `dist_from_center` to the transform: *)
           WTransform.rescale ~sx:dist_from_center ~sy:dist_from_center
             transform
         in
         let cx, cy =
           (* The small dot along the skeleton: *)
           let open Float in
           let i = truncate progress in
           let prog = progress - floor progress in
           let fi = float i in
           let si = fi + 1. in
           ( cos (alpha fi) + ((cos (alpha si) - cos (alpha fi)) * prog)
           , sin (alpha fi) + ((sin (alpha si) - sin (alpha fi)) * prog) )
         in
         let main_ball =
           if main then
             WImg.paint (WPaint.rgba 0. 0. 0. 1.)
             @@ WImg.transform transform @@ WImg.fill_path
             @@ fun ctx -> WPath.ellipse ctx ~cx ~cy ~rx:0.03 ~ry:0.03
           else WImg.empty
         in
         let ocx, ocy =
           (* Coordinates on the circle: *)
           Float.(cos (alpha progress), sin (alpha progress))
         in
         let debug_dot =
           if t.debug then
             (* The small dot on the circle: *)
             WImg.paint (WPaint.rgba 0. 0. 0. 1.)
             @@ WImg.transform transform
                  ( WImg.fill_path
                  @@ fun ctx ->
                  WPath.ellipse ctx ~cx:ocx ~cy:ocy ~rx:0.06 ~ry:0.06 )
           else WImg.empty
         in
         (* The really-bouncing ball: *)
         let bounce_factor =
           match t.bar_structure_length with
           | 1 | 2 -> 0.1
           | 3 -> 0.5
           | more ->
               let open Float in
               0.5
               + (float t.bar_structure_length / 20.)
               + (1. - (float t.bpm / 300.))
         in
         let icx, icy =
           let open Float in
           ( cx + (bounce_factor * (cx - ocx))
           , cy + (bounce_factor * (cy - ocy)) )
         in
         let rx = if main then 0.08 else 0.06 in
         let paint =
           if main then WPaint.rgba 0.3 0. 0. 0.9
           else WPaint.rgba 0.4 0.4 0.4 0.4
         in
         WImg.seq
           [ main_ball
           ; debug_dot
           ; WImg.paint paint
               (WImg.transform transform
                  ( WImg.fill_path
                  @@ fun ctx -> WPath.ellipse ctx ~cx:icx ~cy:icy ~rx ~ry:rx ))
           ]
       in
       let advance = 0.1 in
       let tail = 10 in
       let stickiness = 0.1 in
       let constrained =
         if abs_float (t.bar_progress -. floor t.bar_progress) < stickiness
         then floor t.bar_progress
         else t.bar_progress
       in
       (* let constrained = floor t.bar_progress in *)
       List.init tail ~f:(fun i ->
           constrained +. advance -. (0.04 *. float (tail - 1 - i)) )
       |> List.mapi ~f:(fun ith p -> bouncing_ball ~main:(ith = tail - 1) p)
       |> WImg.seq) ]

let render ~display_tempo ~w ~h ~mouse_x ~mouse_y ~metronome_x_factor context
    (t: t) =
  let screen = Screen.of_ints ~margin:10. w h in
  let mouse = Mouse.of_ints mouse_x mouse_y in
  let text_size =
    Float.(max 10. (t.text_size * screen.Screen.h) |> min 300.)
  in
  let side = screen.Screen.w *. metronome_x_factor in
  let wim =
    WImg.seq
      [ WImg.paint (WPaint.rgba 0. 0. 0. 0.3)
          (WImg.stroke_path
             WOutline.{default with stroke_width= 5.}
             (fun ctx ->
               WPath.round_rect ctx ~x:10. ~y:10.
                 ~w:(float w -. 20.)
                 ~h:(float h -. 20.)
                 ~r:10. ))
      ; draw_debug_info (* ~task:vg_task *) ~text_size ~screen ~mouse t ~y:10.
      ; display_text_lines t ~text_size ~x:10. ~y:30.
      ; display_metronome ~display_tempo t ~x:(screen.Screen.w -. side)
          ~y:(screen.Screen.h -. side) ~w:side ~h:side
      ; WImg.empty ]
  in
  WRend.render context ~width:screen.Screen.w ~height:screen.Screen.h wim
