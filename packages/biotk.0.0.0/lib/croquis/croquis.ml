open Gg
open Vg
open Core_kernel

type point = float * float

module Float_array = struct
  let min xs =
    Array.fold xs ~init:Float.max_value ~f:Float.min
  let max xs =
    Array.fold xs ~init:Float.min_value ~f:Float.max
end

module Scaling = struct
  type 'a t = 'a -> float

  let id x = x
  let linear ~domain:(from_lo, from_hi) ~range:(to_lo, to_hi) =
    let delta = to_hi -. to_lo in
    let rho = delta /. (from_hi -. from_lo) in
    fun x -> (x -. from_lo) *. rho +. to_lo
end

module Viewport = struct
  type t = {
    scale_x : float -> float ;
    scale_y : float -> float ;
  }

  let linear ~xlim ~ylim ~size:(w, h) =
    { scale_x = Scaling.linear ~domain:xlim ~range:(0., w) ;
      scale_y = Scaling.linear ~domain:ylim ~range:(0., h) }

  let make ?(scale_x = Scaling.id) ?(scale_y = Scaling.id) () =
    { scale_x ; scale_y }

  let id = {
    scale_x = ident ;
    scale_y = ident ;
  }
  let scale_x vp = vp.scale_x
  let scale_y vp = vp.scale_y

  let scale vp (x, y) =
    (vp.scale_x x, vp.scale_y y)
end

type thickness = [
  | `normal
  | `thick
]

let thickness_value = function
  | `thin -> 0.001
  | `normal -> 0.01
  | `thick -> 0.1

type point_shape = [
  | `bullet
  | `circle
]

module Font = struct
  type t = Vg_text.Font.t Lazy.t

  let ascender x = Vg_text.Font.ascender (Lazy.force x)
  let descender x = Vg_text.Font.descender (Lazy.force x)
  let xmin x = Vg_text.Font.xmin (Lazy.force x)
  let xmax x = Vg_text.Font.xmax (Lazy.force x)
  let ymin x = Vg_text.Font.ymin (Lazy.force x)
  let ymax x = Vg_text.Font.ymax (Lazy.force x)

  let embedded_load s =
    Lazy.from_fun (fun () ->
        match Vg_text.Font.load_from_string s with
        | Ok f -> f
        | Error (#Otfm.error as e) ->
          let buf = Buffer.create 253 in
          let fmt = Format.formatter_of_buffer buf in
          Otfm.pp_error fmt e ;
          Format.pp_print_flush fmt () ;
          failwith (Buffer.contents buf)
        | Error (`Read_error msg) ->
          failwithf "Read_error: %s" msg ()
      )

  let dejavu_sans_mono = embedded_load Dejavu_sans_mono.contents
  let dejavu_sans_mono_bold = embedded_load Dejavu_sans_mono_bold.contents
  let dejavu_sans_mono_oblique = embedded_load Dejavu_sans_mono_oblique.contents
  let dejavu_sans_mono_bold_oblique = embedded_load Dejavu_sans_mono_bold_oblique.contents

  let liberation_sans = embedded_load Liberation_sans.contents
  let liberation_sans_bold = embedded_load Liberation_sans_bold.contents
  let liberation_sans_italic = embedded_load Liberation_sans_italic.contents
  let liberation_sans_bold_italic = embedded_load Liberation_sans_bold_italic.contents

  let default = liberation_sans
end

module Picture = struct
  class type t = object
    method render : image
    method bbox : Box2.t
  end

  let points ?(col = Color.black) ?(shape = `bullet) ~x ~y () =
    let xmin = Float_array.min x in
    let xmax = Float_array.max x in
    let ymin = Float_array.min y in
    let ymax = Float_array.max y in
    object
      method render =
        let area = match shape with
          | `bullet -> `Anz
          | `circle ->
            `O { P.o with P.width = thickness_value `thin }
        in
        let mark =
          I.cut ~area (P.empty |> P.circle V2.zero 0.1) (I.const col)
        in
        Array.map2_exn x y ~f:(fun x y ->
            I.move (V2.v x y) mark
          )
        |> Array.fold ~init:I.void ~f:I.blend

      method bbox =
        Box2.v (V2.v xmin ymin) (V2.v (xmax -. xmin) (ymax -. ymin))
    end

  let rect ?draw ?fill ?(thickness = `normal) ~xmin ~xmax ~ymin ~ymax () =
    object
      method render =
        let sw = V2.v xmin ymin in
        let nw = V2.v xmin ymax in
        let ne = V2.v xmax ymax in
        let se = V2.v xmax ymin in
        let p =
          P.empty
          |> P.sub sw
          |> P.line nw
          |> P.line ne
          |> P.line se
          |> P.line sw
        in
        let outline = match draw with
          | None -> I.void
          | Some col ->
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     P.cap = `Square } in
            I.cut ~area p (I.const col)
        in
        let background = match fill with
          | None -> I.void
          | Some col ->
            I.cut ~area:`Anz p (I.const col)
        in
        I.blend outline background

      method bbox =
        Box2.v (V2.v xmin ymin) (V2.v (xmax -. xmin) (ymax -. ymin))
    end

  let circle ?draw ?fill ?(thickness = `normal) ~x ~y ~radius () =
    let c = V2.v x y in
    object
      method render =
        let p =
          P.empty
          |> P.circle c radius
        in
        let outline = match draw with
          | None -> I.void
          | Some col ->
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     P.cap = `Square } in
            I.cut ~area p (I.const col)
        in
        let background = match fill with
          | None -> I.void
          | Some col ->
            I.cut ~area:`Anz p (I.const col)
        in
        I.blend outline background

      method bbox =
        Box2.v_mid c (V2.v (2. *. radius) (2. *. radius))
    end

  let void =
    object
      method render = I.void
      method bbox = Box2.empty
    end

  let blend xs =
    object
      method render =
        List.fold xs ~init:I.void ~f:(fun acc p -> I.blend acc p#render)

      method bbox =
        List.map xs ~f:(fun x -> x#bbox)
        |> List.fold ~init:Box2.empty ~f:Box2.union
    end

  let blend2 x y = blend [ x ; y ]

  let bbox x = x#bbox

  let arrow_head_geometry points =
    match List.rev points with
    | [] | [ _ ] -> None
    | (x1, y1) :: (x2, y2) :: _ ->
      let tip = V2.v x1 y1 in
      let top = V2.v x2 y2 in
      let delta_colinear = V2.(sub top tip |> unit) in
      let delta_ortho = V2.(delta_colinear |> ortho |> smul 0.3) in
      let bottom = V2.(add tip delta_colinear) in
      let wing_up = V2.add bottom delta_ortho in
      let wing_down = V2.sub bottom delta_ortho in
      Some (
        object
          method bottom = bottom
          method tip = tip
          method wing_up = wing_up
          method wing_down = wing_down
        end
      )

  let path ?(col = Color.black) ?(thickness = `normal) ?(arrow_head = false) ?(cap = `Butt) points =
    let arrow_head = if arrow_head then arrow_head_geometry points else None in
    object
      method render =
        let body = match List.rev points with
          | [] | [ _ ] -> I.void
          | (ox, oy) :: (_ :: _ as t) ->
            let tip = match arrow_head with
              | None -> V2.v ox oy
              | Some h -> h#bottom
            in
            let path =
              List.fold t ~init:(P.empty |> P.sub tip) ~f:(fun acc (x, y) ->
                  P.line (V2.v x y) acc
                )
            in
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     cap } in
            I.cut ~area path (I.const col)
        and head = match arrow_head with
          | None -> I.void
          | Some head ->
            let path =
              P.empty
              |> P.sub head#tip
              |> P.line head#wing_up
              |> P.line head#wing_down
              in
              I.cut ~area:`Anz path (I.const col)
        in
        I.blend head body

      method bbox =
        let init = Box2.empty in
        let init =
          List.fold points ~init ~f:(fun acc (x, y) ->
              Box2.add_pt acc (V2.v x y)
            )
        in
        match arrow_head with
        | None -> init
        | Some head ->
          List.fold [ head#tip ; head#wing_up ; head#wing_down ] ~init ~f:Box2.add_pt
    end

  let text ?(col = Color.black) ?(size = 12.) ?(font = Font.default) ?(halign = `middle) ?(valign = `base) ~x ~y text =
    let font = Lazy.force font in
    let layout = Vg_text.Layout.make font ~size text in
    let img = Vg_text.cut ~col:col layout in
    let width, maxy, miny = Vg_text.Layout.(width layout, maxy layout, miny layout) in
    let dx =
      match halign with
      | `middle -> width /. 2.
      | `left -> 0.
      | `right -> width
    in
    let dy =
      match valign with
      | `base -> 0.
      | `top -> maxy
      | `bottom -> miny
    in
    object
      method render =
        I.move (V2.v (x -. dx) (y -. dy)) img

      method bbox =
        let bb = Box2.of_pts V2.(v 0. miny) V2.(v width maxy) in
        Box2.move (V2.v (x -. dx) (y -. dy)) bb
    end


  let translate ?(dx = 0.) ?(dy = 0.) t =
    object
      method bbox = Box2.move (V2.v dx dy) (t#bbox)
      method render =
        t#render
        |> I.move (V2.v dx dy)
    end

  let scale ?(center = `bbox_center) ?(sx = 1.) ?(sy = 1.) t =
    object
      method bbox =
        let bb = t#bbox in
        match center with
        | `bbox_center ->
           Box2.(v_mid (mid bb) (V2.v (w bb *. sx) (h bb *. sy)))
        | `origin ->
           Box2.of_pts
             (V2.v (Box2.minx bb *. sx) (Box2.maxy bb *. sy))
             (V2.v (Box2.maxx bb *. sx) (Box2.miny bb *. sy))
      method render =
        let center =
          match center with
          | `bbox_center -> Box2.mid t#bbox
          | `origin -> V2.zero
        in
        t#render
        |> I.move V2.(neg center)
        |> I.scale (V2.v sx sy)
        |> I.move center
    end

  let reshape t ~bbox =
    object
      method bbox = bbox

      method render =
        let src_bbox = t#bbox in
        let src_center = Box2.mid t#bbox in
        let dst_center = Box2.mid bbox in
        let sx = Box2.(w bbox /. w src_bbox) in
        let sy = Box2.(h bbox /. h src_bbox) in
        t#render
        |> I.move V2.(neg src_center)
        |> I.scale (V2.v sx sy)
        |> I.move dst_center
    end

  let path_of_box2 b =
    P.empty
    |> P.line (Box2.bl_pt b)
    |> P.line (Box2.br_pt b)
    |> P.line (Box2.tr_pt b)
    |> P.line (Box2.tl_pt b)
    |> P.line (Box2.bl_pt b)

  let crop t b =
    object
      method bbox = b
      method render =
        I.cut (path_of_box2 b) t#render
    end

  let frame t =
    object
      method bbox = t#bbox
      method render =
        let bb = t#bbox in
        let sw = Box2.bl_pt bb in
        let nw = Box2.tl_pt bb in
        let ne = Box2.tr_pt bb in
        let se = Box2.br_pt bb in
        let p =
          P.empty
          |> P.sub sw
          |> P.line nw
          |> P.line ne
          |> P.line se
          |> P.line sw
        in
        let area = `O { P.o with P.width = 0.01 ;
                                 P.cap = `Square } in
        I.blend t#render (I.cut ~area p (I.const Color.black))
    end

  (* Tetris-like layout *)
  module Pileup_layout = struct
    type block = {
      bbox : Box2.t ;
      contents : t ;
    }

    let block_intersects b1 b2 =
      let open Float in
      let b1 = b1.bbox in
      let b2 = b2.bbox in
      Box2.(
        minx b1 <= minx b2 && minx b2 <= maxx b1
        || minx b2 <= minx b1 && minx b1 <= maxx b2
      )

    let block_compare b1 b2 =
      let b1 = b1.bbox in
      let b2 = b2.bbox in
      Caml.compare Box2.(minx b1, maxx b1) Box2.(minx b2, maxx b2)

    let make_block contents = {
      bbox = contents#bbox ;
      contents ;
    }

    let x_overlap_partition = function
      | [] -> [], []
      | h :: t ->
        let rec loop inside outside last = function
          | [] -> List.rev (last :: inside), List.rev outside
          | h :: t ->
            if block_intersects last h then
              loop inside (h :: outside) last t
            else
              loop (last :: inside) outside h t
        in
        loop [] [] h t

    let make items =
      let rec loop acc base_y = function
        | [] -> List.rev acc
        | items ->
          let layer, rest = x_overlap_partition items in
          let layer_height =
            List.map layer ~f:(fun bl -> Box2.h bl.bbox)
            |> List.reduce_exn ~f:Float.max
          in
          let translated_layer =
            List.map layer ~f:(fun bl ->
                translate ~dy:(base_y -. Box2.miny bl.bbox) bl.contents
              )
          in
          loop (translated_layer :: acc) (base_y +. layer_height) rest
      in
      let sorted_blocks =
        List.map items ~f:make_block
        |> List.sort ~compare:(fun x y -> block_compare x y)
      in
      let layers =
        match sorted_blocks with
        | [] -> []
        | h :: _ -> loop [] (Box2.maxy h.bbox) sorted_blocks
      in
      List.concat layers
  end

  let pileup xs = blend (Pileup_layout.make xs)

  module VStack_layout = struct
    let make alignment items =
      let bboxes = List.map items ~f:(fun i -> i#bbox) in
      let height = List.fold bboxes ~init:0. ~f:(fun acc bb ->
          acc +. Box2.h bb
        )
      in
      let justify y pic bbox =
        let dy = y -. Box2.maxy bbox in
        let dx = match alignment with
          | `none -> 0.
          | `centered -> -. Box2.midx bbox
          | `left -> -. Box2.minx bbox
          | `right -> -. Box2.maxx bbox
        in
        translate ~dx ~dy pic
      in
      List.fold2_exn items bboxes ~init:(height, []) ~f:(fun (y, acc) pic bbox ->
          let pic' = justify y pic bbox in
          (y -. Box2.h bbox, pic' :: acc)
        )
      |> snd
      |> List.rev
  end

  let vstack ?(align = `none) xs =
    VStack_layout.make align xs
    |> blend

  module HStack_layout = struct
    let make alignment items =
      let bboxes = List.map items ~f:(fun i -> i#bbox) in
      let justify x pic bbox =
        let dx = x -. Box2.minx bbox in
        let dy = match alignment with
          | `none -> 0.
          | `centered -> -. Box2.midy bbox
          | `top -> -. Box2.maxy bbox
          | `bottom -> -. Box2.miny bbox
        in
        translate ~dx ~dy pic
      in
      List.fold2_exn items bboxes ~init:(0., []) ~f:(fun (x, acc) pic bbox ->
          let pic' = justify x pic bbox in
          (x +. Box2.w bbox, pic' :: acc)
        )
      |> snd
      |> List.rev
  end

  let hstack ?(align = `none) xs =
    HStack_layout.make align xs
    |> blend
end

module Plot = struct
  type t =
    | Points of {
        title : string option ;
        col : Color.t ;
        shape : point_shape ;
        x : float array ;
        y : float array ;
      }

  let points ?title ?(col = Color.black) ?(shape = `bullet) x y =
    Points { title ; col ; shape ; x ; y }

  let min_x = function
    | Points { x ; _ } -> Float_array.min x

  let max_x = function
    | Points { x ; _ } -> Float_array.max x

  let min_y = function
    | Points { y ; _ } -> Float_array.min y

  let max_y = function
    | Points { y ; _ } -> Float_array.max y

  let bb plot =
    let minx = min_x plot in
    let miny = min_y plot in
    Box2.v
      (V2.v minx miny)
      (V2.v (max_x plot -. minx) (max_y plot -. miny))

  let render ?(width = 10.) ?(height = 6.) plots =
    match plots with
    | [] -> Picture.void
    | _ ->
      let bb =
        List.map plots ~f:bb
        |> List.reduce_exn ~f:Box2.union
      in
      let vp =
        Viewport.linear
          ~xlim:Box2.(minx bb, maxx bb)
          ~ylim:Box2.(miny bb, maxy bb)
          ~size:(width, height)
      in
      List.map plots ~f:(function
          | Points { x ; y ; col ; shape ; _ } ->
            let x = Array.map x ~f:(Viewport.scale_x vp) in
            let y = Array.map y ~f:(Viewport.scale_y vp) in
            Picture.points ~col ~x ~y ~shape ()
        )
      |> Picture.blend
end

type target = [
  | `File of string
  | `Channel of Stdlib.out_channel
  | `Buffer of Buffer.t
]

module Layout = struct
  type t =
    | Simple of Picture.t

  let simple x = Simple x

  let rec size ?width ?height view =
    let res w h = w *. 10., h *.10. in
    match width, height with
    | Some w, Some h -> res w h
    | Some w, None ->
      let h = Box2.h view *. w /. Box2.w view in
      res w h
    | None, Some h ->
      let w = Box2.w view *. h /. Box2.h view in
      res w h
    | None, None -> size ~width:10. view

  let box2_padding alpha b =
    let w = Box2.w b in
    let h = Box2.h b in
    let delta = Float.(min w h * alpha) in
    Box2.v_mid
      (Box2.mid b)
      (V2.v (w +. delta) (h +. delta))


  let render ?width ?height format (Simple pic) target =
    let view = box2_padding 0.01 pic#bbox in
    let size = size ?width ?height view in
    let image = pic#render in
    let renderer =
      match format with
      | `pdf ->
        let otf_font x =
          Lazy.force x
          |> Vg_text.Font.data
          |> Vgr_pdf.otf_font
          |> function
          | Ok x -> x
          | Error _ -> assert false
        in
        let font (f : Vg.Font.t) =
          match f.name with
          | "DejaVuSansMono" -> otf_font Font.dejavu_sans_mono
          | "DejaVuSansMono-Bold" -> otf_font Font.dejavu_sans_mono_bold
          | "DejaVuSansMono-Oblique" -> otf_font Font.dejavu_sans_mono_oblique
          | "DejaVuSansMono-BoldOblique" -> otf_font Font.dejavu_sans_mono_bold_oblique
          | "LiberationSans" -> otf_font Font.liberation_sans
          | "LiberationSans-Bold" -> otf_font Font.liberation_sans_bold
          | "LiberationSans-Italic" -> otf_font Font.liberation_sans_italic
          | "LiberationSans-BoldItalic" -> otf_font Font.liberation_sans_bold_italic
          | _ -> `Sans
        in
        Vgr_pdf.target ~font ()
      | `svg -> Vgr_svg.target ()
    in
    let render target =
      let r = Vgr.create renderer target in
      ignore (Vgr.render r (`Image (V2.of_tuple size, view, image))) ;
      ignore (Vgr.render r `End)
    in
    match target with
    | `File fn ->
      Out_channel.with_file fn ~f:(fun oc -> render (`Channel oc))
    | (`Channel _ | `Buffer _) as target -> render target
end
