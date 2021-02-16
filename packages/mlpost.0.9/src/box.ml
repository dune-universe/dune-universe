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

open Types
open Num
open Point
open Num.Infix

type style =
  | Rect
  | Circle
  | RoundRect
  | Patatoid
  | Patatoid2
  | Ellipse
  | RoundBox
  | Custom of (Num.t -> Num.t -> Num.t * Num.t * Path.t)

let margin = Num.bp 2.

module Name = struct
  type t = Internal of int | Userdef of string

  let compare = Stdlib.compare

  let print fmt = function
    | Internal i -> Format.pp_print_int fmt i
    | Userdef s -> Format.pp_print_string fmt s
end

module NMap = Map.Make (Name)

let print_dom fmt m =
  Format.fprintf fmt "@[{";
  NMap.iter (fun k _ -> Format.fprintf fmt "%a;@ " Name.print k) m;
  Format.fprintf fmt "}@]"

type t = {
  name : Name.t;
  width : Num.t;
  height : Num.t;
  ctr : Point.t;
  stroke : Color.t option;
  pen : Pen.t option;
  fill : Color.t option;
  contour : Path.t;
  desc : desc;
  dash : Dash.t option;
  post_draw : t -> Command.t;
  pre_draw : t -> Command.t;
}

and desc = Emp | Pic of Picture.t | Grp of t array * t NMap.t

let width b = b.width

let height b = b.height

let ctr b = b.ctr

let bpath b = b.contour

let set_bpath p b = { b with contour = p }

let halfheight b = Point.pt (zero, b.height /./ 2.)

let halfwidth b = Point.pt (b.width /./ 2., zero)

let north b = Point.add b.ctr (halfheight b)

let south b = Point.sub b.ctr (halfheight b)

let east b = Point.add b.ctr (halfwidth b)

let west b = Point.sub b.ctr (halfwidth b)

let build_point a b = Point.pt (xpart a, ypart b)

let north_west x = build_point (west x) (north x)

let north_east x = build_point (east x) (north x)

let south_west x = build_point (west x) (south x)

let south_east x = build_point (east x) (south x)

type vposition = [ Command.vposition | `Custom of t -> Num.t ]

type hposition = [ Command.hposition | `Custom of t -> Num.t ]

type vposition_red = [ Types.vposition_red | `Custom of t -> Num.t ]

type hposition_red = [ Types.hposition_red | `Custom of t -> Num.t ]

type position = [ Command.position | `Custom of t -> Point.t ]

type position_red = [ Types.position_red | `Custom of t -> Point.t ]

let hreduce = function
  | `Custom c -> `Custom c
  | #Command.hposition as p -> (hreduce p :> hposition_red)

let vreduce = function
  | `Custom c -> `Custom c
  | #Command.vposition as p -> (vreduce p :> vposition_red)

let pos_reduce = function
  | `Custom c -> `Custom c
  | #Command.position as p -> (pos_reduce p :> position_red)

let corner pos x =
  match pos with
  | `Custom c -> c x
  | #Types.position as other -> (
      match Types.pos_reduce other with
      | `Northwest -> north_west x
      | `Northeast -> north_east x
      | `Southwest -> south_west x
      | `Southeast -> south_east x
      | `West -> west x
      | `East -> east x
      | `Center -> ctr x
      | `North -> north x
      | `South -> south x )

let cornerh pos x =
  match pos with
  | `Custom c -> c x
  | #Command.position as pos -> xpart (corner pos x)

let cornerv pos x =
  match pos with
  | `Custom c -> c x
  | #Command.position as pos -> ypart (corner pos x)

let rec transform t b =
  let tr = Point.transform t in
  let nw = tr (north_west b)
  and sw = tr (south_west b)
  and se = tr (south_east b) in
  let hvec = Point.sub nw sw and wvec = Point.sub se sw in
  {
    b with
    ctr = Point.transform t b.ctr;
    height = Num.abs (ypart hvec) +/ Num.abs (ypart wvec);
    width = Num.abs (xpart hvec) +/ Num.abs (xpart wvec);
    contour = Path.transform t b.contour;
    desc = transform_desc t b.desc;
  }

and transform_desc t = function
  | Emp -> Emp
  | Pic p -> Pic (Picture.transform t p)
  | Grp (a, m) -> Grp (Array.map (transform t) a, NMap.map (transform t) m)

let rec shift pt b =
  {
    b with
    ctr = Point.shift pt b.ctr;
    contour = Path.shift pt b.contour;
    desc = shift_desc pt b.desc;
  }

and shift_desc pt = function
  | Emp -> Emp
  | Pic p -> Pic (Picture.shift pt p)
  | Grp (a, m) ->
      let s = shift pt in
      Grp (Array.map s a, NMap.map s m)

let scale f p = transform [ Transform.scaled f ] p

let rotate f p = transform [ Transform.rotated f ] p

let yscale n p = transform [ Transform.yscaled n ] p

let xscale n p = transform [ Transform.xscaled n ] p

let center pt x = shift (Point.sub pt x.ctr) x

let border pos b =
  match pos with
  | `North -> ypart (ctr b) +/ (height b /./ 2.)
  | `South -> ypart (ctr b) -/ (height b /./ 2.)
  | `West -> xpart (ctr b) -/ (width b /./ 2.)
  | `East -> xpart (ctr b) +/ (width b /./ 2.)

let rec draw ?(debug = false) b =
  let path_cmd =
    match (b.stroke, b.pen) with
    | None, _ -> Command.nop
    | Some color, None -> Command.draw ~color ?dashed:b.dash b.contour
    | Some color, Some pen -> Command.draw ~pen ~color ?dashed:b.dash b.contour
  in
  let fill_cmd =
    match b.fill with
    | None -> Command.nop
    | Some color -> Command.fill ~color b.contour
  in
  let contents_cmd =
    match b.desc with
    | Emp -> Command.nop
    | Pic pic -> pic
    | Grp (a, _) ->
        Command.iter 0 (Array.length a - 1) (fun i -> draw ~debug a.(i))
  in
  let debug_cmd =
    if debug then
      (* TODO maybe we should better draw the rectangle [w,h]
         instead of the contour *)
      let rect = Path.shift b.ctr (Shapes.rectangle b.width b.height) in
      Command.seq
        [
          Command.draw ~color:Color.red ~dashed:Dash.evenly rect;
          ( match b.name with
          | Name.Internal _ -> Command.nop
          | Name.Userdef s ->
              Command.label ~pos:`Center
                (Picture.tex ("\\tiny " ^ Picture.escape_all s))
                (north_west b) );
        ]
    else Command.nop
  in
  Command.seq
    [ b.pre_draw b; fill_cmd; contents_cmd; path_cmd; debug_cmd; b.post_draw b ]

let rect_ w h = (w, h, Shapes.rectangle w h)

let circ_ w h =
  let m = maxn w h in
  (m, m, Shapes.circle m)

let ellipse_ w h =
  let p = Shapes.ellipse w h in
  let pic = Command.draw p in
  (Picture.width pic, Picture.height pic, p)

let round_rect_ w h =
  let rx = minn w h /./ 10. in
  (w, h, Shapes.round_rect w h rx rx)

let round_box_ w h = (w, h, Shapes.round_box w h)

let patatoid_ w h =
  let p = Shapes.patatoid w h in
  let pic = Command.draw p in
  (Picture.width pic, Picture.height pic, p)

let patatoid2_ w h =
  let p = Shapes.patatoid2 w h in
  let pic = Command.draw p in
  (Picture.width pic, Picture.height pic, p)

let from_style = function
  | Rect -> rect_
  | Circle -> circ_
  | RoundRect -> round_rect_
  | Patatoid -> patatoid_
  | Patatoid2 -> patatoid2_
  | Ellipse -> ellipse_
  | RoundBox -> round_box_
  | Custom f -> f

let make_contour s ?(dx = margin) ?(dy = margin) w h c =
  let w = w +/ (2. *./ dx) and h = h +/ (2. *./ dy) in
  let w, h, p = (from_style s) w h in
  (w, h, Path.shift c p)

let no_drawing _ = Command.nop

let fresh_name =
  let x = ref 1 in
  fun () ->
    incr x;
    Name.Internal !x

let mkbox ?(style = Rect) ?dx ?dy ?name ?brush ?(stroke = Some Color.black) ?pen
    ?dash ?fill ?(pre_draw = no_drawing) ?(post_draw = no_drawing) w h c desc =
  let w, h, s = make_contour style ?dx ?dy w h c in
  let b = Brush.t ?pen ?dash ?color:stroke ?brush () in
  let name =
    match name with None -> fresh_name () | Some s -> Name.Userdef s
  in
  {
    desc;
    name;
    stroke = Brush.color b;
    pen = Brush.pen b;
    fill;
    dash = Brush.dash b;
    width = w;
    height = h;
    ctr = c;
    contour = s;
    post_draw;
    pre_draw;
  }

let pic ?style ?dx ?dy ?name ?brush ?(stroke = None) ?pen ?dash ?fill pic =
  let c = Picture.ctr pic in
  mkbox ?style ?dx ?dy ?name ?brush ~stroke ?pen ?dash ?fill (Picture.width pic)
    (Picture.height pic) c (Pic pic)

let merge_maps =
  let add_one m b =
    let m =
      match b.desc with
      | Emp | Pic _ -> m
      | Grp (_, m') -> NMap.fold NMap.add m' m
    in
    NMap.add b.name b m
  in
  List.fold_left add_one NMap.empty

let box ?style ?dx ?dy ?name ?brush ?stroke ?pen ?dash ?fill b =
  mkbox ?style ?dx ?dy ?name ?brush ?stroke ?pen ?dash ?fill (width b)
    (height b) (ctr b)
    (Grp ([| b |], merge_maps [ b ]))

let path ?style ?dx ?dy ?name ?brush ?(stroke = None) ?pen ?dash ?fill p =
  pic ?style ?dx ?dy ?name ?brush ~stroke ?pen ?dash ?fill
    (Picture.make (Command.draw p))

let empty ?(width=Num.zero) ?(height=Num.zero) ?style ?name ?brush
          ?(stroke=None) ?pen ?dash ?fill () =
  mkbox ?style ?name ~dx:zero ~dy:zero ?brush ~stroke ?pen ?dash ?fill
    width height Point.origin Emp

let empty_from_box ?style ?name ?brush ?(stroke=None) ?pen ?dash ?fill box =
  mkbox ?style ?name ?brush ~stroke ?pen ?dash ?fill ~dx:zero ~dy:zero
    (width box) (height box) (ctr box) (Grp ([||], merge_maps [box] ))

(* groups the given boxes in a new box *)
let group ?style ?(dx = Num.zero) ?(dy = Num.zero) ?name ?brush ?(stroke = None)
    ?pen ?dash ?fill bl =
  let xmin b = xpart (south_west b) in
  let xmax b = xpart (north_east b) in
  let ymin b = ypart (south_west b) in
  let ymax b = ypart (north_east b) in
  match bl with
  | [] ->
      empty ~width:dx ~height:dy ?style ?name ?brush ~stroke ?pen ?dash ?fill ()
  | [ b ] -> box ?style ~dx ~dy ?name ?brush ~stroke ?pen ?dash ?fill b
  | b :: r ->
      let xmin, xmax, ymin, ymax =
        List.fold_left
          (fun (xmin', xmax', ymin', ymax') b ->
            ( Num.minn xmin' (xmin b),
              Num.maxn xmax' (xmax b),
              Num.minn ymin' (ymin b),
              Num.maxn ymax' (ymax b) ))
          (xmin b, xmax b, ymin b, ymax b)
          r
      in
      let w = xmax -/ xmin in
      let h = ymax -/ ymin in
      let c = Point.pt (xmin +/ (w /./ 2.), ymin +/ (h /./ 2.)) in
      mkbox ?style ~dx ~dy ?name ?brush ~stroke ?pen ?dash ?fill w h c
        (Grp (Array.of_list bl, merge_maps bl))

let group_array ?name ?brush ?stroke ?fill ?dx ?dy ba =
  group ?name ?brush ?stroke ?fill ?dx ?dy (Array.to_list ba)

(* groups the given boxes in a rectangular shape of size [w,h]
   and center [c] *)
let group_rect ?name ?(stroke = None) w h c bl =
  mkbox ~dx:zero ~dy:zero ?name ~stroke w h c
    (Grp (Array.of_list bl, merge_maps bl))

type 'a box_creator =
  ?dx:Num.t ->
  ?dy:Num.t ->
  ?name:string ->
  ?brush:Brush.t ->
  ?stroke:Color.t option ->
  ?pen:Pen.t ->
  ?dash:Dash.t ->
  ?fill:Color.t ->
  'a ->
  t

let rect = box ~style:Rect

let circle = box ~style:Circle

let ellipse = box ~style:Ellipse

let round_rect = box ~style:RoundRect

let patatoid = box ~style:Patatoid

let patatoid2 = box ~style:Patatoid2

let round_box = box ~style:RoundBox

let tex ?style ?dx ?dy ?name ?brush ?(stroke = None) ?pen ?dash ?fill s =
  pic ?style ?dx ?dy ?name ?brush ~stroke ?pen ?dash ?fill (Picture.tex s)

let nth i b =
  match b.desc with
  | Grp (a, _) ->
      let n = Array.length a - 1 in
      if i < 0 || i > n then
        invalid_arg (Format.sprintf "Box.nth: index %d out of 0..%d" i n);
      a.(i)
  | Emp -> invalid_arg "Box.nth: empty box"
  | Pic _ -> invalid_arg "Box.nth: picture box"

let elts b = match b.desc with Emp | Pic _ -> [||] | Grp (a, _) -> a

let elts_list b = Array.to_list (elts b)

let get' n b =
  if b.name = n then b
  else
    match b.desc with
    | Emp -> invalid_arg "Box.get: empty box"
    | Pic _ -> invalid_arg "Box.get: picture box"
    | Grp (_, m) -> (
        try NMap.find n m
        with Not_found ->
          invalid_arg
            (Misc.sprintf "Box.get: no sub-box %a out of %a" Name.print n
               print_dom m) )

let get n b = get' (Name.Userdef n) b

let sub b1 b2 = get' b1.name b2

let relative b g =
  let b' = sub b g in
  let v = Point.sub (ctr b) (ctr b') in
  shift v g

let get_fill b = b.fill

let set_fill c b = { b with fill = Some c }

let get_stroke b = b.stroke

let set_stroke s b = { b with stroke = Some s }

let clear_stroke b = { b with stroke = None }

let get_name b =
  match b.name with Name.Internal _ -> None | Name.Userdef s -> Some s

let get_dash b = b.dash

let set_dash d b = { b with dash = Some d }

let clear_dash b = { b with dash = None }

let set_name name b = { b with name = Name.Userdef name }

let set_post_draw f b = { b with post_draw = f }

let set_pre_draw f b = { b with pre_draw = f }

let add_post_draw f b =
  let d = b.post_draw in
  { b with post_draw = (fun t -> Command.append (d t) (f t)) }

let clear_post_draw b = { b with post_draw = no_drawing }

let clear_pre_draw b = { b with pre_draw = no_drawing }

let shadow b =
  let shadow b =
    let shad i =
      let d = bp (i /. 2.) in
      Command.fill
        ~color:(Color.gray (0.2 +. (i *. 0.2)))
        (Path.shift (Point.pt (d, d)) (bpath b))
    in
    Command.seq (List.rev_map shad [ 1.; 2.; 3. ])
  in
  { b with pre_draw = shadow }

let get_pen b = b.pen

let set_pen p b = { b with pen = Some p }

let set_contour c b = { b with contour = c }

(* new box primitives *)

let ycoord pos a =
  (* get the vertical position of a box, using a either Top, Bot or the
     center as reference *)
  match vreduce pos with
  | `Custom c -> c a
  | #Types.vposition_red as p -> (
      match p with
      | `Center -> ypart (ctr a)
      | (`North | `South) as x -> border x a )

let xcoord pos a =
  (* get the horizontal position of a box, using a either Left, Right or the
     center as reference *)
  match hreduce pos with
  | `Custom c -> c a
  | #Types.hposition_red as p -> (
      match p with
      | `Center -> xpart (ctr a)
      | (`West | `East) as x -> border x a )

let box_fold f acc l =
  let _, l =
    List.fold_left
      (fun (acc, l) b ->
        let acc, b = f acc b in
        (acc, b :: l))
      (acc, []) l
  in
  List.rev l

let halign ?(pos : vposition = `Center) y l =
  List.map (fun b -> shift (Point.pt (zero, y -/ ycoord pos b)) b) l

let set_height pos h b =
  let nc =
    match vreduce pos with
    | `Center -> ypart b.ctr
    | `North -> ypart b.ctr +/ ((b.height -/ h) /./ 2.)
    | `South -> ypart b.ctr -/ ((b.height -/ h) /./ 2.)
    | `Custom c ->
        let n = c b in
        n +/ ((ypart b.ctr -/ n) */ (h // b.height))
  in
  { b with height = h; ctr = Point.pt (xpart b.ctr, nc) }

let set_width pos w b =
  let nc =
    match hreduce pos with
    | `Center -> xpart b.ctr
    | `West -> xpart b.ctr -/ ((b.width -/ w) /./ 2.)
    | `East -> xpart b.ctr +/ ((b.width -/ w) /./ 2.)
    | `Custom c ->
        let n = c b in
        n +/ ((xpart b.ctr -/ n) */ (w // b.width))
  in
  { b with width = w; ctr = Point.pt (nc, ypart b.ctr) }

let set_gen2 mycorner chdim pos1 y1 pos2 y2 box =
  let pos1 = mycorner pos1 box in
  let pos2 = mycorner pos2 box in
  let a = (y1 -/ y2) // (pos1 -/ pos2) in
  let b = ((y2 */ pos1) -/ (y1 */ pos2)) // (pos1 -/ pos2) in
  let w, h = chdim (fun x -> a */ x) (box.width, box.height) in
  let ctr = chdim (fun x -> (a */ x) +/ b) (xpart box.ctr, ypart box.ctr) in
  { box with width = w; height = h; ctr = Point.pt ctr }

let set_height2 pos1 y1 pos2 y2 b =
  set_gen2 cornerv (fun conv (x, y) -> (x, conv y)) pos1 y1 pos2 y2 b

let set_width2 pos1 y1 pos2 y2 b =
  set_gen2 cornerh (fun conv (x, y) -> (conv x, y)) pos1 y1 pos2 y2 b

let valign ?(pos = `Center) x l =
  List.map (fun b -> shift (Point.pt (x -/ xcoord pos b, zero)) b) l

let extractv pos =
  match pos_reduce pos with
  | `Northwest | `North | `Northeast -> `North
  | `West | `Center | `East -> `Center
  | `Southwest | `South | `Southeast -> `South
  | `Custom c -> `Custom (fun t -> ypart (c t))

let extracth pos =
  match pos_reduce pos with
  | `Northwest | `West | `Southwest -> `West
  | `North | `Center | `South -> `Center
  | `Northeast | `East | `Southeast -> `East
  | `Custom c -> `Custom (fun t -> xpart (c t))

let set_size pos ~width ~height b =
  set_height (extractv pos) height (set_width (extracth pos) width b)

let max_height l = Num.fold_max height Num.zero l

let max_width l = Num.fold_max width Num.zero l

let same_size ?(pos = `Center) bl =
  List.map (set_size pos ~width:(max_width bl) ~height:(max_height bl)) bl

let same_height ?(pos = `Center) bl =
  List.map (set_height pos (max_height bl)) bl

let same_width ?(pos = `Center) bl = List.map (set_width pos (max_width bl)) bl

let hplace ?(padding = zero) ?(pos = `Center) ?(min_width = zero)
    ?(same_width = false) l =
  if l = [] then []
  else
    let min_width =
      if same_width then Num.maxn (max_width l) min_width else min_width
    in
    let l =
      List.map
        (fun b -> set_width (extracth pos) (Num.maxn min_width b.width) b)
        l
    in
    let refb = List.hd l in
    let refc = ctr refb and refw = width refb in
    box_fold
      (fun x p ->
        ( x +/ p.width +/ padding,
          center (Point.pt (x +/ (p.width /./ 2.), ypart p.ctr)) p ))
      (xpart refc -/ (refw /./ 2.))
      l

let vplace ?(padding = zero) ?(pos = `Center) ?(min_height = zero)
    ?(same_height = false) l =
  if l = [] then []
  else
    let min_height =
      if same_height then Num.maxn (max_height l) min_height else min_height
    in
    let l =
      List.map
        (fun b -> set_height (extractv pos) (Num.maxn min_height b.height) b)
        l
    in
    let refb = List.hd l in
    let refc = ctr refb and refh = height refb in
    box_fold
      (fun y p ->
        ( y -/ p.height -/ padding,
          center (Point.pt (xpart p.ctr, y -/ (p.height /./ 2.))) p ))
      (ypart refc +/ (refh /./ 2.))
      l

let hbox_list ?padding ?(pos = `Center) ?min_width ?same_width l =
  match l with
  | [] -> []
  | hd :: _ ->
      let y = ypart (corner pos hd) in
      halign ~pos:(extractv pos) y
        (hplace ?padding ~pos ?min_width ?same_width l)

let vbox_list ?padding ?(pos = `Center) ?min_height ?same_height l =
  match l with
  | [] -> []
  | hd :: _ ->
      let x = xpart (corner pos hd) in
      let l = vplace ?padding ~pos ?min_height ?same_height l in
      valign ~pos:(extracth pos) x l

let hequalize h l = List.map (set_height h) l

let wequalize w l = List.map (set_width w) l

let hbox ?padding ?pos ?style ?min_width ?same_width ?dx ?dy ?name ?brush
    ?stroke ?pen ?dash ?fill l =
  group ?style ?dx ?dy ?name ?brush ?stroke ?pen ?dash ?fill
    (hbox_list ?padding ?pos ?min_width ?same_width l)

let vbox ?padding ?pos ?style ?min_height ?same_height ?dx ?dy ?name ?brush:_
    ?stroke ?pen ?dash ?fill l =
  group ?style ?dx ?dy ?name ?stroke ?pen ?dash ?fill
    (vbox_list ?padding ?pos ?min_height ?same_height l)

let modify_box ?stroke ?pen ?dash b =
  let s = match stroke with None -> Some Color.black | Some x -> x in
  {
    b with
    stroke = s;
    pen;
    dash;
    contour = Path.shift b.ctr (Shapes.rectangle b.width b.height);
  }

let hblock ?padding ?(pos = `Center) ?name ?stroke ?pen ?dash ?min_width
    ?same_width pl =
  group ?name
    (List.map
       (modify_box ?stroke ?pen ?dash)
       (hbox_list ?padding ~pos ?min_width ?same_width
          (List.map (set_height (extractv pos) (max_height pl)) pl)))

let vblock ?padding ?(pos = `Center) ?name ?stroke ?pen ?dash ?min_height
    ?same_height pl =
  group ?name
    (List.map
       (modify_box ?stroke ?pen ?dash)
       (vbox_list ?padding ~pos ?min_height ?same_height
          (List.map (set_width (extracth pos) (max_width pl)) pl)))

let tabularl ?hpadding ?vpadding ?(pos = `Center) ?style ?name ?stroke ?pen
    ?dash ?fill pll =
  (* we first compute the widths of columns and heights of rows *)
  let hmaxl = List.map (Num.fold_max height Num.zero) pll in
  let rec calc_wmax pll =
    match pll with
    | [] :: _ -> []
    | _ ->
        let cols, qll =
          List.fold_left
            (fun (col, rem) pl -> (List.hd pl :: col, List.tl pl :: rem))
            ([], []) pll
        in
        Num.fold_max width Num.zero cols :: calc_wmax qll
  in
  let wmaxl = calc_wmax pll in
  let pll =
    List.map2
      (fun row height ->
        List.map2
          (fun cell width -> set_size pos ~height ~width (group [ cell ]))
          row wmaxl)
      pll hmaxl
  in
  vbox ?padding:vpadding ~pos ?style ?name ?stroke ?pen ?dash ?fill
    (List.map (fun r -> hbox ?padding:hpadding ~pos r) pll)

let tabular ?(hpadding = Num.zero) ?(vpadding = Num.zero) ?pos ?style ?name
    ?stroke ?pen ?dash ?fill m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  tabularl ~hpadding ~vpadding ?pos ?style ?name ?stroke ?pen ?dash ?fill pll

let tabulari ?(hpadding = Num.zero) ?(vpadding = Num.zero) ?pos ?style ?name
    ?stroke ?pen ?dash ?fill w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  tabular ~hpadding ~vpadding ?pos ?style ?name ?stroke ?pen ?dash ?fill m

let gridl ?hpadding ?vpadding ?(pos = `Center) ?stroke ?pen ?dash pll =
  let hmax = Num.fold_max (Num.fold_max height Num.zero) Num.zero pll in
  let wmax = Num.fold_max (Num.fold_max width Num.zero) Num.zero pll in
  let pll =
    List.map
      (fun l ->
        List.map
          (fun c ->
            set_height (extractv pos) hmax (set_width (extracth pos) wmax c))
          l)
      pll
  in
  let pll =
    vbox_list ~pos ?padding:vpadding
      (List.map
         (fun r ->
           group
             (List.map
                (modify_box ?stroke ?pen ?dash)
                (hbox_list ?padding:hpadding ~pos r)))
         pll)
  in
  group pll

let grid ?hpadding ?vpadding ?pos ?stroke ?pen ?dash m =
  let pll = Array.to_list (Array.map Array.to_list m) in
  gridl ?hpadding ?vpadding ?pos ?stroke ?pen ?dash pll

let gridi ?hpadding ?vpadding ?pos ?stroke ?pen ?dash w h f =
  let m = Array.init h (fun j -> Array.init w (fun i -> f i j)) in
  grid ?hpadding ?vpadding ?pos ?stroke ?pen ?dash m

let henlarge l =
  let toh f x = xpart (f x) in
  let min = Num.fold_min (toh west) (bp infinity) l in
  let max = Num.fold_max (toh east) (bp neg_infinity) l in
  List.map (set_width2 `West min `East max) l

let venlarge l =
  let tow f x = ypart (f x) in
  let min = Num.fold_min (tow south) (bp infinity) l in
  let max = Num.fold_max (tow north) (bp neg_infinity) l in
  List.map (set_height2 `North max `South min) l

module P = Path

let strip ?sep p = match sep with None -> p | Some n -> Path.strip n p

let cpath ?style ?outd ?ind ?sep a b =
  let r, l = (outd, ind) in
  let p = P.pathk ?style [ P.knotp ?r (ctr a); P.knotp ?l (ctr b) ] in
  strip ?sep (P.cut_after (bpath b) (P.cut_before (bpath a) p))

let cpath_left ?style ?outd ?ind ?sep a b =
  let r, l = (outd, ind) in
  let p = P.pathk ?style [ P.knotp ?r (ctr a); P.knotp ?l b ] in
  strip ?sep (P.cut_before (bpath a) p)

let cpath_right ?style ?outd ?ind ?sep a b =
  let r, l = (outd, ind) in
  let p = P.pathk ?style [ P.knotp ?r a; P.knotp ?l (ctr b) ] in
  strip ?sep (P.cut_after (bpath b) p)

(* (* Deleted because of circular dependency with the Arrow module.
It did not seem to be used anyway. *)
let thick_arrow ?style ?(boxed=true) ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width a b =
  let p = cpath a b in
  let pa = Path.point 0. p in
  let pb = Path.point 1. p in
  Arrow.draw_thick ?style ~boxed ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width pa pb
*)

(* Specials Points *)
let setp name pt box =
  let add_smap m = NMap.add (Name.Userdef name) (shift pt (empty ~name ())) m in
  {
    box with
    desc =
      ( match box.desc with
      | Emp -> Grp ([| box |], add_smap NMap.empty)
      | Pic _ -> Grp ([| box |], add_smap NMap.empty)
      | Grp (l, m) -> Grp (l, add_smap m) );
  }

let getp name box = ctr (get name box)

let getpx name box = xpart (getp name box)

let getpy name box = ypart (getp name box)

(*let place_relative_to
    ?(same_height=false)
    ?(pos=`Center)
    ?pos2
    ?(offset=Num.zero)
    ?(orientation)
    box1 box2 =
  let pos = pos_reduce pos in
  let pos2 = match pos2 with
    | None -> inverse_pos pos
    | Some s -> pos_reduce s in
  let [box1;box2] =
    if same_height
    then same_height [box1;box2]
    else [box1;box2] in
  let point1 = corner pos box1 in
  let point2 = corner pos box2 in
  let orient = match orient with
    | None -> Point.normalize (Point.sub point1 (ctr box1))
    | Some s -> pos_reduce s in
  let vec = normalize
*)

(* placement *)

let opposite_position : position -> position = function
  | #Types.position as x -> (Types.opposite_position x :> position)
  | `Custom f -> `Custom (fun b -> Point.sub (ctr b) (f b))

let place posa ?(pos = opposite_position posa) ?padding a b =
  let pa = corner posa a in
  let pb = corner pos b in
  let c = shift (Point.sub pa pb) b in
  match padding with
  | None -> c
  | Some padding ->
      shift (Point.mult padding (normalize (Point.sub pa (ctr a)))) c

(* Boxlike *)

let set_pos = center
