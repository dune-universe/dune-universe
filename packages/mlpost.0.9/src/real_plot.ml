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

module C = Command
module P = Picture

type ('a, 'b) node = { values : 'b; node : 'a }

type 'a curve = ('a, (float -> float option) list) node

type 'a graph = 'a curve list

let curve_l fl node = { values = fl; node }

let curve_opt f node = curve_l [ f ] node

let curve f node = curve_opt (fun x -> Some (f x)) node

let graph x = x

let rec calc_one_value nb_f ((which_f, acc) as w_acc) x = function
  | [] -> (nb_f, None :: acc)
  | f :: lf -> (
      match f x with
      | None -> calc_one_value (nb_f + 1) w_acc x lf
      | Some y ->
          if which_f = nb_f then (nb_f, Some (x, y) :: acc)
          else (nb_f, Some (x, y) :: None :: acc) )

let calc xmin xmax pitch { values = cf; node } =
  let rec aux acc = function
    | x when x <= xmin -> calc_one_value 0 acc xmin cf
    | x -> aux (calc_one_value 0 acc x cf) (x -. pitch)
  in
  { values = snd (aux (0, []) xmax); node }

open MetaPath

let cons_opt x l = match x with None -> l | Some x -> to_path x :: l

let rec pathn_opt acc current = function
  | [] -> cons_opt current acc
  | v :: l -> (
      match (v, current) with
      | None, _ -> pathn_opt (cons_opt current acc) None l
      | Some v, None -> pathn_opt acc (Some (start (knotn v))) l
      | Some v, Some c ->
          pathn_opt acc (Some (concat ~style:jLine c (knotn v))) l )

let draw_aux ?label:_ values =
  C.seq
    (List.map
       (fun (values, brush) ->
         let line = pathn_opt [] None values in
         C.seq (List.map (Path.draw ~brush) line))
       values)

let ysep = 10

let tick pitch xmax nb =
  let rec aux acc x = function
    | n when n <= 0 -> acc
    | n -> aux (x :: acc) (x -. pitch) (n - 1)
  in
  aux [] xmax nb

let tick_log xmax =
  let rec aux acc = function
    | x when x > xmax -> acc
    | x -> aux (x :: acc) (x *. 10.)
  in
  aux [] 1.

let tick_logneg xmin =
  let rec aux acc = function
    | x when x < xmin -> acc
    | x -> aux (x :: acc) (x *. 10.)
  in
  aux [] (-1.)

let vtick =
  let t = Point.bpp (2.5, 0.) in
  fun v -> (Point.sub v t, Point.add v t)

let draw_axes ~logarithmic:_ ~ytick ~xmin ~xmax ~ymin ~ymax ~yzero ~xzero
    ~pitch:_ =
  let ytick = C.seq ytick in
  let vert =
    Arrow.simple (Path.pathn ~style:Path.jLine [ (xzero, ymin); (xzero, ymax) ])
  in
  let hori =
    Arrow.simple (Path.pathn ~style:Path.jLine [ (xmin, yzero); (xmax, yzero) ])
  in
  C.seq [ ytick; vert; hori ]

let count_max iter =
  let y = ref neg_infinity in
  iter (fun x -> y := max !y x);
  !y

let count_min iter =
  let y = ref infinity in
  iter (fun x -> y := min !y x);
  !y

let filter_opt f l =
  {
    l with
    values =
      List.map (function Some (_, y) as p when f y -> p | _ -> None) l.values;
  }

let draw ?(logarithmic = false) ?curve_brush:_ ?label ?ymin ?ymax ~xmin ~xmax
    ~pitch ~width ~height graph =
  let values = List.map (calc xmin xmax pitch) graph in
  (* ymin, ymax calculation *)
  let values =
    match (ymin, ymax) with
    | None, None -> values
    | _ ->
        let f =
          match (ymin, ymax) with
          | None, None -> assert false
          | Some ymin, None -> fun f -> f >= ymin
          | Some ymin, Some ymax -> fun f -> f >= ymin && f <= ymax
          | None, Some ymax -> fun f -> f <= ymax
        in
        List.map (filter_opt f) values
  in
  let yvalues f =
    List.iter
      (fun x -> List.iter (function Some (_, y) -> f y | None -> ()) x.values)
      values
  in
  let ymax = match ymax with None -> count_max yvalues | Some ymax -> ymax in
  (*let ymax =  if ymax = 0. then 1. else ymax in *)
  let ymin = match ymin with None -> count_min yvalues | Some ymin -> ymin in
  (*let ymin = if ymin = 0. then -.1. else ymin in  *)
  let ymax = if ymin = ymax then ymin +. 1. else ymax in
  (* scale *)
  let conv =
    if logarithmic then fun v ->
      if abs_float v < 1. then v
      else (log10 (abs_float v) +. 1.) *. (v /. abs_float v)
    else fun v -> v
  in
  let scaley = Num.divn height (Num.bp (conv ymax -. conv ymin)) in
  let scalex = Num.divn width (Num.bp (xmax -. xmin)) in
  let scalex x = Num.multn (Num.bp (x -. xmin)) scalex in
  let scaley y = Num.multn (Num.bp (conv y -. conv ymin)) scaley in
  let scale (x, y) = (scalex x, scaley y) in
  let scale_opt = function
    | Some (x, y) -> Some (scale (x, y))
    | None -> None
  in
  let xzero, yzero = scale (0., 0.) in
  (* tick vertical *)
  let ymm = ymax -. ymin in
  (*let xmm = xmax -. xmin in*)
  let ypitchl =
    if logarithmic then
      let l1 = tick_log ymax in
      let l2 = tick_logneg ymin in
      l1 @ l2
    else
      let ypitch = 10. ** floor (log10 (ymm /. float ysep)) in
      let ymax2 = ypitch *. floor (ymax /. ypitch) in
      let ysep = int_of_float (ymm /. ypitch) in
      tick ypitch ymax2 ysep
  in

  let ypitchl = (ymin :: ypitchl) @ [ ymax ] in
  (* Remove the tick which are too close but we need concrete
     for that... Currently only for ex, but we can be more
     precise *)
  let ypitchl =
    if not Concrete.supported then List.map (fun y -> (y, scaley y)) ypitchl
    else
      let ex2 = 2. *. Num.ex_factor () in
      let _, ypitchl =
        List.fold_left
          (fun (last, acc) y ->
            let yn = scaley y in
            let f = Concrete.float_of_num yn in
            if abs_float (last -. f) > ex2 then (f, (y, yn) :: acc)
            else (last, acc))
          (infinity, []) ypitchl
      in
      ypitchl
  in
  let zero = scalex 0. in
  let ytick =
    List.map
      (fun (y, yn) ->
        let p = Point.pt (zero, yn) in
        let p1, p2 = vtick p in
        let label = Format.sprintf "{%2.1f}" y in
        C.seq
          [
            C.label ~pos:`West (Picture.tex label) p1;
            Path.draw (Path.pathp ~style:Path.jLine [ p1; p2 ]);
          ])
      ypitchl
  in
  (* values *)
  let values =
    List.map (fun x -> { x with values = List.map scale_opt x.values }) values
  in
  (* Brush and legend *)
  let color = Color.color_gen 1. 1. in
  let curve_brush _ = Brush.t () in
  let colors =
    List.map
      (fun x ->
        let b = curve_brush x.node in
        let b, c =
          match Brush.color b with
          | Some c -> (b, c)
          | None ->
              let c = color () in
              (Brush.t ~color:c ~brush:b (), c)
        in
        (b, c, x))
      values
  in
  let legend =
    match label with
    | None -> C.nop
    | Some label ->
        let legend =
          Legend.legend (List.map (fun (_, c, x) -> (c, label x.node)) colors)
        in
        C.label ~pos:`East legend
          (Point.pt (scale (xmax, (ymax +. ymin) /. 2.)))
  in
  let values = List.map (fun (b, _, x) -> (x.values, b)) colors in
  let xmin, ymin = scale (xmin, ymin) in
  let xmax, ymax = scale (xmax, ymax) in
  let axes =
    draw_axes ~logarithmic ~ytick ~xmin ~xmax ~ymin ~ymax ~yzero ~xzero ~pitch
  in
  C.seq [ axes; draw_aux ?label values; legend ]
