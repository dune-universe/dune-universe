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

open Command
open Path
open Num
open Num.Infix

type skeleton = { width : int; height : int; stepx : Num.t; stepy : Num.t }

let mk_skeleton width height stepx stepy = { width; height; stepx; stepy }

type labels = int -> Num.t -> Picture.t option

type ticks = (Num.t * Pen.t) option

let get_style = function
  | None -> fun _ -> (Dash.evenly, Pen.default)
  | Some f -> f

let off_pattern _ = Dash.pattern [ Dash.on (bp 5.) ]

let defpen _ = Pen.default

let get_borders sx sy h w = (zero, sx */ num_of_int w, sy */ num_of_int h, zero)

let draw_grid ?(hdash = off_pattern) ?(vdash = off_pattern) ?(hpen = defpen)
    ?(vpen = defpen) ?color { width = w; height = h; stepx = sx; stepy = sy } =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let drawline dashed pen p = Command.draw ~pen ~dashed ?color p in
  let horizontal i =
    let y = num_of_int i */ sy in
    let pi = pathn [ (maxl, y); (maxr, y) ] in
    drawline (hdash i) (hpen i) pi
  in
  let vertical i =
    let x = num_of_int i */ sx in
    let pi = pathn [ (x, maxd); (x, maxu) ] in
    drawline (vdash i) (vpen i) pi
  in
  seq
    (Misc.fold_from_to
       (fun acc i -> horizontal i :: acc)
       (Misc.fold_from_to (fun acc i -> vertical i :: acc) [] 0 w)
       0 h)

(* This is a hack, we need the maximal size a label can take *)
let label_scale stepx =
  let max_width = Picture.width (Picture.tex "$55$") in
  4. /. 5. *./ stepx // max_width

(* The default label function, it is quite generic as the labels are resized
 * when they do not fit into a cell *)
let deflabel x w =
  Some
    (Picture.transform
       [ Transform.scaled (label_scale w) ]
       (Picture.tex (Printf.sprintf "$%d$" x)))

let defticks = Some (bp 0.25, Pen.default)

let get_corners maxu maxr =
  ((bp 0., maxu), (maxr, maxu), (bp 0., bp 0.), (maxr, bp 0.))

let draw_axes ?(hpen = Pen.default) ?(vpen = Pen.default) ?(hlabel = deflabel)
    ?(vlabel = deflabel) ?(ticks = defticks) ?(closed = false) ?hcaption
    ?vcaption { width = w; height = h; stepx = sx; stepy = sy } =
  let maxl, maxr, maxu, maxd = get_borders sx sy h w in
  let ul, ur, ll, lr = get_corners maxu maxr in
  let hcaptcmd =
    match hcaption with
    | None -> Command.nop
    | Some labl ->
        let hlabels_height =
          match hlabel w sx with
          | None -> Num.zero
          | Some pic -> Picture.height pic
        in
        let h_caption_height = Picture.height labl in
        Command.label ~pos:`Southwest labl
          (Point.pt
             ( num_of_int w */ sx,
               Num.zero -/ hlabels_height -/ (bp 0.5 */ h_caption_height) ))
  in

  let vcaptcmd =
    match vcaption with
    | None -> Command.nop
    | Some labl ->
        let rot_labl = Picture.transform [ Transform.rotated 90. ] labl in
        let vlabels_width =
          match vlabel h sy with
          | None -> Num.zero
          | Some pic -> Picture.width pic
        in
        let v_caption_width = Picture.width rot_labl in
        Command.label ~pos:`Southwest rot_labl
          (Point.pt
             ( Num.zero -/ vlabels_width -/ (bp 0.5 */ v_caption_width),
               num_of_int h */ sy ))
  in
  let labelcmd pos p i f =
    match f i sx with None -> Command.nop | Some x -> Command.label ~pos x p
  in
  let ticks_cmd pathf =
    match ticks with
    | None -> Command.nop
    | Some (f, pen) -> Command.draw ~pen (pathf f)
  in
  let horizontal i =
    let x = num_of_int i */ sx in
    seq
      [
        labelcmd `South (Point.pt (x, maxd)) i hlabel;
        ticks_cmd (fun f -> pathn [ (x, maxd); (x, maxd +/ (sy */ f)) ]);
        ( if closed then
          ticks_cmd (fun f -> pathn [ (x, maxu); (x, maxu -/ (sy */ f)) ])
        else Command.nop );
      ]
  in
  let vertical i =
    let y = num_of_int i */ sy in
    seq
      [
        labelcmd `Left (Point.pt (maxl, y)) i vlabel;
        ticks_cmd (fun f -> pathn [ (maxl, y); (maxl +/ (sx */ f), y) ]);
        ( if closed then
          ticks_cmd (fun f -> pathn [ (maxr, y); (maxr -/ (sy */ f), y) ])
        else Command.nop );
      ]
  in
  seq
    [
      Command.draw ~pen:hpen (pathn [ ll; lr ]);
      Command.draw ~pen:vpen (pathn [ ll; ul ]);
      ( if closed then
        seq
          [
            Command.draw ~pen:hpen (pathn [ ul; ur ]);
            Command.draw ~pen:vpen (pathn [ lr; ur ]);
          ]
      else Command.nop );
      hcaptcmd;
      vcaptcmd;
      seq
        (Misc.fold_from_to
           (fun acc i -> horizontal i :: acc)
           (Misc.fold_from_to (fun acc i -> vertical i :: acc) [] 0 h)
           0 w);
    ]

let draw_simple_axes ?hpen ?vpen hcaption vcaption sk =
  draw_axes ?hpen ?vpen
    ~hlabel:(fun _ _ -> None)
    ~vlabel:(fun _ _ -> None)
    ~ticks:None ~hcaption:(Picture.tex hcaption)
    ~vcaption:(Picture.rotate (-90.) (Picture.tex vcaption))
    sk

type drawing = Stepwise | Normal

let draw_func ?pen ?(drawing = Normal) ?style ?dashed ?color ?label
    ?(from_x = 0) ?to_x f { width = w; height = h; stepx = sx; stepy = sy } =
  let to_x = match to_x with None -> w | Some x -> x in
  let _, maxr, maxu, _ = get_borders sx sy h w in
  let ul, ur, ll, lr = get_corners maxu maxr in
  let box = pathn ~style:jLine ~cycle:jLine [ ul; ll; lr; ur ] in
  let normal acc i =
    let x, y = (num_of_int i */ sx, Num.bp (f i) */ sy) in
    (x, y) :: acc
  in
  let stepwise (acc, _, y) i =
    let nx, ny = (num_of_int i */ sx, Num.bp (f i) */ sy) in
    ((nx, ny) :: (nx, y) :: acc, nx, ny)
  in
  let graph =
    match drawing with
    | Normal -> Misc.fold_from_to normal [] from_x to_x
    | Stepwise ->
        let p, _, _ =
          Misc.fold_from_to stepwise ([], Num.bp 0., Num.bp 0.) from_x to_x
        in
        p
  in
  let pic =
    Picture.clip
      (Picture.make (Command.draw ?pen ?dashed ?color (pathn ?style graph)))
      box
  in
  match label with
  | None -> draw_pic pic
  | Some (lab, pos, i) ->
      let pt = Point.pt (num_of_int i */ sx, Num.bp (f i) */ sy) in
      seq [ Command.label ~pos lab pt; draw_pic pic ]
