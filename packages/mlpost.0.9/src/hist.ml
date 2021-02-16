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
open Color
open Box
open Num
open Point
open Path

type 'a labels = Values | User of 'a list

type path_3D =
  | Prems of (Path.t * Path.t * Color.t)
  | PasPrems of (Path.t * Color.t)

module Q = Misc.Q

let max l =
  let rec max_aux acc = function
    | [] -> acc
    | x :: res -> if x > acc then max_aux x res else max_aux acc res
  in
  max_aux (List.hd l) l

let maxlist l =
  let rec max_aux acc = function
    | [] -> acc
    | x :: res ->
        let m = max x in
        if m > acc then max_aux m res else max_aux acc res
  in
  max_aux (max (List.hd l)) l

(* Valeur maximale dans un histogramme cumulé *)
let maxcumul l =
  let rec list_aux acc l =
    match l with
    | [] -> acc
    | x :: res ->
        list_aux (List.fold_left (fun acc x -> acc +. x) 0. x :: acc) res
  in
  max (list_aux [] l)

(* Ne fonctionne que si b est une hbox qui contient au moins une boite *)
let move_hbox _cumul cpt scalex b =
  let p = Box.south_west b in
  Box.shift
    (Point.pt (multf (float cpt) scalex, zero))
    (Box.shift (Point.sub Point.origin p) b)

let default_vlabel i _ = Some (Picture.tex (string_of_int i))

let laxe ~nbcol ?(vlabel = default_vlabel) padding scalex scaley hcaption
    vcaption valmax nbval =
  let hlabel _ _ = None in
  let axe =
    Plot.mk_skeleton (nbval / nbcol) (int_of_float valmax)
      (addn (multf (float nbcol) scalex) padding)
      scaley
  in
  Plot.draw_axes ?hcaption ?vcaption ~vlabel ~hlabel ~ticks:None axe

let rec draw_perspect acc = function
  | Prems (p1, p2, c) :: r ->
      draw_perspect
        ( fill ~color:c p1 ++ fill ~color:c p2 ++ Command.draw p1
        ++ Command.draw p2 ++ acc )
        r
  | PasPrems (p, c) :: r ->
      draw_perspect (fill ~color:c p ++ Command.draw p ++ acc) r
  | [] -> acc

(* Construit les composantes 3D à partir d'un vecteur, d'une boite et d'une couleur *)
let perspect scale derns b c =
  let c = match c with None -> Color.white | Some i -> i in
  let nw = north_west b in
  let ne = north_east b in
  let se = south_east b in
  let dep = divf scale 3. in
  let p1 =
    [
      nw;
      ne;
      pt (addn (xpart ne) dep, addn (ypart ne) dep);
      pt (addn (xpart nw) dep, addn (ypart nw) dep);
    ]
  in
  let p2 =
    [
      ne;
      se;
      pt (addn (xpart se) dep, addn (ypart se) dep);
      pt (addn (xpart ne) dep, addn (ypart ne) dep);
    ]
  in
  let path1 = pathp ~cycle:jLine ~style:jLine p1 in
  let path2 = pathp ~cycle:jLine ~style:jLine p2 in
  (* (fill ~color:c path1) ++ (fill ~color:c path2) ++  *)
  (*       (Command.draw path1) ++ (Command.draw path2) *)
  if derns then Prems (path1, path2, c) else PasPrems (path2, c)

let rec mk_perspect2 acc prems scale hb i j l =
  match l with
  | _ :: res ->
      let b = Box.nth j (Box.nth i hb) in
      let paths = perspect scale prems b (get_fill b) in
      mk_perspect2 (paths :: acc) false scale hb i (j + 1) res
  | [] -> acc

let box_perspect scale hb l =
  let rec mk_perspect acc scale hb i l =
    match l with
    | x :: res ->
        mk_perspect
          (mk_perspect2 [] true scale hb i 0 x @ acc)
          scale hb (i + 1) res
    | [] -> acc
  in
  mk_perspect [] scale hb 0 l

(* Gère la position du label pour qu'elle soit cohérente *)
let label_direction poslab x =
  match Types.vreduce poslab with
  | `North -> if x < 0. then (`South, Box.south) else (`South, Box.north)
  | `South -> if x < 0. then (`North, Box.south) else (`South, Box.north)
  | `Center -> (`Center, Box.ctr)

let rec mk_labels2 acc poslab i j hb l l2 =
  match (l, l2) with
  | x :: res, x2 :: res2 ->
      let sens, haut = label_direction poslab x2 in
      let b = Box.nth j (Box.nth i hb) in
      mk_labels2
        (acc ++ Command.label ~pos:sens x (haut b))
        poslab i (j + 1) hb res res2
  | [], [] -> acc
  | _, _ -> failwith "Both datas and labels lists must have the same size"

(* Positionne les labels sur chaque élément de l'histogramme *)
let box_labels lab hb l =
  let rec mk_labels acc poslab i hb l l2 =
    match (l, l2) with
    | x :: res, x2 :: res2 ->
        mk_labels
          ( acc
          ++ mk_labels2 Command.nop poslab i 0 hb (List.rev x) (List.rev x2) )
          poslab (i + 1) hb res res2
    | [], [] -> acc
    | _, _ -> failwith "Both datas and labels lists must have the same size"
  in
  match lab with
  | poslab, Values ->
      let picl2 l = List.map (fun x -> Picture.tex (string_of_float x)) l in
      let picl l = List.map (fun x -> picl2 x) l in
      mk_labels nop poslab 0 hb (picl l) l
  | poslab, User pl -> mk_labels nop poslab 0 hb pl l

(* Positionne les labels sous chaque barre *)
let hist_labels hlab hb =
  let rec mk_labels acc i hlab =
    match hlab with
    | x :: res ->
        mk_labels
          (acc ++ (Command.label ~pos:`South x) (Box.south (Box.nth i hb)))
          (i + 1) res
    | [] -> acc
  in
  mk_labels nop 0 hlab

(* Fonction de dessin d'histogramme *)
let hist ~cumul _width _height padding fill perspective scalex scaley ?histlabel
    ?hlabel cpt l =
  let rec consvbox boxs = function
    | [], cq -> (vbox boxs, cq)
    | x :: res, cq ->
        let c, cq = Q.pop cq in
        let b =
          set_fill c
            (set_stroke black (empty ~width:scalex ~height:(multf x scaley) ()))
        in
        consvbox (b :: boxs) (res, Q.push c cq)
  in
  let rec fct_hist boxs = function
    | [], _ -> hbox ~pos:`South ~padding (List.rev boxs)
    | x :: res, collist ->
        let lavbox, listcol = consvbox [] (x, collist) in
        fct_hist (lavbox :: boxs) (res, if cumul then collist else listcol)
  in
  let fcth = fct_hist [] (l, fill) in
  let hb = move_hbox cumul cpt scalex fcth in
  let persp =
    if perspective then draw_perspect nop (box_perspect scalex hb l) else nop
  in
  let labels =
    match histlabel with None -> nop | Some lab -> box_labels lab hb l
  in
  persp ++ Box.draw hb ++ labels
  ++ match hlabel with None -> nop | Some hlab -> hist_labels hlab hb

let drawing_parameters width height ?padding nbval valmax nbcol =
  let padding =
    match padding with None -> divf width (4. *. float nbval) | Some i -> i
  in
  let scalex =
    divf
      (subn width (multf (float ((nbval - 1) / nbcol)) padding))
      (float nbval)
  in
  let scaley = divf height valmax in
  (scalex, scaley, padding)

(* Histogramme classique *)
let simple ?(width = bp 100.) ?(height = bp 200.) ?padding
    ?(fill = [ lightblue ]) ?(perspective = false) ?hcaption ?vcaption
    ?histlabel ?vlabel ?hlabel l =
  let histlabel =
    match histlabel with
    | None -> None
    | Some (_, Values) as h -> h
    | Some (p, User l) -> Some (p, User (List.map (fun x -> [ x ]) l))
  in
  let valmax = max l in
  let nbval = List.length l in
  let scalex, scaley, padding =
    drawing_parameters width height ?padding nbval valmax 1
  in
  let cq = Q.of_list fill in
  let l = List.map (fun x -> [ x ]) l in
  hist ~cumul:false width height padding cq perspective scalex scaley ?histlabel
    ?hlabel 0 l
  ++ laxe ~nbcol:1 padding scalex scaley hcaption vcaption valmax nbval ?vlabel

(* Histogramme de comparaison *)
let compare ?(width = bp 100.) ?(height = bp 200.) ?padding
    ?(fill = [ lightblue; red ]) ?(perspective = false) ?hcaption ?vcaption
    ?histlabel ?vlabel ?hlabel l =
  let nblist = List.length l in
  let valmax = maxlist l in
  let nbval = List.fold_left (fun acc x -> List.length x + acc) 0 l in
  let scalex, scaley, padding =
    drawing_parameters width height ?padding nbval valmax nblist
  in
  let rec fct_hist bhlabel c cpt = function
    | [], _ -> c
    | x :: res, cq ->
        let col, rescol = Q.pop cq in
        let x = List.map (fun x -> [ x ]) x in
        if bhlabel then
          fct_hist false
            ( c
            ++ hist ?hlabel ?histlabel ~cumul:false width height
                 (addn padding (multn (bp (float_of_int (nblist - 1))) scalex))
                 (Q.push col Q.empty) perspective scalex scaley cpt x )
            (cpt + 1)
            (res, Q.push col rescol)
        else
          fct_hist false
            ( c
            ++ hist ?histlabel ~cumul:false width height
                 (addn padding (multn (bp (float_of_int (nblist - 1))) scalex))
                 (Q.push col Q.empty) perspective scalex scaley cpt x )
            (cpt + 1)
            (res, Q.push col rescol)
  in
  fct_hist true nop 0 (l, Q.of_list fill)
  ++ laxe ~nbcol:nblist padding scalex scaley hcaption vcaption valmax nbval
       ?vlabel

(* Histogramme cumulé *)
let stack ?(width = bp 100.) ?(height = bp 200.) ?padding
    ?(fill = [ lightblue; red; green ]) ?(perspective = false) ?hcaption
    ?vcaption ?histlabel ?vlabel ?hlabel l =
  let nblist = List.length l in
  let valmax = maxcumul l in
  let nbval = nblist in
  let scalex, scaley, padding =
    drawing_parameters width height ?padding nbval valmax 1
  in
  hist ~cumul:true width height padding (Q.of_list fill) perspective scalex
    scaley ?histlabel ?hlabel 0 l
  ++ laxe ~nbcol:1 padding scalex scaley hcaption vcaption valmax nbval ?vlabel
