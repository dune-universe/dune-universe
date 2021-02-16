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

open Mlpost
open Num
open Command
open Format
open Helpers
open Point
open Path
module T = Transform

let ( ++ ) x y = pt (cm x, cm y)

let shift x y = transform [ Transform.shifted (x ++ y) ]

let () = Random.init 1234

open Tree_adv

let rec bin = function
  | 0 -> Node (0, [])
  | n ->
      let (Node (_, l) as t) = bin (n - 1) in
      Node (n, t :: l)

let to_box n = (Box.tex (sprintf "${2^{%d}}$" n), n)

let t = map to_box (bin 4)

module Pl = struct
  type t = Box.t * int

  let width (z, _) = Box.width z

  let height (z, _) = Box.height z

  let set_pos p (z, n) = (Box.center p z, n)
end

module P = Tree_adv.Place (Pl)

let t = P.place t

let adv_fig =
  let ( ++ ) = Command.( ++ ) in
  draw fst t
  ++ fold ( ++ ) Command.nop
       (gen_draw_arrows Command.nop
          ~style:(fun a b -> Helpers.draw_simple_arrow a b)
          ~corner:(fun d (z, _) -> Box.corner d z)
          t)

open Tree
open Box

(* Bresenham (JCF) *)
(* the data to plot are computed here *)

let x2 = 9

let y2 = 6

let bresenham_data =
  let a = Array.create (x2 + 1) 0 in
  let y = ref 0 in
  let e = ref ((2 * y2) - x2) in
  for x = 0 to x2 do
    a.(x) <- !y;
    if !e < 0 then e := !e + (2 * y2)
    else (
      y := !y + 1;
      e := !e + (2 * (y2 - x2)) )
  done;
  a

(* drawing *)

let bresenham0 =
  let width = bp 6. and height = bp 6. in
  let g =
    Box.gridi (x2 + 1) (y2 + 1) (fun i j ->
        let fill =
          if bresenham_data.(i) = y2 - j then Some Color.red else None
        in
        Box.empty ~width ~height ?fill ~stroke:(Some Color.black) ())
  in
  Box.draw g

let block1 =
  let b1 =
    hblock
      ~min_width:(width (tex "c"))
      [ empty (); tex "A"; tex "B"; tex "c"; tex "toto" ]
  in
  let b2 =
    hblock ~same_width:true
      [ tex "A"; tex "B"; tex ~fill:Color.red "c"; tex "toto" ]
  in
  draw (vbox [ b1; b2 ])

let block2 = draw (hblock [ tex "A"; tex "B"; tex "c"; tex "toto" ])

let vblock1 = draw (vblock [ tex "A"; tex "B"; tex "c"; tex "toto" ])

let hbox1 = draw (hbox ~pos:`North [ tex "."; tex "B"; tex "c"; tex "toto" ])

let hbox2 =
  let s b = Box.shift (Point.p (100., 100.)) b in
  let stroke = Some Color.red in
  let b =
    vbox ~stroke ~pos:`West [ tex "A"; s (tex "Bx"); tex "c"; tex "toto" ]
  in
  let t = hbox ~stroke [ b; b; b ] in
  draw (vbox [ t; s t; t ])

let simple_box =
  Box.draw
    (Box.rect ~stroke:(Some Color.black)
       (Box.empty ~width:(bp 50.) ~height:(bp 50.) ()))

let hvbox =
  let row = vbox [ tex "A"; tex "B"; tex "C" ] in
  let col = hbox [ nth 0 row; tex "D"; tex "E" ] in
  seq [ draw row; draw col ]

let d1 =
  let a = circle (tex "$\\sqrt2$") in
  let b = shift (2. ++ 0.) (rect ~fill:Color.purple (tex "$\\pi$")) in
  let pen = Pen.scale (bp 3.) Pen.default in
  seq
    [
      draw a;
      draw b;
      Command.draw ~color:Color.red (Path.shift (1. ++ 1.) (bpath a));
      draw_label_arrow ~color:Color.orange ~pen ~pos:`Northeast
        (Picture.tex "foo") (west a) (south_east b);
      box_arrow ~color:Color.blue a b;
    ]

open Box

let d2 =
  let tex = tex ~stroke:(Some Color.black) in
  let b =
    hbox ~padding:(bp 10.) ~pos:`North ~stroke:(Some Color.red) ~dx:(bp 2.)
      ~dy:(bp 2.)
      [
        vbox ~padding:(bp 4.) ~pos:`East [ tex "A"; tex "BC"; tex "D" ];
        vbox ~padding:(bp 4.) ~pos:`West [ tex "E"; tex "FGH" ];
      ]
  in
  seq [ draw ~debug:false b; box_arrow (nth 1 (nth 0 b)) (nth 0 (nth 1 b)) ]

let proval =
  let f = 7. in
  let pen = Pen.rotate 40. (Pen.yscale (bp 0.5) Pen.square) in
  let check =
    jointpath
      [ (-1.2, 1.2); (0., -2.); (2., 2.); (5., 5.) ]
      [ jLine; jCurve; jCurve ]
  in
  seq
    [
      fill ~color:(Color.gray 0.2) (Path.scale (Num.bp f) fullcircle);
      label ~pos:`West (Picture.tex "Pr") (Point.p (f /. -4., 0.));
      label ~pos:`East (Picture.tex "al") (Point.p (f /. 4., 0.));
      Command.draw ~color:Color.green ~pen check;
    ]

open Tree

let yannick style =
  let tt s = Box.tex ~style ~fill:Color.orange ("\\texttt{" ^ s ^ "}") in
  let node s = node ~ls:(bp 20.) ~cs:(bp 10.) ~edge_style:Square (tt s) in
  let leaf s = leaf (tt s) in

  let tree =
    node "ComposerPage"
      [
        leaf "MemSet";
        node "ComposerMessages"
          [
            node "ComposerMsg" [ leaf "StrCpy"; leaf "DeclarerPanneRobustesse" ];
          ];
      ]
  in
  draw tree

let rec random_tree ?arrow_style ?edge_style ?stroke ?pen ?sep n =
  let random_tree = random_tree ?arrow_style ?edge_style ?stroke ?pen ?sep in
  let tex s = shadow (tex ~fill:Color.yellow ~stroke:(Some Color.black) s) in
  match n with
  | 1 -> leaf (tex "1")
  | 2 ->
      node ?arrow_style ?edge_style ?stroke ?pen ?sep
        (Box.tex ~style:Box.Rect ~fill:(Color.rgb 0.5 0.3 0.2) "2")
        [ leaf (tex "1") ]
  | n ->
      let k = 1 + Random.int (n - 2) in
      node ?arrow_style ?edge_style ?stroke ?pen ?sep
        (tex (string_of_int n))
        [ random_tree k; random_tree (n - 1 - k) ]

let d2c, d2s, d2sq, d2hsq =
  (*   let ls = bp (-1.0) in *)
  let stroke = Color.blue and pen = Pen.circle and arrow_style = Directed in
  ( draw
      (random_tree ~edge_style:Curve ~arrow_style ~stroke ~pen ~sep:(bp 5.) 17),
    draw
      (random_tree ~edge_style:Straight ~arrow_style ~stroke ~pen ~sep:(bp 3.)
         17),
    draw (random_tree ~edge_style:Square ~arrow_style ~stroke ~pen 17),
    draw (random_tree ~edge_style:HalfSquare ~arrow_style ~stroke ~pen 17) )

let d5 =
  let rand_tree name i =
    set_name name (set_stroke Color.black (to_box (random_tree i)))
  in
  let t1 = rand_tree "1" 5 in
  let t2 = rand_tree "2" 6 in
  let bl = Box.hbox ~padding:(Num.cm 2.) [ box t1; box t2 ] in
  let b1 = nth 0 (sub t1 bl) in
  let b2 = nth 0 (nth 0 (nth 1 (sub t2 bl))) in
  seq [ Box.draw bl; box_arrow ~sep:(bp 5.) b1 b2 ]

let tree1 () = pic (draw (random_tree (1 + Random.int 5)))

let rec random_tree2 = function
  | 1 -> leaf (tree1 ())
  | 2 -> node ~cs:(mm 0.2) (tree1 ()) [ leaf (tree1 ()) ]
  | n ->
      let k = 1 + Random.int (n - 2) in
      node ~cs:(mm 0.2) (tree1 ()) [ random_tree2 k; random_tree2 (n - 1 - k) ]

let d6 = draw (random_tree2 10)

let cheno011 =
  let p =
    Path.path ~cycle:jCurve [ (0., 0.); (30., 40.); (40., -20.); (10., 20.) ]
  in
  let pen = Pen.scale (bp 1.5) Pen.circle in
  seq
    [
      Command.draw p;
      seq
        (List.map
           (fun (pos, l, i) ->
             Command.dotlabel ~pos (Picture.tex l) (point i p))
           [
             (`South, "0", 0.);
             (`Northeast, "1", 1.);
             (`Southwest, "2", 2.);
             (`North, "3", 3.);
             (`West, "4", 4.);
           ]);
      Command.draw ~pen (subpath 1.3 3.2 p);
    ]

open Dash

let d3 =
  let p = pathp [ cmp (0., 0.); cmp (5., 0.) ] in
  let pat = pattern [ on (bp 6.); off (bp 12.); on (bp 6.) ] in
  Command.draw p ~dashed:pat

let d4 =
  seq
    [
      cheno011;
      iter 1 5 (fun i ->
          Picture.transform [ T.rotated (10. *. float i) ] cheno011);
    ]

let d7 =
  let pic =
    Picture.transform [ T.scaled (bp 4.) ] (Picture.tex "bound this!")
  in
  let pbox =
    pathp ~style:jLine ~cycle:jLine
      [
        Picture.ulcorner pic;
        Picture.urcorner pic;
        Picture.lrcorner pic;
        Picture.llcorner pic;
      ]
  in
  seq
    [
      pic;
      Command.draw (Picture.bbox pic);
      Command.draw pbox;
      Command.dotlabel ~pos:`West (Picture.tex "ulcorner")
        (Picture.ulcorner pic);
      Command.dotlabel ~pos:`West (Picture.tex "llcorner")
        (Picture.llcorner pic);
      Command.dotlabel ~pos:`East (Picture.tex "urcorner")
        (Picture.urcorner pic);
      Command.dotlabel ~pos:`East (Picture.tex "lrcorner")
        (Picture.lrcorner pic);
    ]

let half = Picture.scale (bp 0.5)

let rec right_split n pic =
  if n <= 0 then pic
  else
    let smaller = right_split (n - 1) (half pic) in
    Picture.beside pic (Picture.below smaller smaller)

let d11 =
  let p1 =
    Picture.transform [ Transform.rotated 90. ] (Picture.tex "recursion")
  in
  right_split 3 p1

let rec sierpinski p n =
  if n = 0 then p
  else
    let p = sierpinski p (n - 1) in
    (*     let p = half sp in *)
    let p1 = Picture.beside p p in
    Picture.below p p1

let d12 =
  let p1 = Picture.tex "A" in
  sierpinski p1 5

open Plot
(** plots *)

let sk = mk_skeleton 20 14 (Num.bp 20.) (Num.bp 20.)

let d13 = draw_grid sk

let squaref x = x *. x

let f2 i = sqrt (float_of_int i)

let f3 i = squaref (float_of_int i)

let d14 =
  let hdash _ = Dash.scaled 0.5 Dash.withdots in
  let vdash _ = Dash.scaled 2. Dash.evenly in
  let hvpen i =
    if i mod 5 = 0 then Pen.scale (bp 2.5) Pen.default else Pen.default
  in
  let pen = Pen.scale (bp 4.) Pen.default in
  seq
    [
      draw_grid ~hdash ~vdash ~hpen:hvpen ~vpen:hvpen sk;
      draw_func ~pen f2 sk;
      draw_func ~pen f3 sk;
    ]

let f1 i =
  let aux = function
    | 0 -> 1
    | 1 | 2 -> 2
    | 3 | 4 -> 3
    | 5 -> 4
    | 6 | 7 -> 5
    | 8 | 9 -> 6
    | 10 -> 7
    | 11 | 12 -> 8
    | 13 | 14 -> 9
    | 15 -> 10
    | 16 | 17 -> 11
    | 18 | 19 -> 12
    | 20 -> 13
    | _ -> 0
  in
  float_of_int (aux i)

let f2 i =
  let aux = function
    | 0 | 1 | 2 -> 0
    | 3 -> 1
    | 4 -> 2
    | 5 | 6 | 7 -> 3
    | 8 -> 4
    | 9 -> 5
    | 10 | 11 | 12 -> 6
    | 13 -> 7
    | 14 -> 8
    | 15 | 16 | 17 -> 9
    | 18 -> 10
    | 19 -> 11
    | 20 -> 12
    | _ -> 0
  in
  float_of_int (aux i)

let f3 i = float_of_int ((i + 3) / 5)

let flab i =
  ( Picture.transform
      [ Transform.scaled (bp 1.7) ]
      (Picture.tex (Printf.sprintf "$f_{\\omega_%d}$" i)),
    `North,
    19 )

let instants =
  let pen = Pen.scale (bp 2.5) Pen.default in
  let base =
    Command.draw ~pen (Path.path ~style:jLine [ (0., -65.); (280., -65.) ])
  in
  let tick i =
    let xi = float_of_int i *. 14. in
    let yi = if f1 i = f1 (i - 1) then -60. else -45. in
    let p = Path.path ~style:jLine [ (xi, -65.); (xi, yi) ] in
    Command.draw ~pen p
  in
  Command.seq
    [
      base;
      Command.iter 0 20 tick;
      Command.label
        (Picture.transform [ Transform.scaled two ] (Picture.tex "$\\omega_1$"))
        (p (-20., -55.));
    ]

let florence =
  let sk = mk_skeleton 20 14 (bp 14.) (bp 20.) in
  let pen = Pen.scale (bp 4.) Pen.default in
  let pen2 = Pen.scale (bp 3.) Pen.default in
  let dash _ = Dash.scaled 0.5 Dash.withdots in
  let dash2 = Dash.scaled 0.66 Dash.withdots in
  let dash3 = Dash.scaled 0.9 Dash.evenly in
  let vcaption, hcaption =
    let tr = [ Transform.scaled (bp 1.5) ] in
    ( Picture.transform tr (Picture.tex "\\textsf{Number of ones}"),
      Picture.transform tr (Picture.tex "\\textsf{Instants}") )
  in
  let plot = draw_func ~drawing:Stepwise ~style:jLine in
  seq
    [
      draw_grid ~hdash:dash ~vdash:dash ~color:(Color.gray 0.5) sk;
      draw_axes ~closed:true ~hcaption ~vcaption sk;
      plot ~pen ~label:(flab 1) f1 sk;
      plot ~pen:pen2 ~dashed:dash2 ~label:(flab 2) f2 sk;
      plot ~pen ~dashed:dash3 ~label:(flab 3) f3 sk;
      instants;
    ]

let shapes1 =
  Box.vbox
    [
      Box.path (Shapes.rectangle (bp 10.) (bp 20.));
      Box.path (Shapes.rectangle (bp 35.) (bp 15.));
      Box.path (Shapes.rectangle (bp 15.) (bp 35.));
      Box.path (Shapes.round_rect (bp 55.) (bp 25.) (bp 10.) (bp 10.));
      Box.path (Shapes.round_rect (bp 55.) (bp 25.) (bp 20.) (bp 5.));
      Box.path (Shapes.round_rect (bp 70.) (bp 25.) (bp 14.) (bp 14.));
    ]

let shapes2 =
  Box.vbox
    [
      (*
      Shapes.arc_ellipse (f 10.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse ~stroke:Color.red (f 30.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse ~stroke:Color.red ~close:true (f 30.) (f 10.) 0. 1.7;
      Shapes.arc_ellipse
	~fill:Color.black ~stroke:Color.red (f 30.) (f 10.) 0. 1.7;
*)
      Box.path (Shapes.ellipse (bp 10.) (bp 10.));
      Box.path (Shapes.ellipse (bp 30.) (bp 10.));
      Box.path (Shapes.ellipse (bp 30.) (bp 10.));
    ]

let farey n =
  let u x = Num.bp (200.0 *. x) in
  let circle x y r =
    Command.fill ~color:Color.lightgray
      (Path.shift (Point.pt (u y, u x)) (Path.scale (u (2. *. r)) fullcircle))
  in
  let quartercircle x y r theta =
    Command.draw
      (Path.shift
         (Point.pt (u y, u x))
         (Path.scale (u (2. *. r)) (Path.rotate theta quartercircle)))
  in
  let rec aux acc p1 q1 p2 q2 =
    let p = p1 + p2 in
    let q = q1 + q2 in
    if q > n then acc
    else
      let fq = float q in
      let fr = 0.5 /. fq /. fq in
      let acc = circle (float p /. fq) fr fr :: acc in
      let acc = aux acc p1 q1 p q in
      aux acc p q p2 q2
  in
  let l =
    aux
      [ quartercircle 0.0 0.5 0.5 90.0; quartercircle 1.0 0.5 0.5 180.0 ]
      0 1 1 1
  in
  Picture.scale (Num.bp 30.0) (Command.seq l)

let why_platform =
  let tabular l =
    "{\\begin{tabular}{l}" ^ String.concat " \\\\ " l ^ "\\end{tabular}}"
  in
  let dx = bp 5. and dy = bp 5. in
  let space ~name b = rect ~stroke:None ~name ~dx ~dy b in
  let green s =
    space ~name:s
      (round_rect ~dx ~dy ~stroke:None ~fill:Color.lightgreen (tex s))
  in
  let pink s =
    space ~name:s
      (shadow
         (rect ~dx ~dy ~fill:(Color.color "light pink")
            (tex ("\\large\\sf " ^ s))))
  in
  let interactive =
    tex ~name:"interactive"
      (tabular [ "Interactive provers"; "(Coq, PVS,"; "Isabelle/HOL, etc.)" ])
  in
  let automatic =
    tex ~name:"automatic"
      (tabular
         [
           "Automatic provers"; "(Alt-Ergo, Simplify,"; "Yices, Z3, CVC3, etc.)";
         ])
  in
  let b =
    tabularl ~hpadding:(bp 20.) ~vpadding:(bp 30.)
      [
        [
          green "Annotated C programs";
          empty ();
          green "JML-annotated Java programs";
        ];
        [ pink "Caduceus"; green "Why program"; pink "Krakatoa" ];
        [ empty (); pink "Why"; empty () ];
        [ interactive; green "verification conditions"; automatic ];
      ]
  in
  let arrow x y =
    let p = Box.cpath (get x b) (get y b) in
    Arrow.draw_thick ~line_color:Color.red ~width:(bp 4.) ~head_width:(bp 10.)
      ~fill_color:Color.red (Path.point 0. p) (Path.point 1. p)
  in
  seq
    [
      Box.draw b;
      arrow "Annotated C programs" "Caduceus";
      arrow "Caduceus" "Why program";
      arrow "JML-annotated Java programs" "Krakatoa";
      arrow "Krakatoa" "Why program";
      arrow "Why program" "Why";
      arrow "Why" "verification conditions";
      arrow "verification conditions" "interactive";
      arrow "verification conditions" "automatic";
    ]

(***
let alt_ergo =
  let b =
    tabularl ~hpadding:(bp 20.) ~vpadding:(bp 30.)
      [[green "Annotated C programs"; empty ();
	green "JML-annotated Java programs"];
       [pink "Caduceus"; green "Why program"; pink "Krakatoa";];
       [empty (); pink "Why"; empty ()];
       [interactive; green "verification conditions"; automatic]]
  in
  [Box.draw b]
***)

let rotatedbox =
  let t = tex "$A^{-1}$" in
  let b1 = Box.rotate 90. t in
  Box.draw (Box.hblock [ b1; t ])

let style = RoundRect

let stroke = Some Color.black

let pen = Pen.scale (bp 2.) Pen.circle

let dx = bp 5.

let dy = dx

let tex = Box.tex ~style ~pen ~dx ~dy

let tex' = Box.tex ~style ~pen ~dx ~dy:(bp 10.)

let assia_schema =
  let tabular l =
    "{\\begin{tabular}{l}" ^ String.concat " \\\\ " l ^ "\\end{tabular}}"
  in
  let lang =
    tex ~stroke:(Some Color.red) "langage de developpement de preuves"
  in
  let genie = Box.tex "Genie logiciel formel" in
  let moteur =
    tex' ~stroke:(Some Color.purple) (tabular [ "moteur de"; "dev de preuves" ])
  in
  let verif =
    tex' ~stroke:(Some Color.purple) (tabular [ "verificateur"; " de preuves" ])
  in
  let langf =
    Box.round_rect ~stroke:(Some Color.blue) ~pen ~dx:(bp 50.) ~dy:(bp 10.)
      (Box.tex "langage formel")
  in
  let h = Box.hbox ~padding:(bp 20.) [ moteur; verif ] in
  let v =
    Box.vbox ~dx ~dy:(bp 10.) ~pen ~padding:(bp 5.) ~style
      ~stroke:(Some Color.orange) [ lang; genie ]
  in
  Box.draw (Box.vbox ~padding:(bp (-5.)) [ langf; h; v ])

let grid_with_padding =
  let red s = rect ~stroke:None ~fill:Color.lightred (tex s) in
  let blue s = rect ~stroke:None ~fill:Color.lightblue (tex s) in
  let b =
    gridl ~stroke:None ~hpadding:(bp 5.) ~vpadding:(bp 5.)
      [
        [ empty (); red "abc"; red "def" ];
        [ blue "titre 1"; red ""; red "" ];
        [ blue "titre 2"; red ""; red "" ];
      ]
  in
  Box.draw b

let grid_with_padding_2 =
  let red s = rect ~stroke:None ~fill:Color.lightred (tex s) in
  let blue s = rect ~stroke:None ~fill:Color.lightblue (tex s) in
  let pen = Pen.scale (Num.pt 1.5) Pen.circle in
  let b =
    gridl ~stroke:(Some Color.white) ~pen ~hpadding:(bp 5.) ~vpadding:(bp 5.)
      [
        [ empty (); red "abc"; red "def" ];
        [ blue "titre 1"; red ""; red "" ];
        [ blue "titre 2"; red ""; red "" ];
      ]
  in
  seq [ Box.draw b; Box.draw (shift (Point.pt (bp 5., bp 5.)) b) ]

module Tr = Triangle
open Color
open Command

let t40 = Tr.create ~left:0.5 ~depth:(bp 40.) ()

let x =
  let t = Tr.create ~fill:white ~depth:(bp 13.) () in
  Tr.tex_root_label "$x$" t

let anchor1 =
  let t1 = Tr.tex_label ~depth:0.5 "$\\sigma$" t40 in
  let t2 = Tr.anchor ~depth:1.0 ~x:0.3 t1 x in
  let t3 = Tr.anchor ~depth:1.0 ~x:0.7 t1 x in
  Tr.draw t1 ++ Tr.draw t2 ++ Tr.draw t3

let pose =
  let t1 = Tr.pose_left ~x:0.2 t40 x in
  let t2 = Tr.pose_right ~x:0.8 t40 x in
  let t3 =
    Tr.pose_right ~x:0.95 ~depth:1.2 t40
      (Tr.create ~fill:lightred ~depth:13. ())
  in
  Tr.draw t40 ++ Tr.draw t1 ++ Tr.draw t2 ++ Tr.draw t3

let redex1 =
  let t1 = Tr.create ~left:0.5 ~depth:(bp 40.) () in
  let t1 = Tr.tex_root_label "$t$" t1 in
  let t = Tr.tex ~dx:2. ~dy:2. ~fill:lightred "$r$" in
  let t = Tr.tex_root_label "\\footnotesize$p$" t in
  let t = Tr.anchor ~depth:0.8 ~x:0.3 t1 t in
  Tr.draw t1 ++ Tr.draw t

let label1 =
  let t = Tr.create ~left:0.5 ~depth:(bp 40.) () in
  let t = Tr.tex_label "$\\sigma t$" t in
  Tr.draw t

let pic1 =
  let t = Tr.tex ~pen ~fill:lightred "TOT" in
  Tr.draw t

let x_depth =
  let t = Tr.create ~left:20. ~right:60. ~depth:70. () in
  let r = Tr.root t in
  let depth = 0.7 in
  let x0 = Tr.x_depth ~depth ~x:0. t in
  let x1 = Tr.x_depth ~depth ~x:1. t in
  let x = Tr.x_depth ~depth ~x:0.7 t in
  let p01 =
    Path.pathp [ Tr.x_depth ~depth ~x:(-0.5) t; Tr.x_depth ~depth ~x:1.5 t ]
  in
  let p = Point.add r (Point.mult 1.8 (Point.sub x r)) in
  let pd = Path.pathp [ r; p ] in
  let b = Tr.x_depth ~depth:1. ~x:0.7 t in
  Tr.draw t
  ++ Command.dotlabel ~pos:`Topleft (Picture.tex "0") x0
  ++ Command.dotlabel ~pos:`Topright (Picture.tex "1") x1
  ++ Command.dotlabel ~pos:`Bottomleft (Picture.tex "x") x
  ++ Command.dotlabel ~pos:`Topleft (Picture.tex "0") r
  ++ Command.dotlabel ~pos:`Bottomleft (Picture.tex "1") b
  ++ Command.draw ~color:purple p01
  ++ Command.draw ~color:red pd ++ nop

let figs =
  [
    (*
  grid_with_padding;
  adv_fig;
  grid_with_padding_2;
  rotatedbox;
  assia_schema;
  hbox1;
  hbox2;
  bresenham0;
  simple_box;
  block1;
  hvbox;
  why_platform;d2;
  block2;
  vblock1;
  yannick Box.Rect; yannick Box.Patatoid; yannick Box.Patatoid2; d1;
  d2sq; d2hsq; d2s; d2c;
  cheno011; d3; d4;
  d7;
  proval;
*)
    d11;
    d12;
    d14;
    d13;
    d5;
    florence;
    Box.draw shapes1;
    Box.draw shapes2;
    farey 17;
    (* other *)
    d6;
    Box.draw t40;
    anchor1;
    pose;
    redex1;
    label1;
    pic1;
    x_depth;
  ]

let figs =
  let r = ref 0 in
  List.map
    (fun f ->
      incr r;
      (!r, f))
    figs

(* CM fonts do not scale well *)

(*
let theprelude = "\\documentclass[a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{times}
"
*)

let () = List.iter (fun (i, r) -> Metapost.emit (string_of_int i) r) figs
