(* figures pour l'article JFLA *)

open Mlpost
open Command
open Picture
open Path
open Helpers
open Num
open Num.Infix
open Point
open Box

let draw_arrow = Arrow.simple

(* Stéphane *)
(** the old ugly version *)

(* let graph_sqrt = *)
(*   let u = Num.cm in *)
(*   let pen = Pen.circle ~tr:[Transform.scaled one] () in  *)
(*   let rec pg = function *)
(*     | 0 -> start (knot ~r:(vec up) ~scale:u (0.,0.)) *)
(*     | n -> let f = (float_of_int n /. 2.) in  *)
(* 	concat ~style:jCurve (pg (n-1)) (knot ~scale:u (f, sqrt f))  *)
(*   in *)
(*   [draw (pathn ~style:jLine [(zero,u 2.); (zero,zero); (u 4.,zero)]); *)
(*    draw ~pen (pg 8); *)
(*    label ~pos:`Lowright (tex "$ \\sqrt x$") (pt (u 3., u (sqrt 3.))); *)
(*    label ~pos:`Bot (tex "$x$") (pt (u 2., zero)); *)
(*    label ~pos:`Lowleft (tex "$y$") (pt (zero, u 1.))] *)

(** the new short one :) *)
let graph_sqrt =
  let u = cm 1. in
  let sk = Plot.mk_skeleton 4 3 u u in
  let label = (Picture.tex "$y=\\sqrt{x+\\frac{1}{2}}$", `Upleft, 3) in
  let graph = Plot.draw_func ~label (fun x -> sqrt (float x +. 0.5)) sk in
  seq [ graph; Plot.draw_simple_axes "$x$" "$y$" sk ]

let architecture =
  let mk_box fill name m =
    let m = "{\\tt " ^ m ^ "}" in
    Box.tex ~stroke:(Some Color.black) ~style:RoundRect ~dx:(bp 5.) ~dy:(bp 5.)
      ~name ~fill m
  in
  let mk_unbox name m =
    Box.tex ~style:RoundRect ~stroke:None ~dx:(bp 5.) ~dy:(bp 5.) ~name m
  in
  (* les types de base *)
  let fill = Color.color "salmon" in
  let num = mk_box fill "num" "Num" in
  let point = mk_box fill "point" "Point" in
  let path = mk_box fill "path" "Path" in
  let dots = mk_unbox "dots" "$\\ldots$" in
  let cmd = mk_box fill "cmd" "Command" in
  let basictypes = Box.hbox ~padding:(mm 2.) [ num; point; path; dots; cmd ] in
  (* compile *)
  let compile = mk_unbox "compile" "\\tt Compile" in
  let compile_ext =
    let dx = (Box.width basictypes -/ Box.width compile) /./ 2. in
    Box.hbox ~style:RoundRect ~dx ~fill ~stroke:(Some Color.black) [ compile ]
  in
  (* metapost *)
  let metapost = mk_unbox "metapost" "\\metapost" in
  let metapost_ext =
    let dx = (Box.width basictypes -/ Box.width metapost) /./ 2. in
    Box.hbox ~name:"mpost_ext" ~style:Rect ~dx ~stroke:(Some Color.black)
      [ metapost ]
  in
  (* composants avancés *)
  let fill = Color.color "pink" in
  let box_ = mk_box fill "box" "\\phantom{p}Box\\phantom{p}" in
  let shapes = mk_box fill "shapes" "\\phantom{p}Shapes\\phantom{p}" in
  let arrows = mk_box fill "arrow" "\\phantom{p}Arrow\\phantom{p}" in
  let advanced = Box.hbox ~pos:`Bot ~padding:(mm 2.) [ box_; shapes; arrows ] in
  (* extensions *)
  let fill = Color.color "blanched almond" in
  let tree = mk_box fill "tree" "\\phantom{g}Tree\\phantom{g}" in
  let diag = mk_box fill "diag" "\\phantom{g}Diag\\phantom{g}" in
  let plot = mk_box fill "plot" "\\phantom{g}Plot\\phantom{g}" in
  let extensions = Box.hbox ~pos:`Bot ~padding:(mm 2.) [ tree; diag; plot ] in
  (* wrapping *)
  let pyramid =
    let pen = Pen.scale (Num.bp 1.0) Pen.square in
    Box.vbox ~padding:(mm 2.) ~pen ~dx:(bp 5.) ~dy:(bp 5.) ~style:RoundRect
      ~stroke:(Some Color.black)
      [ extensions; advanced; basictypes; compile_ext; metapost_ext ]
  in
  let mlpost = mk_unbox "mlpost" "\\tt Mlpost" in
  let mlpost_ext =
    let dx = (Box.width pyramid -/ Box.width mlpost) /./ 2. in
    Box.hbox ~dx [ mlpost ]
  in
  let full = Box.vbox ~padding:(mm (-1.)) [ mlpost_ext; pyramid ] in
  let _ = Box.set_stroke Color.black (Box.nth 1 full) in
  (* arrows *)
  let arrows =
    let mp = Box.get "mpost_ext" full in
    List.map
      (fun n ->
        Helpers.box_label_arrow ~outd:(Path.vec Point.down) ~pos:`Bot
          (Picture.make Command.nop) (Box.get n full) mp)
      (* une variante *)
      (*     let mp = Box.get "metapost" full in *)
      (*       List.map  *)
      (* 	(fun n -> Helpers.box_arrow  *)
      (* 	   (Box.get n full) mp) *)
      [ "num"; "point"; "path"; "dots"; "cmd" ]
  in
  seq [ seq arrows; Box.draw full ]

(* Romain *)
open Num

let state = Box.tex ~style:Circle ~stroke:(Some Color.black)

let final = Box.box ~style:Circle

let transition states tex anchor ?outd ?ind x_name y_name =
  let x = Box.get x_name states and y = Box.get y_name states in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  Arrow.draw ~tex ~anchor (cpath ?outd ?ind x y)

(*let loop box =
  let c = Box.ctr box in
  let a = Point.shift c (pt (cm 0., cm (-0.8))) in
  let p = Path.pathk [
    knotp ~r: (vec (dir 225.)) c;
    knotp a;
    knotp ~l: (vec (dir 135.)) c;
  ] in
  let bp = Box.bpath box in
  cut_after bp (cut_before bp p)*)

(*
let loop states tex pos name =
  let box = Box.get name states in
  let fdir, angle, x, y = match pos with
    | `Top -> Box.north, 0., 0., 0.4
    | `Left -> Box.west, 90., (-0.4), 0.
    | `Bot -> Box.south, 180., 0., (-0.4)
    | `Right -> Box.north, 270., 0.4, 0.
  in
  let a = Point.shift (fdir box) (Point.pt (cm x, cm y)) in
  let c = Box.ctr box in
  let p = Path.pathk [
    knotp ~r: (vec (dir (angle +. 45.))) c;
    knotp a;
    knotp ~l: (vec (dir (angle -. 45.))) c;
  ] in
  let bp = Box.bpath box in
  Arrow.draw ~tex ~pos: (pos :> Command.position)
    (cut_after bp (cut_before bp p))
*)
let loop states tex name =
  let box = Box.get name states in
  let a = Point.shift (Box.south box) (Point.pt (cm 0., cm (-0.4))) in
  let c = Box.ctr box in
  let p =
    Path.pathk
      [ knotp ~r:(vec (dir 225.)) c; knotp a; knotp ~l:(vec (dir 135.)) c ]
  in
  let bp = Box.bpath box in
  Arrow.draw ~tex ~anchor:`Bot (cut_after bp (cut_before bp p))

let arrow_loop_explain_kind =
  Arrow.add_belt ~point:0.9
    (Arrow.add_line ~dashed:Dash.evenly ~to_point:0.1
       (Arrow.add_line ~dashed:Dash.evenly ~from_point:0.9
          (Arrow.add_line ~from_point:0.1 ~to_point:0.9 Arrow.empty)))

let loop_explain =
  let construct_pattern = Dash.pattern [ Dash.on (bp 0.2); Dash.off (bp 1.) ] in
  let arc_arrow =
    Arrow.add_head
      ~head:(Arrow.head_classic ~dashed:construct_pattern)
      (Arrow.add_line ~dashed:construct_pattern Arrow.empty)
  in
  let s = state "~~~~~~~~~~~" in
  let pt x y = Point.pt (cm x, cm y) in
  let p x y = Point.shift (Box.ctr s) (pt x y) in
  let a_pos = p 0. (-2.) in
  let angle = 180. in
  let c = Box.ctr s in
  let arrow_path =
    Path.pathk
      [
        knotp ~r:(vec (dir (angle +. 45.))) c;
        knotp a_pos;
        knotp ~l:(vec (dir (angle -. 45.))) c;
      ]
  in
  let vert = Path.pathk [ knotp (p 0. 2.); knotp (p 0. (-3.)) ] in
  let len = 1.2 in
  let diag1 = Path.pathk [ knotp (p len len); knotp (p (-.len) (-.len)) ] in
  let diag2 = Path.pathk [ knotp (p len (-.len)); knotp (p (-.len) len) ] in
  let construct = Command.draw ~dashed:construct_pattern in
  let circle = Path.shift (pt 0.64 0.) (Path.scale (cm 2.) Path.fullcircle) in
  let arc = cut_before vert (cut_after diag2 (cut_after vert circle)) in
  seq
    [
      Box.draw s;
      Command.dotlabel ~pos:`Lowleft (Picture.tex "$A$") a_pos;
      Arrow.draw ~kind:arrow_loop_explain_kind arrow_path;
      construct vert;
      construct diag1;
      construct diag2;
      Arrow.draw ~tex:"$45^{\\circ}$" ~anchor:`Upleft ~kind:arc_arrow arc;
    ]

(*
let initial states pos name =
  let x = Box.get name states in
  let p = match pos with
    | `Left -> Box.west x
    | `Right -> Box.east x
    | `Top -> Box.north x
    | `Bot -> Box.south x
  in
  Arrow.draw (Path.pathp [ Point.shift p (Point.pt (cm (-0.3), zero)); p ])
*)
let initial (states : Box.t) (name : string) : Command.t =
  let b = Box.get name states in
  let p = Box.west b in
  Arrow.draw (Path.pathp [ Point.shift p (Point.pt (cm (-0.3), zero)); p ])

let automate_1 =
  let states =
    Box.vbox ~padding:(cm 0.8)
      [
        Box.hbox ~padding:(cm 1.4)
          [ state ~name:"alpha" "$\\alpha$"; state "$\\beta$" ];
        final (state "$\\gamma$");
      ]
  in
  Box.draw states

let automate =
  let states =
    Box.vbox ~padding:(cm 0.8)
      [
        Box.hbox ~padding:(cm 1.4)
          [ state ~name:"alpha" "$\\alpha$"; state ~name:"beta" "$\\beta$" ];
        final ~name:"gamma" (state "$\\gamma$");
      ]
  in
  seq
    [
      Box.draw states;
      transition states "a" `Lowleft "alpha" "gamma";
      transition states "b" `Lowright "gamma" "beta";
      transition states "c" `Top ~outd:25. ~ind:335. "alpha" "beta";
      transition states "d" `Bot ~outd:205. ~ind:155. "beta" "alpha";
      loop states "e" "gamma";
      initial states "alpha";
    ]

let arrow_metapost =
  seq
    [
      Helpers.draw_simple_arrow
        ~outd:(vec (dir 90.))
        ~ind:(vec (dir 90.))
        (Point.pt (cm 0., cm 0.5))
        (Point.pt (cm 2., cm 0.5));
      Helpers.draw_simple_arrow
        (Point.pt (cm 4., cm 0.5))
        (Point.pt (cm 6., cm 0.5));
      Helpers.draw_simple_arrow ~dashed:Dash.evenly
        ~outd:(vec (dir 90.))
        (Point.pt (cm 8., cm 0.))
        (Point.pt (cm 10., cm 0.));
      Helpers.draw_simple_arrow
        ~pen:(Pen.scale (bp 2.5) Pen.square)
        (Point.pt (cm 12., cm 0.))
        (Point.pt (cm 14., cm 1.));
    ]

let arrow_demo_path =
  Path.pathp [ Point.pt (zero, zero); Point.pt (cm 2., zero) ]

let arrow_simple = Arrow.draw ~kind:Arrow.triangle_full arrow_demo_path

let arrow_loop_explain =
  let pt x y = Point.pt (cm x, cm y) in
  let draw2 = Arrow.point_to_point ~kind:arrow_loop_explain_kind in
  seq
    [
      draw2 ~outd:(vec (dir 45.)) ~ind:(vec (dir 45.)) (pt 0. 0.25) (pt 3. 0.25);
      draw2 (pt 5. 0.25) (pt 8. 0.25);
      draw2 ~outd:(vec (dir 45.)) (pt 10. 0.) (pt 13. 0.);
    ]

(* Johannes *)
open Box

let uml_client, uml =
  let classblock name attr_list method_list =
    let vbox = Box.vbox ~pos:`Left in
    Box.vblock ~pos:`Left ~name
      [
        tex ("{\\bf " ^ name ^ "}");
        vbox (List.map tex attr_list);
        vbox (List.map tex method_list);
      ]
  in
  let a =
    classblock "BankAccount"
      [ "balance : Dollars = $0$" ]
      [ "deposit (amount : Dollars)"; "withdraw (amount : Dollars)" ]
  in
  let b = classblock "Client" [ "name : String"; "address : String" ] [] in
  let diag = Box.vbox ~padding:(cm 1.) [ a; b ] in
  ( Box.draw b,
    seq
      [
        Box.draw diag;
        box_label_arrow ~pos:`Left (Picture.tex "owns") (get "Client" diag)
          (get "BankAccount" diag);
      ] )

open Tree

let sharing =
  let tex s = tex ~name:s ~style:Circle ~dx:two ~dy:two s in
  let tree =
    bin (tex "a")
      (bin (tex "b")
         (leaf (tex "c"))
         (bin (tex "d") (leaf (tex "e")) (leaf (tex "f"))))
      (node (tex "g") [ leaf (tex "h") ])
  in
  let tree = to_box tree in
  seq
    [
      Box.draw tree;
      box_arrow (get "h" tree) (get "f" tree);
      box_arrow (get "g" tree) (get "d" tree);
    ]

let arrowpic =
  pic ~stroke:None
    (Picture.make (draw_arrow (Path.path ~scale:cm [ (0., 0.); (0., -1.) ])))

let harrowpic =
  pic ~stroke:None ~name:"harrow"
    (Picture.make (draw_arrow (Path.path ~scale:cm [ (0., 0.); (1., 0.) ])))

let texttt s = tex ("{\\tt " ^ s ^ "}")

let sharingcompile =
  let code =
    Box.vbox ~pos:`Left
      (List.map texttt [ "path f = ...;"; "path d = ...e...f...;"; "..." ])
  in
  let b =
    Box.hbox ~padding:(cm 2.)
      (List.map
         (fun (b, name) -> Box.box ~dx:(cm 0.5) ~name ~stroke:None b)
         [
           (Box.pic ~stroke:None (Picture.make sharing), "tree"); (code, "code");
         ])
  in
  let arrow x y = box_arrow (get x b) (get y b) in
  seq [ Box.draw b; arrow "tree" "code" ]

let stages =
  let dx = bp 5. and dy = bp 5. in
  let tex' = tex ~style:RoundRect ~dx ~dy in
  let tex = tex' ~stroke:(Some Color.black) in
  let box name = box ~stroke:None ~dx:(mm 2.) ~name in
  let fml = box "fml" (tex "figure.ml") in
  let fmp = box "fmp" (tex "figure.mp") in
  let ps =
    box "ps"
      (vbox ~stroke:(Some Color.black) ~style:RoundRect ~dx ~dy
         [ tex' "figure.1"; tex' "(\\postscript)" ])
  in
  let all = hbox ~padding:(cm 3.5) [ fml; fmp; ps ] in
  seq
    [
      Box.draw all;
      box_labelbox_arrow ~pos:`Top
        (vbox [ tex "\\ocaml"; arrowpic; tex' "compiler \\& ex\\'ecuter" ])
        (get "fml" all) (get "fmp" all);
      box_labelbox_arrow ~pos:`Top
        (vbox [ tex "\\metapost"; arrowpic; tex' "interpr\\'eter" ])
        (get "fmp" all) (get "ps" all);
    ]

let simple =
  seq
    [
      Box.draw (tex "\\LaTeX");
      Box.draw (shift (Point.pt (cm 1., zero)) (circle (empty ())));
    ]

let align =
  seq [ Box.draw (hbox ~padding:(cm 1.) [ tex "\\LaTeX"; circle (empty ()) ]) ]

let persistance =
  let b = hbox ~padding:(cm 1.) [ tex "\\LaTeX"; circle (empty ()) ] in
  Box.draw (vbox [ b; set_stroke Color.black b; b ])

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

let stroke = Some Color.black

let width = bp 10.

and height = bp 10.

let bresenham0 =
  let g =
    Box.gridi (x2 + 1) (y2 + 1) (fun i j ->
        let fill =
          if bresenham_data.(i) = y2 - j then Some Color.red else None
        in
        Box.empty ~width ~height ?fill ~stroke ())
  in
  Box.draw g

let bresenham =
  let g =
    Box.gridi (x2 + 1) (y2 + 1) (fun i j ->
        let fill =
          if bresenham_data.(i) = y2 - j then Some Color.red else None
        in
        Box.empty ~width ~height ?fill ~stroke ())
  in
  let get i j = Box.nth i (Box.nth (y2 - j) g) in
  let label pos s point i j =
    Command.label ~pos (Picture.tex s) (point (get i j))
  in
  seq
    [
      Box.draw g;
      label `Bot "0" Box.south 0 0;
      label `Bot "$x_2$" Box.south x2 0;
      label `Left "0" Box.west 0 0;
      label `Left "$y_2$" Box.west 0 y2;
    ]

(* cercles de Ford (merci Claude) *)

let ford n =
  let u x = Num.bp (200.0 *. x) in
  let circle x y r =
    Command.draw ~color:Color.black
      (Path.shift (Point.pt (u x, u y)) (Path.scale (u (2. *. r)) fullcircle))
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
  let l = aux [] 0 1 1 1 in
  let pic = Picture.make (Command.seq l) in
  Picture.scale (Num.bp 30.0) pic

(* blocks mémoire *)

let simple_block =
  let b = Box.hblock ~pos:`Bot [ Box.tex "a"; Box.tex "b"; Box.tex "C" ] in
  Box.draw b

let pointer_arrow a b =
  let p = pathp [ Box.ctr a; Box.ctr b ] in
  let p = Path.cut_after (Box.bpath b) p in
  let pen = Pen.scale (Num.bp 4.) Pen.circle in
  Command.draw ~pen (pathp [ Box.ctr a ]) ++ draw_arrow p

let block_arrow =
  let a =
    Box.empty ~width:(bp 10.) ~height:(bp 10.) ~stroke:(Some Color.black) ()
  in
  let g = Box.hbox ~padding:(cm 1.) [ a; a ] in
  seq [ Box.draw g; pointer_arrow (Box.nth 0 g) (Box.nth 1 g) ]

let cons hd tl =
  let p1 = tex ~name:"hd" hd in
  let p2 =
    if tl then empty ~name:"tl" ~width:(bp 10.) ()
    else tex ~name:"tl" "\\ensuremath{\\bot}"
  in
  hblock [ p1; p2 ]

let draw_list l =
  let rec make = function
    | [] -> assert false
    | [ x ] -> [ cons x false ]
    | x :: l -> cons x true :: make l
  in
  let l = hbox ~padding:(Num.bp 30.) (make l) in
  let rec arrows = function
    | [] | [ _ ] -> nop
    | b1 :: (b2 :: _ as l) ->
        pointer_arrow (Box.get "tl" b1) (Box.get "hd" b2) ++ arrows l
  in
  seq [ Box.draw l; arrows (Array.to_list (Box.elts l)) ]

let list123 = draw_list [ "1"; "2"; "3" ]

let another_list =
  draw_list (List.map (fun n -> Printf.sprintf "$\\sqrt{%d}$" n) [ 1; 2; 3; 4 ])

let deps =
  let node s =
    Box.round_rect ~name:s ~dx:zero
      (Box.tex ("{\\tt\\phantom{p}" ^ s ^ "\\phantom{p}}"))
  in
  let ellipsis = Box.tex ~name:"..." "\\phantom{p}\\dots\\phantom{lp}" in
  let b =
    vbox ~padding:(bp 30.)
      [
        node "mlpost.mli";
        hbox ~padding:(bp 25.)
          [ node "num.ml"; node "point.ml"; ellipsis; node "path.ml" ];
        node "types.mli";
      ]
  in
  let arrow b1 b2 = box_arrow (Box.get b1 b) (Box.get b2 b) in
  seq
    [
      Box.draw b;
      iterl
        (fun s -> arrow "mlpost.mli" s ++ arrow s "types.mli")
        [ "num.ml"; "point.ml"; "path.ml"; "..." ];
    ]

let sous_typage =
  let tt s = Box.tex ~name:s ("\\tt `" ^ s) in
  let b =
    tabularl ~hpadding:(bp 10.) ~vpadding:(bp 10.)
      [
        [ tt "Upleft"; tt "Top"; tt "Upright" ];
        [ tt "Left"; tt "Center"; tt "Right" ];
        [ tt "Lowleft"; tt "Bot"; tt "Lowright" ];
      ]
  in
  let group l color =
    let bl = Box.group ~style:RoundRect (List.map (fun s -> Box.get s b) l) in
    Command.draw ~color (bpath bl)
  in
  seq
    [
      group [ "Left"; "Center"; "Right" ] Color.red;
      group [ "Top"; "Center"; "Bot" ] Color.blue;
      Box.draw b;
    ]

let texttt x = "\\texttt{" ^ x ^ "}"

let circularity =
  let box x =
    Box.tex ~name:x ~style:Box.RoundRect ~stroke:(Some Color.orange)
      ~fill:Color.yellow ~dx:(Num.cm 0.1) ~dy:(Num.cm 0.1) x
  in
  let dir x = Path.vec (Point.dir x) in
  let transform = box "Transform" in
  let point = box "Point" in
  let picture = box "Picture" in
  let command = box "Command" in
  let circ x y = Box.vbox ~padding:(Num.cm 0.75) [ x; y ] in
  let hbox =
    Box.hbox ~padding:(Num.cm 2.5)
      [ circ transform point; circ picture command ]
  in
  let arrow_down x y tex =
    let x = Box.get x hbox in
    let y = Box.get y hbox in
    let tex = texttt tex in
    Arrow.draw ~tex ~anchor:`Left (Box.cpath ~outd:(dir 225.) x y)
  in
  let arrow_up x y tex =
    let x = Box.get x hbox in
    let y = Box.get y hbox in
    let tex = texttt tex in
    Arrow.draw ~tex ~anchor:`Right (Box.cpath ~outd:(dir 45.) y x)
  in
  seq
    [
      Box.draw hbox;
      arrow_down "Transform" "Point" "shifted";
      arrow_up "Transform" "Point" "transform";
      arrow_down "Picture" "Command" "make";
      arrow_up "Picture" "Command" "draw\\_pic";
    ]

let circularity_solution =
  let box title contents =
    Box.vbox ~pos:`Left [ Box.tex (texttt title); contents ]
  in
  let box_tex title contents =
    let contents = List.map (fun line -> Box.tex (texttt line)) contents in
    box title
      (Box.vbox ~name:title ~pos:`Left ~style:RoundRect
         ~stroke:(Some Color.orange) ~fill:(Color.rgb8 255 255 150) contents)
  in
  let box_hbox title contents =
    box title
      (Box.hbox ~pos:`Top ~padding:(Num.cm 0.1) ~dx:(Num.cm 0.1)
         ~dy:(Num.cm 0.1) ~style:RoundRect ~stroke:(Some Color.red)
         ~fill:(Color.rgb8 255 255 210) contents)
  in
  let mlpost_mli =
    box_tex "mlpost.mli"
      [ "module rec Num: sig ... end~~~~~~~~~~~"; "and Point: ..." ]
  in
  let num_ml = box_tex "num.ml" [ "type t = ..."; "let cm = ..." ] in
  let color_ml = box_tex "color.ml" [ "type t = ..." ] in
  let box_ml = box_tex "box.ml" [ "type t = ..." ] in
  let types_mli =
    box_tex "types.mli" [ "type num = ..."; "and point = ..."; "and box = ..." ]
  in
  let mlpost = box_hbox "Mlpost" [ num_ml; color_ml; box_ml ] in
  let all = Box.vbox ~padding:(Num.cm 0.2) [ mlpost_mli; mlpost; types_mli ] in
  let arrow dir x y =
    let x = Box.get x all in
    let y = Box.get y all in
    Arrow.draw (Box.cpath ~outd:(Path.vec (Point.dir dir)) x y)
  in
  seq
    [
      Box.draw all;
      arrow 270. "num.ml" "types.mli";
      arrow 320. "color.ml" "types.mli";
      arrow 270. "box.ml" "types.mli";
    ]

let () = Metapost.emit "sous_typage" sous_typage

let () = Metapost.emit "automate_1" automate_1

let () = Metapost.emit "automate" automate

let () = Metapost.emit "loop_explain" loop_explain

let () = Metapost.emit "uml_client" uml_client

let () = Metapost.emit "uml" uml

let () = Metapost.emit "graph_sqrt" graph_sqrt

let () = Metapost.emit "architecture" architecture

let () = Metapost.emit "bresenham0" bresenham0

let () = Metapost.emit "bresenham" bresenham

let () = Metapost.emit "sharing" sharing

let () = Metapost.emit "tree_compile" sharingcompile

let () = Metapost.emit "arrow_metapost" arrow_metapost

let () = Metapost.emit "arrow_simple" arrow_simple

let () = Metapost.emit "arrow_loop_explain" arrow_loop_explain

let () = Metapost.emit "stages" stages

let () = Metapost.emit "simple" simple

let () = Metapost.emit "align" align

let () = Metapost.emit "persistance" persistance

let () = Metapost.emit "ford" (Command.draw_pic (ford 17))

let () = Metapost.emit "simple_block" simple_block

let () = Metapost.emit "block_arrow" block_arrow

let () = Metapost.emit "list123" list123

let () = Metapost.emit "another_list" another_list

let () = Metapost.emit "deps" deps

let () = Metapost.emit "circularity" circularity

let () = Metapost.emit "circularity_solution" circularity_solution

open Box

let pen = Pen.scale (Num.bp 4.) Pen.circle

let text = tex ~stroke:None ~dx:zero

let texttt ?fill ?name s = text ?fill ?name ("\\texttt{" ^ s ^ "}")

let pointer_arrow ?outd ?ind a b =
  let r = outd and l = ind in
  let p = pathk [ knotp ?r (Box.ctr a); knotp ?l (Box.ctr b) ] in
  let p = cut_after (Box.bpath b) p in
  Command.draw ~pen (pathp [ Box.ctr a ]) ++ draw_arrow p

let self_arrow a b =
  let b = nth 0 b in
  let ya = ypart (ctr a) in
  let xb = xpart (ctr b) in
  let xright = xpart (ctr a) +/ multf 0.7 (width a) in
  let ytop = ypart (ctr b) +/ multf 1.1 (height b) in
  let p =
    pathk ~style:jLine
      [
        knotp (ctr a);
        knotp (Point.pt (xright, ya));
        knotp (Point.pt (xright, ytop));
        knotp (Point.pt (xb, ytop));
        knotp (north b);
      ]
  in
  let p = cut_after (Box.bpath b) p in
  Command.draw ~pen (pathp [ Box.ctr a ]) ++ draw_arrow p

let closure1 =
  let height = bp 10. in
  let pointer ?name () = empty ?name ~width:(bp 15.) ~height () in
  let b =
    hbox ~padding:(bp 50.) ~pos:`Top
      [
        vbox ~padding:(bp 30.)
          [
            hbox ~padding:(bp 20.) ~pos:`Top
              [
                texttt ~name:"pow" "pow";
                vblock ~name:"closure pow" [ tex "code"; pointer () ];
              ];
            hbox ~padding:(bp 20.) ~pos:`Top
              [
                texttt ~name:"sum" "sum";
                vblock ~name:"closure sum"
                  [ tex "code"; tex "\\small 0.001"; pointer (); pointer () ];
              ];
          ];
        vbox ~padding:(bp 15.)
          [
            texttt ~name:"f" "f";
            vblock ~name:"closure f" [ tex "code"; texttt "n"; pointer () ];
          ];
      ]
  in
  let arrow ?outd ?ind (x, i) (y, j) =
    pointer_arrow ?outd ?ind (nth i (get x b)) (nth j (get y b))
  in
  let self_arrow x i = self_arrow (nth i (get x b)) (get x b) in
  let label s x i =
    Command.label ~pos:`Left
      (Picture.tex ("\\tt\\tiny " ^ s))
      (Box.west (nth i (get x b)))
  in
  [
    Box.draw b;
    Helpers.box_arrow (get "pow" b) (nth 0 (get "closure pow" b));
    self_arrow "closure pow" 1;
    label "pow" "closure pow" 1;
    Helpers.box_arrow (get "f" b) (nth 0 (get "closure f" b));
    arrow ~outd:(vec left) ~ind:(vec left) ("closure f", 2) ("closure pow", 0);
    label "i" "closure f" 1;
    label "pow" "closure f" 2;
    Helpers.box_arrow (get "sum" b) (nth 0 (get "closure sum" b));
    self_arrow "closure sum" 3;
    label "sum" "closure sum" 3;
    label "eps" "closure sum" 1;
    label "f" "closure sum" 2;
    arrow ~outd:(vec right) ~ind:(vec right) ("closure sum", 2) ("closure f", 0);
  ]

let () = Metapost.emit "closure1" (seq closure1)

open Tree

let mlposttree =
  let tex = tex ~stroke:(Some Color.black) ~style:RoundRect in
  let leaf s = leaf (Box.tex s) in
  let node s = node ~arrow_style:Undirected (tex s) in
  [
    draw
      (node "\\mlpost"
         [
           node "\\ocaml" [ leaf "\\dots"; leaf "\\dots" ];
           node "\\metapost" [ node "\\LaTeX" []; leaf "\\dots" ];
         ]);
  ]

let () = Metapost.emit "mlposttree" (seq mlposttree)

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

let () = Metapost.emit "why_platform" (seq why_platform)

(*
Local Variables: 
compile-command: "make figures.mp"
End: 
*)
