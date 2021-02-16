open Mlpost
open Box
open Tree
open Color

let sprintf = Format.sprintf

let stern_brocot h =
  let frac (a, b) = tex (sprintf "$\\frac{%d}{%d}$" a b) in
  let rec make ((a, b) as lo) ((c, d) as hi) h =
    let r = (a + c, b + d) in
    if h = 1 then leaf (frac r)
    else
      node ~arrow_style:Undirected (frac r)
        [ make lo r (h - 1); make r hi (h - 1) ]
  in
  make (0, 1) (1, 0) h

let tree8 = draw (stern_brocot 5)

let texint n = tex (sprintf "$F_{%d}$" n)

let rec fib = function
  | (0 | 1) as n -> leaf (texint n)
  | n -> node ~arrow_style:Undirected (texint n) [ fib (n - 1); fib (n - 2) ]

let fibtree = draw (fib 6)

type t = Node of int * t list

let rec bin = function
  | 0 -> Node (0, [])
  | n ->
      let (Node (_, l) as t) = bin (n - 1) in
      Node (n, t :: l)

let rec trans (Node (n, l)) =
  node ~arrow_style:Undirected (tex (sprintf "${2^{%d}}$" n)) (List.map trans l)

let tree10 = draw (trans (bin 4))

open Num
open Color

let hist =
  Hist.stack
    ~fill:[ lightred; lightblue; lightyellow; lightgreen ]
    [ [ 4.; 5.; 5. ]; [ 8.; 3.; 1. ]; [ 2.; 8.; 1.; 4. ] ]

open Helpers
open Command
open Box

let hbox = hbox ~padding:(bp 30.)

let vbox = vbox ~padding:(bp 30.)

let diag =
  let rt s = rect (tex s) in
  let a = rt "A" and b = rt "B" and c = rt "C" in
  let ab = round_rect ~dx:(bp 10.) ~dy:(bp 10.) (hbox [ a; b ]) in
  let v = vbox [ ab; c ] in
  let arrow x y = box_arrow ~sep:(bp 5.) (sub x v) (sub y v) in
  draw v ++ arrow a b ++ arrow ab c

let ( |> ) x f = f x

let archi_backend =
  let sep = bp 5. and dx = bp 5. and dy = bp 10. in
  let rt ?fill s = round_rect ?fill (tex s) in
  let sigma = tex "$\\Sigma$" in
  let metapost = rt ~fill:lightred "Metapost"
  and cairo = rt ~fill:lightred "Cairo"
  and concrete = rt ~fill:lightblue "Concrete"
  and mp = rt ~fill:lightred ".mp" in
  let left = vbox [ mp; metapost ] in
  let lsigma = sub sigma (Box.hbox [ sub metapost left; sigma ]) in
  let left = round_rect ~dy ~dx ~fill:lightgreen (group [ left; lsigma ]) in
  let right = vbox [ concrete; cairo ] in
  let rsigma = sub sigma (Box.hbox [ sub concrete right; sigma ]) in
  let right = group [ right; rsigma ] in
  let center = hbox [ left; right ] in
  let mlpost = tex "Mlpost" in
  let mlpost = List.hd (same_width [ mlpost; center ]) in
  let mlpost = round_rect ~fill:lightblue mlpost in
  let output = rt ~fill:lightgray "output" in
  let img =
    Box.scale (bp 0.5)
      (round_rect ~fill:lightyellow ~dy ~dx (Tree.to_box (fib 3)))
  in
  let all = vbox [ mlpost; center; output ] in
  let img = Box.place `East ~padding:(bp 30.) (sub output all) img in
  let arrow (x, y) = box_arrow ~sep (sub x all) (sub y all) in
  let arrows =
    [
      (mp, metapost);
      (concrete, cairo);
      (mlpost, concrete);
      (concrete, mlpost);
      (mlpost, mp);
      (metapost, output);
      (cairo, output);
    ]
    |> List.map arrow |> seq
  in
  draw all ++ arrows ++ draw img ++ box_arrow ~sep (sub output all) img

(* Koda-Ruskey *)

open Command
open Num
open Path
open Box
open Tree

type tree = Node of int * forest

and forest = tree list

type color = White | Black

let bits = Array.create 100 White

let rec enum_forest k = function
  | [] -> k ()
  | Node (i, f') :: f ->
      let k () = enum_forest k f in
      if bits.(i) = White then (
        k ();
        bits.(i) <- Black;
        enum_forest k f' )
      else (
        enum_forest k f';
        bits.(i) <- White;
        k () )

let leaf i = Node (i, [])

let t1 = Node (0, [ leaf 1 ])

let t2 = Node (2, [ leaf 3; leaf 4 ])

let f0 = [ t1; t2 ]

let draw1 f0 =
  let white = circle (empty ()) in
  let black = circle ~fill:Color.black (empty ()) in
  let node = node ~arrow_style:Undirected ~ls:(bp 10.) ~sep:(bp 2.) in
  let rec make_tree = function
    | [] -> []
    | Node (i, sons) :: f ->
        node (if bits.(i) = White then white else black) (make_tree sons)
        :: make_tree f
  in
  hbox ~padding:(bp 5.) (List.map to_box (make_tree f0))

(* let () = Metapost.emit "f0" (Box.draw(draw1 f0)) *)

let gray f =
  Array.fill bits 0 100 White;
  let boxes = ref [] in
  let k () = boxes := draw1 f :: !boxes in
  enum_forest k f;
  hbox ~padding:(bp 10.) (List.rev !boxes)

let box_arrow = Helpers.box_arrow ~sep:(bp 10.)

let smart_box_arrow ol a b =
  let p = smart_path ~style:jLine ol (ctr a) (ctr b) in
  let p = cut_after (bpath b) (cut_before (bpath a) p) in
  let p = strip (bp 10.) p in
  Arrow.simple p

let gray_grid w f =
  Array.fill bits 0 100 White;
  let boxes = ref [ [] ] in
  let k () =
    let b = draw1 f in
    match !boxes with
    | l :: _ when List.length l = w -> boxes := [ b ] :: !boxes
    | l :: r -> boxes := (b :: l) :: r
    | [] -> assert false
  in
  enum_forest k f;
  let rec reverse = function
    | [] -> []
    | [ x ] -> [ List.rev x ]
    | x :: y :: l -> List.rev x :: y :: reverse l
  in
  let boxes = reverse (List.rev !boxes) in
  let h = List.length boxes in
  let b = gridl ~hpadding:(bp 20.) ~vpadding:(bp 10.) ~stroke:None boxes in
  let sub i j = nth j (nth i b) in
  Box.draw b
  ++ iter 0 (h - 1) (fun i ->
         iter 0 (w - 2) (fun j ->
             if i mod 2 = 0 then box_arrow (sub i j) (sub i (j + 1))
             else box_arrow (sub i (j + 1)) (sub i j)))
  ++ iter 0 (h - 2) (fun i ->
         if i mod 2 = 0 then
           smart_box_arrow
             [ Rightn (bp 25.); Down; Left ]
             (sub i (w - 1))
             (sub (i + 1) (w - 1))
         else
           smart_box_arrow
             [ Leftn (bp 25.); Down; Right ]
             (sub i 0)
             (sub (i + 1) 0))

let _ =
  List.iter
    (fun (name, fig) -> Metapost.emit name fig)
    [
      ("archi_backend", archi_backend);
      ("fibtree", fibtree);
      ("hist", hist);
      ("diag", diag);
      ("gray_f0", gray_grid 5 f0);
      (*     "tree10", tree10; *)
    ]
