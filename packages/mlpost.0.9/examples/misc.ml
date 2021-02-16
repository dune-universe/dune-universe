open Mlpost
open Command
open Picture
open Path
open Num
open Num.Infix
open Helpers

(*parse <<togglescript>> *)

(*parse <<misc1 *)
(* the data to plot are computed here *)

let x2 = 9

let y2 = 6

let bresenham_data =
  let a = Array.make (x2 + 1) 0 in
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

let pen = Pen.scale (bp 1.5) Pen.default

let grid w h d f =
  let p i j = (bp (float i *. d), bp (float j *. d)) in
  seq
    [
      iter 0 (w - 1) (fun i -> iter 0 (h - 1) (f i));
      iter 0 w (fun i -> draw ~pen (pathn [ p i 0; p i h ]));
      iter 0 h (fun j -> draw ~pen (pathn [ p 0 j; p w j ]));
    ]

let misc1 =
  let d = 10. in
  let p i j = (bp (float i *. d), bp (float j *. d)) in
  let p2 i j = (bp ((0.5 +. float i) *. d), bp ((0.5 +. float j) *. d)) in
  let pic q i j = draw_pic (Picture.center (Point.pt (p2 i j)) q) in
  let cell i j =
    if j = bresenham_data.(i) then
      let sq = Path.scale (bp d) unitsquare in
      let sq = shift (Point.pt (p i j)) sq in
      fill ~color:Color.red sq
    else seq []
  in
  seq
    [
      grid (x2 + 1) (y2 + 1) d cell;
      pic (tex "0") 0 (-1);
      pic (tex "0") (-1) 0;
      pic (tex "$x_2$") x2 (-1);
      pic (tex "$y_2$") (-1) y2;
    ]

(*parse >> *)

open Diag

(*parse <<misc2 *)
let misc2 =
  let pen = Pen.circle in
  let node x y s = node x y (Box.tex s) in
  let empty_node x y = node x y "\\phantom{M}" in
  let a = empty_node 0. 4. in
  let b = empty_node 0. 3. in
  let inv = node 0. 2. "inv" in
  let c = empty_node 0. 1. in
  let d = empty_node 0. 0. in
  let do_ = node (-2.) 2. "do" in
  let diag = create [ a; b; c; d; inv; do_ ] in
  let arrow = arrow diag in
  arrow a b ~lab:"$i\\leftarrow0$" ~pos:`Right;
  arrow b inv ~lab:"$m\\leftarrow t[i]$" ~pos:`Right;
  arrow c d ~lab:"$i\\ge n$" ~pos:`Right;
  arrow c do_ ~outd:Left ~ind:Down ~lab:"$i<n$" ~pos:`Lowleft;
  arrow inv c ~lab:"$i\\leftarrow i+1$" ~pos:`Right;
  arrow do_ inv ~lab:"$m\\ge t[i]$" ~pos:`Top;
  arrow do_ b ~outd:Up ~ind:Left ~lab:"$m<t[i]$" ~pos:`Upleft;
  draw ~fill:Color.yellow ~stroke:Color.blue ~pen diag

(*
let pen = Pen.scale (bp 1.5) Pen.default

let grid w h d f =
  let p i j = bp (float i *. d), bp (float j *. d) in
  seq [iter 0 (w-1) (fun i -> iter 0 (h-1) (f i));
       iter 0 w (fun i -> Command.draw ~pen (pathn [p i 0; p i h]));
       iter 0 h (fun j -> Command.draw ~pen (pathn [p 0 j; p w j]))]

let bq = tex "\\font\\Chess=chess10 {\\Chess q}"
let question = tex "?"

let misc3 = 
  let d = 15. in
  let p i j = bp (float i *. d), bp (float j *. d) in
  let p2 i j = bp ((0.5 +. float i) *. d), bp ((0.5 +. float j) *. d) in
  let pic q i j = draw_pic (Picture.center (Point.pt (p2 i j)) q) in
  let cell i j = 
    let l = 
      if (i+j) mod 2 = 1 then 
	let sq = Path.scale (bp d) unitsquare in
	let sq = shift (Point.pt (p i j)) sq in
	[fill ~color:(Color.gray 0.7) sq] 
      else 
	[] 
    in
    seq 
      (if i = 4 && j = 7 || i = 3 && j = 5 || i = 6 && j = 6 then 
	 l @ [pic bq i j]
       else if j = 4 then 
	 l @ [pic question i j]
       else
	 l)
  in
  grid 8 8 d cell
*)
(*parse >> <<misc4 *)

let foi, iof = (float_of_int, int_of_float)

let nodes =
  Array.init 6 (fun i ->
      Array.init 4 (fun j -> node (foi i) (foi j) (Box.empty ~width:(bp 4.) ())))

(* 	  (Printf.sprintf "(%d,%d)" i j))) *)

let nodesl = List.flatten (List.map Array.to_list (Array.to_list nodes))

let diag = create nodesl

let add = arrow diag ~head:false

let edges =
  for i = 0 to 5 do
    for j = 0 to 3 do
      ( try add ~outd:(Angle 45.) nodes.(i).(j) nodes.(i + 1).(j + 1)
        with _ -> () );
      ( try add ~outd:(Angle 135.) nodes.(i).(j) nodes.(i - 1).(j + 1)
        with _ -> () );
      try add ~outd:Up nodes.(i).(j) nodes.(i).(j + 1) with _ -> ()
    done
  done

let graph =
  draw ~fill:(Color.gray 0.8)
    ~style:(Box.circle ~dx:(Num.bp 6.) ~dy:(Num.bp 6.))
    diag

let misc4 = draw_pic (Picture.scale (bp 0.5) (Picture.make graph))

(*parse >> <<misc5 *)

let foi, iof = (float_of_int, int_of_float)

let nodes =
  Array.init 6 (fun i ->
      Array.init 4 (fun j ->
          (node (foi i) (foi j) (Box.empty ~width:(bp 4.) ()), i, j)))

(*  	  (Printf.sprintf "(%d,%d)" i j), i, j))) *)

let nodesl =
  List.fold_left
    (fun acc (n, i, j) -> if (i + j) mod 2 = 0 then n :: acc else acc)
    []
    (List.flatten (List.map Array.to_list (Array.to_list nodes)))

let diag = create nodesl

let node i j =
  let n, _, _ = nodes.(i).(j) in
  n

let add = arrow diag ~head:false

let edges =
  for i = 0 to 5 do
    for j = 0 to 3 do
      if (i + j) mod 2 = 0 then (
        ( try add ~outd:(Angle 165.) (node i j) (node (i - 3) (j + 1))
          with _ -> () );
        ( try add ~outd:(Angle 135.) (node i j) (node (i - 1) (j + 1))
          with _ -> () );
        ( try add ~outd:(Angle 45.) (node i j) (node (i + 1) (j + 1))
          with _ -> () );
        try add ~outd:(Angle 15.) (node i j) (node (i + 3) (j + 1))
        with _ -> () )
    done
  done

let graph =
  draw ~fill:(Color.gray 0.8)
    ~style:(Box.circle ~dx:(Num.bp 6.) ~dy:(Num.bp 6.))
    diag

let misc5 = draw_pic (Picture.scale (bp 0.5) (Picture.make graph))

(*parse >> <<misc6 *)

open Num.Infix

let scale k = 2. *./ Num.mm k

let drawover p = seq [ fill ~color:Color.white p; Command.draw p ]

let filldraw p c = seq [ fill ~color:c p; Command.draw p ]

let polyg fl = path ~scale ~style:jLine ~cycle:jLine fl

let mydraw b c p = if b then filldraw p c else drawover p

let copper = Color.rgb (213. /. 256.) (186. /. 256.) (6. /. 256.)

let steel = Color.gray 0.85

(** The (closed) hihat ! true : color, false : b&w *)
let hihat b =
  let d = mydraw b in
  let cymb = Path.yscale (scale 1.2) (Path.xscale (scale 7.) fullcircle) in
  let head =
    polyg
      [
        (-0.5, 0.);
        (-0.3, 0.);
        (-0.15, 1.65);
        (0.15, 1.65);
        (0.3, 0.);
        (0.5, 0.);
        (0.33, -1.2);
        (-0.33, -1.2);
      ]
  in
  let stem = polyg [ (0.2, -1.2); (0.2, -9.); (-0.2, -9.); (-0.2, -1.2) ] in
  let stemjoint =
    polyg [ (0.5, -4.8); (0.25, -6.); (-0.25, -6.); (-0.5, -4.8) ]
  in
  let foot b l alpha c =
    let hb = b /. 2. and ls = l *. sin alpha and lc = -.l *. cos alpha in
    let f = polyg [ (hb, 0.); (hb -. ls, lc); (-.hb -. ls, lc); (-.hb, 0.) ] in
    Path.shift (Point.p ~scale c) f
  in
  seq
    [
      d steel stem;
      d steel stemjoint;
      d steel (foot 0.5 3.5 0. (0., -9.0));
      d steel (foot 0.5 7.5 (Num.deg2rad 35.) (-0.3, -9.0));
      d steel (foot 0.5 7.5 (Num.deg2rad (-35.)) (0.3, -9.0));
      d steel (foot 0.3 2.1 (Num.deg2rad 110.) (0.0, -12.));
      d steel (foot 0.3 2. (Num.deg2rad 250.) (0.0, -12.));
      d steel (foot 0.3 5. 0. (0., -9.0));
      d steel head;
      d copper cymb;
    ]

(** The snare drum ! true : color, false : b&w *)
let snare b =
  let d = mydraw b in
  let drumout =
    polyg [ (-3.5, 1.66); (3.5, 1.66); (3.5, -1.66); (-3.5, -1.66) ]
  in
  let drumin =
    polyg [ (-3.5, 1.26); (3.5, 1.26); (3.5, -1.26); (-3.5, -1.26) ]
  in
  seq [ d steel drumout; d Color.red drumin ]

let snarepic b pos =
  let pic = Picture.make (snare b) in
  let tpic = Picture.transform [ Transform.shifted (Point.p ~scale pos) ] pic in
  draw_pic tpic

let misc6 = seq [ hihat true; snarepic true (-5.3, -3.8) ]

(*parse >> <<misc7 *)

open Point

let rec fold_append ?(style = jCurve) = function
  | [] -> failwith "No empty list allowed"
  | [ x ] -> x
  | x :: xs -> append ~style x (fold_append xs)

let s = 0.004

let xs1 = 48.

let xs2 = 25.

let ys = 19.

let add (a1, a2) (b1, b2) = (a1 +. b1, a2 +. b2)

let mult f (a1, a2) = (f *. a1, f *. a2)

let myscale = List.map (mult s)

let misc7 =
  let pen1 = Pen.circle in
  let mygreen = Color.rgb 0.8 0.9 0.8 in
  let p1 = (750., 8000. -. 4950.) in
  let p2 = (1050., 8000. -. 4950.) in
  let p3 = (2100., 8000. -. 4800.) in
  let p4 = (2925., 8000. -. 4650.) in
  let p5 = (4050., 8000. -. 5100.) in
  let p6 = (4050., 8000. -. 5550.) in
  let p7 = (3750., 8000. -. 6075.) in
  let p8 = (3150., 8000. -. 6900.) in
  let p9 = (3075., 8000. -. 7500.) in
  let p10 = (3525., 8000. -. 7950.) in
  let p11 = (4275., 8000. -. 8775.) in
  let p12 = (5400., 8000. -. 9300.) in
  let p13 = (4725., 8000. -. 8550.) in
  let p14 = (4275., 8000. -. 7725.) in
  let p15 = (4875., 8000. -. 8325.) in
  let p16 = (5550., 8000. -. 8700.) in
  let p17 = (5100., 8000. -. 7950.) in
  let p18 = (4800., 8000. -. 7125.) in
  let p19 = (5400., 8000. -. 7725.) in
  let p20 = (6150., 8000. -. 8100.) in
  let p21 = (5550., 8000. -. 7275.) in
  let p22 = (5250., 8000. -. 6375.) in
  let p23 = (5850., 8000. -. 7050.) in
  let p24 = (6600., 8000. -. 7500.) in
  let p25 = (6075., 8000. -. 6675.) in
  let p26 = (5700., 8000. -. 5775.) in
  let p27 = (6975., 8000. -. 7125.) in
  let p28 = (8625., 8000. -. 7950.) in
  let p29 = (7875., 8000. -. 7350.) in
  let p30 = (7275., 8000. -. 6750.) in
  let p31 = (8175., 8000. -. 7200.) in
  let p32 = (9150., 8000. -. 7425.) in
  let p33 = (8325., 8000. -. 6975.) in
  let p34 = (7725., 8000. -. 6375.) in
  let p35 = (8550., 8000. -. 6750.) in
  let p36 = (9525., 8000. -. 6825.) in
  let p37 = (8625., 8000. -. 6450.) in
  let p38 = (8100., 8000. -. 6000.) in
  let p39 = (9000., 8000. -. 6300.) in
  let p40 = (9975., 8000. -. 6300.) in
  let p41 = (9075., 8000. -. 6000.) in
  let p42 = (8400., 8000. -. 5550.) in
  let p43 = (9525., 8000. -. 5925.) in
  let p44 = (10425., 8000. -. 5925.) in
  let p45 = (9300., 8000. -. 5550.) in
  let p46 = (8250., 8000. -. 5100.) in
  let p47 = (7275., 8000. -. 4875.) in
  let p48 = (6300., 8000. -. 4800.) in
  let p49 = (7275., 8000. -. 4500.) in
  let p50 = (8400., 8000. -. 4500.) in
  let p51 = (7500., 8000. -. 4050.) in
  let p52 = (6825., 8000. -. 3900.) in
  let p53 = (7800., 8000. -. 3825.) in
  let p54 = (8700., 8000. -. 3975.) in
  let p55 = (7875., 8000. -. 3375.) in
  let p56 = (7050., 8000. -. 3075.) in
  let p57 = (8175., 8000. -. 3150.) in
  let p58 = (8925., 8000. -. 3450.) in
  let p59 = (8175., 8000. -. 2775.) in
  let p60 = (7350., 8000. -. 2400.) in
  let p61 = (8250., 8000. -. 2475.) in
  let p62 = (9225., 8000. -. 3000.) in
  let p63 = (8850., 8000. -. 2100.) in
  let p64 = (8400., 8000. -. 1650.) in
  let p66 = (8100., 8000. -. 1875.) in
  let p67 = (7200., 8000. -. 1575.) in
  let p68 = (5850., 8000. -. 1500.) in
  let p69 = (5625., 8000. -. 2025.) in
  let p70 = (5475., 8000. -. 2400.) in
  let p71 = (5100., 8000. -. 3000.) in
  let p72 = (4650., 8000. -. 3750.) in
  let p73 = (3525., 8000. -. 3450.) in
  let p74 = (2550., 8000. -. 3075.) in
  let p75 = (2325., 8000. -. 3375.) in
  let p76 = (2100., 8000. -. 3600.) in
  let p77 = (1425., 8000. -. 4050.) in
  let p78 = (975., 8000. -. 4350.) in
  let p79 = (525., 8000. -. 4875.) in
  let p80 = (1840., 8000. -. 4600.) in
  let p81 = (2375., 8000. -. 4550.) in
  let line1 = path (myscale [ p79; p1; p2; p3; p4; p5 ]) in
  let line2 =
    fold_append ~style:jLine
      (List.map
         (fun l -> path (myscale l))
         [
           [ p9; p10; p11; p12 ];
           [ p12; p13; p14 ];
           [ p14; p15; p16 ];
           [ p16; p17; p18 ];
           [ p18; p19; p20 ];
           [ p20; p21; p22 ];
           [ p22; p23; p24 ];
           [ p24; p25; p26 ];
           [ p26; p27; p28 ];
           [ p28; p29; p30 ];
           [ p30; p31; p32 ];
           [ p32; p33; p34 ];
           [ p34; p35; p36 ];
           [ p36; p37; p38 ];
           [ p38; p39; p40 ];
           [ p40; p41; p42 ];
           [ p42; p43; p44 ];
           [ p44; p45; p46 ];
           [ p46; p47; p48 ];
           [ p48; p49; p50 ];
           [ p50; p51; p52 ];
           [ p52; p53; p54 ];
           [ p54; p55; p56 ];
           [ p56; p57; p58 ];
           [ p58; p59; p60 ];
           [ p60; p61; p62 ];
           [ p62; p66; p67; p68 ];
         ])
  in
  let line3 = path (myscale [ p62; p63; p64 ]) in
  let line4 = path (myscale [ p72; p73; p74 ]) in
  let line5 = path (myscale [ p79; p80; p81 ]) in
  let line6 = path (myscale [ p6; p6; p7; p8; p9 ]) in
  let line7 = path (myscale [ p74; p75; p76; p77; p78; p78; p79 ]) in
  let line8 = path (myscale [ p68; p69; p70; p71; p72 ]) in
  let bird =
    cycle ~style:jLine
      (fold_append ~style:jLine [ line1; line6; line2; line8; line4; line7 ])
  in
  Command.iter (-1) 1 (fun x ->
      Command.iter (-1) 1 (fun y ->
          let xf, yf = (float_of_int x, float_of_int y) in
          let offset = ((xf *. xs1) +. (yf *. xs2), yf *. ys) in
          let offset2 =
            (((xf +. 1.) *. xs1) +. ((yf -. 1.) *. xs2), (yf -. 1.) *. ys)
          in
          let tr p = Path.shift (bpp offset) p in
          let mypath po =
            let offset = add offset2 po in
            Path.shift (bpp offset) Path.fullcircle
          in
          seq
            ( [
                fill ~color:Color.red (mypath (-12., 27.));
                Command.draw ~color:Color.blue (mypath (-12., 27.));
              ]
            @ [ fill ~color:mygreen (tr bird) ]
            @ List.map
                (fun p -> Command.draw ~pen:pen1 (tr p))
                [ line1; line2; line3; line4; line5 ]
            @ List.map
                (fun p -> Command.draw ~pen:pen1 (tr p))
                [ line6; line7; line8 ] )))

(*parse >> <<misc8 *)

(* type of Box lattice *)

type node = N of Box.t * node list (* a node and its successors *)

type lattice = node list list (* nodes lines, from top to bottom *)

(* drawing *)

let dx = bp 12.

let dy = bp 12.

module H = Hashtbl.Make (struct
  type t = Box.t

  let hash b = Hashtbl.hash b

  let equal = ( == )
end)

let nodes = H.create 97

let draw la =
  let line l = Box.hbox ~padding:dx (List.map (function N (b, _) -> b) l) in
  let to_list b = Array.to_list (Box.elts b) in
  let to_list2 b = List.map to_list (to_list b) in
  let la' = Box.vbox ~padding:dy (List.map line la) in
  List.iter2
    (List.iter2 (fun (N (b, _)) b' -> H.add nodes b b'))
    la (to_list2 la');
  let box b = H.find nodes b in
  let draw_node (N (b, l)) =
    let b = box b in
    Box.draw b ++ iterl (fun (N (s, _)) -> box_arrow b (box s)) l
  in
  iterl (iterl draw_node) la

(* example: the subwords lattice *)

let node s l =
  let s = if s = "" then "$\\varepsilon$" else s in
  let s = "\\rule[-0.1em]{0in}{0.8em}" ^ s in
  N (Box.circle (Box.tex s), l)

(* folds over the bits of an integer (as powers of two) *)
let fold_bit f =
  let rec fold acc n =
    if n = 0 then acc
    else
      let b = n land -n in
      fold (f acc b) (n - b)
  in
  fold

(* the bits in [n] indicate the selected characters of [s] *)
let subword s n =
  let len = fold_bit (fun l _ -> l + 1) 0 n in
  let w = Bytes.create len in
  let j = ref 0 in
  for i = 0 to String.length s - 1 do
    if n land (1 lsl i) != 0 then (
      Bytes.set w !j s.[i];
      incr j )
  done;
  Bytes.unsafe_to_string w

(* builds the lattice of [s]'s subwords *)
let subwords s =
  let n = String.length s in
  let levels = Array.make (n + 1) [] in
  let memo = Hashtbl.create 97 in
  let rec make_node lvl x =
    try Hashtbl.find memo x
    with Not_found ->
      let n =
        node (subword s x)
          (fold_bit (fun l b -> make_node (lvl - 1) (x - b) :: l) [] x)
      in
      Hashtbl.add memo x n;
      levels.(lvl) <- n :: levels.(lvl);
      n
  in
  let _ = make_node n (lnot (-1 lsl n)) in
  Array.to_list levels

let misc8 = draw (subwords "abcd")

(*parse >> <<misc9 *)

type t = One | Two | Three | Four

let d0 = 1.

let d1 = sqrt (2. *. (1. +. cos (72. *. 2. *. 3.14159 /. 360.)))

let d2 = sqrt (2. *. (1. -. cos (36. *. 2. *. 3.14159 /. 360.)))

let r0 = d0 /. (d0 +. d2)

let r1 = d1 /. (d0 +. d1)

let rec pave t a b c n =
  if n > 0 then
    match t with
    | One ->
        let d = Point.segment r0 a c in
        seq [ pave One b c d (n - 1); pave Four b d a (n - 1) ]
    | Two ->
        let d = Point.segment r0 a b in
        seq [ pave Two c d b (n - 1); pave Three c a d (n - 1) ]
    | Three ->
        let d = Point.segment r1 a b in
        let e = Point.segment r0 a c in
        seq
          [
            pave One d c e (n - 1);
            pave Three b c d (n - 1);
            pave Four d e a (n - 1);
          ]
    | Four ->
        let d = Point.segment r1 a c in
        let e = Point.segment r0 a b in
        seq
          [
            pave Two d e b (n - 1);
            pave Three d a e (n - 1);
            pave Four c d b (n - 1);
          ]
  else
    let pen = Pen.circle in
    let gb = Color.rgb 0. 1. 1. in
    let gr = Color.rgb 1. 1. 0. in
    let path = pathp ~style:jLine ~cycle:jLine [ a; b; c ] in
    let color, segs =
      match t with
      | One -> (gb, [ [ a; c ]; [ a; b ] ])
      | Two -> (gb, [ [ a; b ]; [ a; b ] ])
      | Three -> (gr, [ [ a; c ]; [ c; b ] ])
      | Four -> (gr, [ [ b; c ]; [ a; b ] ])
    in
    seq
      [
        Command.draw path;
        fill path ~color;
        seq (List.map (fun l -> Command.draw ~pen (pathp ~style:jLine l)) segs);
      ]

let misc9 =
  let a = cmp (0., 0.) in
  let b = cmp (3., 0.) in
  let d = Point.rotate 72. b in
  let c = Point.add d (cmp (3., 0.)) in
  seq [ pave Three a c d 6; pave Four a b c 6 ]

(*
(*>> <<misc10 *)

let misc10 =
  let k = 7. in
  let pen = Pen.rotate 40. (Pen.scale (bp 0.5) Pen.square) in
  let check = 
    jointpath [-1.2,1.2; 0., -2. ; 2., 2. ; 5., 5.] [jLine ; jCurve; jCurve]
  in
  seq [fill ~color:Color.black (Path.scale (bp k) Path.fullcircle) ;
       label ~pos:`Left (Picture.tex "Pr") (Point.p (k /. (-4.),0.)) ;
       label ~pos:`Right (Picture.tex "al") (Point.p (k /. 4.,0.)) ;
       Command.draw ~color:Color.green ~pen check;]
*)
(*parse >> <<misc11 *)

let misc11 =
  let () = Random.init 42 in
  (* In order to have always the same figure *)
  let branchrotation = 60. in
  let offset = 180. -. branchrotation in
  let thinning = 0.7 in
  let shortening = 0.8 in
  let drawit a b thickness =
    let pen = Pen.scale (bp thickness) Pen.circle in
    Command.draw ~pen (pathp [ a; b ])
  in
  let randrotate a b neg =
    let r = offset +. Random.float branchrotation in
    let r = if neg then 0. -. r else r in
    let tr = [ Transform.rotate_around b r ] in
    Point.transform tr a
  in
  let rec tree a b n size acc =
    let acc = drawit a b size :: acc in
    if n <= 0 then acc
    else
      let c = segment shortening b (randrotate a b false) in
      let d = segment shortening b (randrotate a b true) in
      let newsize = thinning *. size in
      let acc = tree b c (n - 1) newsize acc in
      tree b d (n - 1) newsize acc
  in
  seq (tree (p (0., 0.)) (pt (bp 0., Num.cm 1.)) 10 2. [])

(*parse >> <<misc12 *)
let alpha = atan 1.

let beta = atan 1. /. 2.

let mag = 10.

let proj x y z =
  ( mag *. float (x - y) *. cos alpha,
    mag *. ((float (x + y) *. sin alpha *. sin beta) +. (float z *. cos beta))
  )

let pen = Pen.scale (bp 2.5) Pen.default

let square color p i j =
  let pt i j =
    let x, y = p i j in
    Point.pt (bp x, bp y)
  in
  let points = [ pt i j; pt (i + 1) j; pt (i + 1) (j + 1); pt i (j + 1) ] in
  let path = pathp ~style:jLine ~cycle:jLine points in
  seq [ fill ~color path; Command.draw ~pen path ]

let right = square Color.orange (fun i j -> proj i 0 j)

let up = square Color.yellow (fun i j -> proj i j 3)

let left = square Color.green (fun i j -> proj 0 (3 - i) j)

let misc12 =
  seq
    [
      iter 0 2 (fun i -> iter 0 2 (right i));
      iter 0 2 (fun i -> iter 0 2 (up i));
      iter 0 2 (fun i -> iter 0 2 (left i));
    ]

(*parse >> <<misc13 *)

(* football field of length 2w and width 2h *)
let football_field w h =
  let green = Color.rgb 0.1 0.8 0.1 in
  let white = Color.white in
  (* scale : one meter = 2 bp *)
  let u x = Num.bp (2. *. x) in
  let pt x y = Point.pt (u x, u y) in
  (* shapes *)
  let rect x1 y1 x2 y2 =
    pathp ~style:jLine ~cycle:jLine [ pt x1 y1; pt x1 y2; pt x2 y2; pt x2 y1 ]
  in
  let circle x y r =
    Path.shift (pt x y) (Path.scale (u (2. *. r)) Path.fullcircle)
  in
  (* line width = 0.1 +- 0.02 meters *)
  let pen = Pen.scale (u 0.24) Pen.circle in
  let plot x y =
    Command.draw ~color:white
      ~pen:(Pen.scale (u 0.3) Pen.circle)
      (pathp [ pt x y ])
  in
  let line = Command.draw ~pen ~color:white in
  let half =
    fill ~color:green (rect 0. (-.h -. 2.) (w +. 2.) (h +. 2.))
    ++
    let field = rect 0. (-.h) w h in
    line field
    ++ (* corners *)
    line (cut_after field (cut_before field (circle w h 1.)))
    ++ line (cut_after field (cut_before field (circle w (-.h) 1.)))
    ++ plot (w -. 11.) 0.
    ++
    let penalty_area =
      let hr = (7.32 /. 2.) +. 16.5 in
      rect w hr (w -. 16.5) (-.hr)
    in
    line penalty_area
    ++ line
         (cut_after penalty_area
            (cut_before penalty_area (circle (w -. 11.) 0. 9.15)))
    ++ line
         (let hr = (7.32 /. 2.) +. 5.5 in
          rect w hr (w -. 5.5) (-.hr))
  in
  half
  ++ draw_pic (Picture.rotate 180. (Picture.make half))
  ++ line (circle 0. 0. 9.15)
  ++ plot 0. 0.

let misc13 = football_field 50. 35.

(*parse >> <<misc14 *)

let ( ++ ) x y = pt (cm x, cm y)

let misc14 =
  let a = Box.circle (Box.tex "$\\sqrt2$") in
  let b =
    Box.shift (2. ++ 0.) (Box.rect ~fill:Color.purple (Box.tex "$\\pi$"))
  in
  let pen = Pen.scale (bp 3.) Pen.default in
  seq
    [
      Box.draw a;
      Box.draw b;
      Command.draw ~color:Color.red (Path.shift (1. ++ 1.) (Box.bpath a));
      draw_label_arrow ~color:Color.orange ~pen ~pos:`Upright
        (Picture.tex "foo") (Box.west a) (Box.south_east b);
      box_arrow ~color:Color.blue a b;
    ]

(*parse >> *)
let () =
  List.iter
    (fun (i, fig) ->
      Metapost.emit ("misc" ^ string_of_int i) (Picture.scale (Num.bp 3.) fig))
    [
      (1, misc1);
      (2, misc2);
      (*   3,misc3; chess10 can't be used by cairo*)
      (4, misc4);
      (5, misc5);
      (6, misc6);
      (7, misc7);
      (8, misc8);
      (9, misc9);
      (*   10,misc10; Pen.square is not implemented *)
      (11, misc11);
      (12, misc12);
      (13, misc13);
      (14, misc14);
    ]
