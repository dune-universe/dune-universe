open Mlpost
open Num
open Command
open Helpers
open Path
open Point
open Color
open Box

(*parse <<togglescript>> *)

(*parse <<boxes1 *)

let boxes1 =
  let node s =
    rect ~name:s ~stroke:None (round_rect ~stroke:None ~fill:lightblue (tex s))
  in
  let b = hbox ~padding:(bp 20.) [ node "A"; node "B" ] in
  let arrow x y = box_arrow ~pen:Pen.circle ~color:red (get x b) (get y b) in
  seq [ draw b; arrow "A" "B" ]

(*parse >> <<boxes2 *)
let boxes2 =
  let tex = tex ~style:Rect ~stroke:(Some Color.black) in
  let b =
    hbox ~padding:(bp 20.)
      [
        vbox ~padding:(bp 4.) ~pos:`Right
          [ tex "A"; tex ~name:"bc" "$\\sqrt{x}$"; tex "D" ];
        vbox ~padding:(bp 4.) ~pos:`Left [ tex ~name:"e" "E"; tex "FGH" ];
      ]
  in
  seq [ draw b; box_arrow (get "bc" b) (get "e" b) ]

(*parse >> <<boxes3 *)
let boxes3 =
  let tex = tex ~style:Circle ~stroke:(Some Color.black) in
  let b = vbox [ tex "a"; hbox [ tex ~name:"b" "b"; tex "c" ] ] in
  let f = hbox ~padding:(bp 20.) [ b; b; b ] in
  let arrow = box_arrow ~outd:(vec (dir (-60.))) in
  let node i = get "b" (nth i f) in
  seq [ draw f; arrow (node 0) (node 1); arrow (node 1) (node 2) ]

(*parse >> <<boxes4 *)
(* inspired by functional metapost *)
let boxes4 =
  let two = Num.bp 2. in
  let b =
    vbox ~fill:black ~padding:(Num.bp 3.) ~dx:two ~dy:two
      [
        tex ~style:Circle ~fill:red "R";
        tex ~style:Circle ~fill:yellow "Y";
        tex ~style:Circle ~fill:green "G";
      ]
  in
  draw b

(*parse >> <<boxes5 *)
(* inspired by functional metapost *)
let boxes5 =
  let two = Num.bp 2. in
  let five = Num.bp 5. in
  let tex = tex ~dx:two ~dy:two in
  let vbox =
    vbox ~padding:(Num.bp 3.) ~stroke:(Some black) ~style:RoundRect ~dy:five
      ~dx:five
  in
  let b =
    vbox
      [
        tex "recursively enumerable languages";
        vbox
          [
            tex "decidable languages";
            vbox
              [
                tex "context sensitive";
                vbox
                  [
                    tex "context free";
                    tex ~style:RoundRect ~stroke:(Some black) "regular";
                  ];
              ];
          ];
      ]
  in
  draw b

(*parse >> <<boxes6 *)
module P = Point
open Num.Infix

let ( |> ) x f = f x

let draw_point ?(color = Color.red) t =
  Point.draw ~pen:(Pen.scale (bp 4.) Pen.default) ~color t

(* aligne verticalement le barycentre [(west,5);(east,2)] *)
let boxes6 =
  let two = Num.bp 2. in
  let five = Num.bp 5. in
  let tex = tex ~dx:two ~dy:two in
  let a = tex "recursively enumerable languages" in
  let b = tex "context sensitive" in
  let c = tex "context free" in
  let add_point t =
    let w = corner `West t in
    let e = corner `East t in
    let p =
      P.mult (one // (two +/ five)) (P.add (P.mult five w) (P.mult two e))
    in
    setp "something" p t
  in
  let a = add_point a in
  let b = add_point b in
  let c = add_point c in
  let points =
    [ a; b; c ]
    |> List.map (getp "something")
    |> List.map draw_point |> Command.seq
  in
  (*(*Example de débuggage quand on a le nouveau backend*)
    List.iter fun p -> let {Concrete.CPoint.x=x;y=y} =
    Concrete.cpoint_of_point (getp "something" p) in
             Format.printf "x = %f; y = %f@." x y) [a;b;c];*)
  Command.seq
    [ points; Box.draw (vbox ~pos:(`Custom (getp "something")) [ a; b; c ]) ]

(*parse >> <<boxes7 *)

let boxes7 =
  let b = rect (empty ~width:(cm 3.) ~height:(cm 1.5) ()) in
  let make t pos =
    let aux pad = Box.draw (Box.place pos b ?padding:pad (rect (tex t))) in
    aux None ++ aux (Some (cm 1.2))
  in
  let boxes =
    [
      Box.draw b;
      make "center" `Center;
      make "south" `South;
      make "north" `North;
      make "east" `East;
      make "west" `West;
      make "southwest" `Southwest;
      make "southeast" `Southeast;
      make "northwest" `Northwest;
      make "northeast" `Northeast;
      make "custom"
        (`Custom
          (fun b ->
            Point.add
              (Point.scale (bp 0.5) (ctr b))
              (Point.scale (bp 0.5) (corner `Southeast b))));
    ]
  in
  Command.seq boxes

(*parse >> <<boxes8 *)

let mod_float a b = a -. (b *. floor (a /. b))

let place_around ?(auto_inverse = false) r arc n f =
  let up = P.pt (zero, r) in
  let turn arc b =
    let b =
      if auto_inverse && arc > 90. && arc < 270. then Box.rotate 180. b else b
    in
    let b = Box.center up b in
    let t = Transform.rotate_around P.origin arc in
    Box.transform [ t ] b
  in
  let rec aux acc = function
    | x when x = n -> acc
    | n ->
        let a = float_of_int n *. arc in
        let a = mod_float a 360. in
        aux (turn a (f n) :: acc) (n + 1)
  in
  aux [] 0

let place_circle_ ?auto_inverse r n f =
  let arc = 360. /. float_of_int n in
  place_around ?auto_inverse r arc n f

let boxes8 =
  let yop d i = Box.circle (Box.tex (Printf.sprintf "$%i_{%02d}$" d i)) in
  let rec aux acc = function
    | 0 -> acc
    | i ->
        let n = float_of_int (i * 10) ** 0.8 in
        let r = cm (2.8 *. n /. 15.) in
        let n = int_of_float n in
        aux
          ( Box.draw (Box.group (place_circle_ ~auto_inverse:true r n (yop i)))
          :: acc )
          (i - 1)
  in
  Command.seq (aux [] 3)

(*parse >> <<boxes9 *)
(* aligne verticalement le barycentre [(west,5);(east,2)] *)
let boxes9 =
  let t = Picture.tex "f" in
  let a = Picture.tex "y" in
  Command.seq
    [
      draw_point ~color:Color.blue Point.origin;
      draw_point ~color:Color.red (Point.bpp (6., 6.));
      draw_point ~color:Color.green (Point.bpp (-6., -6.));
      draw_point ~color:Color.orange (Picture.north t);
      draw_point ~color:Color.red (Picture.south t);
      draw_point ~color:Color.green (Picture.north a);
      draw_point ~color:Color.cyan (Picture.south a);
      a;
      t;
    ]

(*parse >> *)

let () =
  List.iter
    (fun (i, fig) ->
      Metapost.emit ("boxes" ^ string_of_int i) (Picture.scale (Num.bp 3.) fig))
    [
      (1, boxes1);
      (2, boxes2);
      (3, boxes3);
      (4, boxes4);
      (5, boxes5);
      (6, boxes6);
      (7, boxes7);
      (8, boxes8);
      (9, boxes9);
    ]
