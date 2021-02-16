(* A small library to draw automata. *)

open Mlpost
open Path
open Point
open Num
open Command
open Box

(*parse <<togglescript>> *)
(* Nodes are boxes (type Box.t), created using functions such as "state" and
   "final" below. These boxes are given names using the labeled argument 
   ~name, for further reference in transition drawing. Nodes placement is
   left to the user, and is typically performed using alignment functions
   such as Box.hbox, Box.vbox or Box.tabularl (see examples below). 
 
   Given a set of placed nodes, that is a box containing nodes as sub-boxes,
   function "transition" draws a transition from one node to another, given
   their names. A label and its position are also given. Optional arguments
   outd and ind can be used to control the shape of the arrow (when it must
   not be a straight arrow). Function "loop" is used to draw a self-transition,
   drawn below a node (it could be easily generalized to draw a loop to the
   right of the node, etc.). Finally, function "initial" draws an incoming
   arrow to the left of a node (again, it could be generalized).
*)

let state = Box.tex ~dx:(bp 4.) ~style:Circle ~stroke:(Some Color.black)

let final = Box.box ~style:Circle

let transition states tex anchor ?outd ?ind x_name y_name =
  let x = Box.get x_name states and y = Box.get y_name states in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  Arrow.draw ~tex ~anchor (cpath ?outd ?ind x y)

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

let initial states name =
  let b = Box.get name states in
  let p = Box.west b in
  Arrow.draw (Path.pathp [ Point.shift p (Point.pt (cm (-0.3), zero)); p ])

(*** Examples ***************************************************************)

(*parse <<automata1 *)

let automata1 =
  let states =
    Box.hbox ~padding:(cm 1.4)
      [ state ~name:"0" "0"; final ~name:"1" (state "1") ]
  in
  seq
    [
      Box.draw states;
      transition states "0..9" `Top "0" "1";
      (*loop states "0..9" "1";
        initial states "0" *)
    ]

(*parse >> *)

(*parse <<automata2 *)

let automata2 =
  let states =
    Box.hbox ~padding:(cm 2.)
      [
        state ~name:"0" "0";
        state ~name:"1" "1";
        state ~name:"2" "2";
        state ~name:"3" "3";
        final ~name:"4" (state "4");
      ]
  in
  seq
    [
      Box.draw states;
      initial states "0";
      transition states "\\texttt{(}" `Top "0" "1";
      transition states "\\texttt{*}" `Top "1" "2";
      transition states "\\texttt{*}" `Top ~outd:25. ~ind:335. "2" "3";
      transition states "$a$" `Bot ~outd:205. ~ind:155. "3" "2";
      transition states "\\texttt{)}" `Top "3" "4";
      loop states "$b$" "2";
      loop states "*" "3";
    ]

(*parse >> *)

(****

let state name s = rect ~name  ~stroke:None (rect (tex ("$" ^ s ^ "$")))

let automata3 =
  let states = tabularl ~hpadding:(cm 1.) ~vpadding:(cm 1.)
    [[state "11" "S\\rightarrow E\\bullet";
      state "0" "S\\rightarrow\\bullet E"; 
      state "5" "E\\rightarrow\\texttt{int}\\bullet";
      ];
     [state "1" "E\\rightarrow\\bullet E\\texttt{+}E";
      state "3" "E\\rightarrow\\bullet\\texttt{(}E\\texttt{)}";
      state "2" "E\\rightarrow\\bullet\\texttt{int}";
     ];
     [state "4" "E\\rightarrow E\\bullet\\texttt{+}E";
      state "7" "E\\rightarrow E\\texttt{+}\\bullet E";
      state "6" "E\\rightarrow\\texttt{(}\\bullet E\\texttt{)}";
     ];
     [state "9" "E\\rightarrow E\\texttt{+}E\\bullet";
      state "10" "E\\rightarrow\\texttt{(}E\\texttt{)}\\bullet";
      state "8" "E\\rightarrow\\texttt{(}E \\bullet\\texttt{)}";
     ]
    ]
  in
  let eps = "$\\epsilon$" in
  let tt s = "\\texttt{" ^ s ^ "}" in
  [draw states;
   transition states "$E$" `Top "0" "11";
   transition states eps `Upleft "0" "1";
   transition states eps `Upright "0" "2";
   transition states eps `Left "0" "3";

   loop states eps "1";
   transition states "$E$" `Left "1" "4";
   transition states eps `Top "1" "3";
   transition states eps `Upright ~outd:20. "1" "2";

   transition states (tt "+") `Top "4" "7";

   transition states eps `Lowleft "7" "1";
   transition states eps `Right "7" "2";
   transition states eps `Right "7" "3";
   transition states "$E$" `Upleft "7" "9";

   transition states (tt "int") `Left "2" "5";

   transition states (tt "(") ~outd:(-0.) `Top "3" "6";
   transition states "$E$" `Left "6" "8";
   transition states (tt ")") `Top "8" "10";

   transition states eps ~outd:170. `Lowleft  "6" "1";
   transition states eps `Right  "6" "2";
   transition states eps ~outd:160. `Top  "6" "3";
  ]

****)

(*parse <<automata4 *)

let state name l =
  rect ~name ~stroke:None
    (rect
       (tex ("$\\begin{array}{l}" ^ String.concat "\\\\" l ^ "\\end{array}$")))

let automata4 =
  let states =
    tabularl ~hpadding:(cm 1.) ~vpadding:(cm 1.)
      [
        [
          state "1"
            [
              "S\\rightarrow\\bullet E\\#";
              "E\\rightarrow\\bullet E\\texttt{+}E";
              "E\\rightarrow\\bullet\\texttt{(}E\\texttt{)}";
              "E\\rightarrow\\bullet\\texttt{int}";
            ];
          state "2" [ "E\\rightarrow\\texttt{int}\\bullet" ];
          state "3"
            [
              "S\\rightarrow E\\bullet\\#";
              "E\\rightarrow E\\bullet\\texttt{+}E";
            ];
        ];
        [
          state "4"
            [
              "E\\rightarrow\\texttt{(}\\bullet E\\texttt{)}";
              "E\\rightarrow\\bullet E\\texttt{+}E";
              "E\\rightarrow\\bullet\\texttt{(}E\\texttt{)}";
              "E\\rightarrow\\bullet\\texttt{int}";
            ];
          state "5"
            [
              "E\\rightarrow\\texttt{(}E \\bullet\\texttt{)}";
              "E\\rightarrow E\\bullet\\texttt{+}E";
            ];
          state "6"
            [
              "E\\rightarrow E\\texttt{+}\\bullet E";
              "E\\rightarrow\\bullet E\\texttt{+}E";
              "E\\rightarrow\\bullet\\texttt{(}E\\texttt{)}";
              "E\\rightarrow\\bullet\\texttt{int}";
            ];
        ];
        [
          empty ();
          state "7" [ "E\\rightarrow\\texttt{(}E\\texttt{)}\\bullet" ];
          state "8"
            [
              "E\\rightarrow E\\texttt{+}E\\bullet";
              "E\\rightarrow E\\bullet\\texttt{+}E";
            ];
        ];
      ]
  in
  let tt s = "\\texttt{" ^ s ^ "}" in
  seq
    [
      draw states;
      initial states "1";
      transition states "$E$" `Top ~outd:30. "1" "3";
      transition states (tt "(") `Left "1" "4";
      transition states (tt "(") `Top ~outd:150. "6" "4";
      transition states (tt "int") `Bot "1" "2";
      transition states (tt "int") `Lowright "4" "2";
      transition states (tt "int") `Lowleft "6" "2";
      transition states (tt "+") `Right "3" "6";
      transition states (tt "+") `Top "5" "6";
      transition states "$E$" `Left ~outd:(-110.) "6" "8";
      transition states (tt "+") `Right ~outd:70. "8" "6";
      transition states "$E$" `Top "4" "5";
      transition states (tt ")") `Left "5" "7";
      loop states (tt "(") "4";
    ]

(*parse >> *)

let () = Metapost.emit "automata1" (Picture.scale (Num.bp 3.) automata1)

let () = Metapost.emit "automata2" (Picture.scale (Num.bp 3.) automata2)

let () = Metapost.emit "automata4" (Picture.scale (Num.bp 2.) automata4)
