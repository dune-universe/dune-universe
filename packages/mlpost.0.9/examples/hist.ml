open Mlpost
open Num
open Color
open Box

(*parse <<togglescript>> *)

(*parse <<hist1 *)
let hist1 = Hist.simple [ 3.; 1.; 6. ]

(*parse >> <<hist2 *)
let hist2 = Hist.simple ~width:(bp 30.) ~height:(bp 40.) [ 3.; 1.; 6. ]

(*parse >> <<hist3 *)
let hist3 = Hist.simple ~padding:(bp 10.) [ 3.; 1.; 6. ]

(*parse >> <<hist4 *)
let hist4 =
  Hist.simple ~fill:[ blue; white; red ] [ 3.; 1.; 6.; 2.; -1.; 7.; 6. ]

(*parse >> <<hist5 *)
let hist5 =
  let hlabel =
    List.map Picture.tex [ "2000"; "2001"; "2002"; "2003"; "2004"; "2005" ]
  in
  let vlabel n _ =
    if n mod 10 = 0 then Some (Picture.tex (string_of_int n)) else None
  in
  Hist.simple ~width:(bp 300.) ~vcaption:(Picture.tex "students") ~hlabel
    ~vlabel
    [ 45.; 50.; 62.; 80.; 72.; 61. ]

(*parse >> <<hist6 *)
let hist6 =
  Hist.simple ~histlabel:(`Top, Hist.Values) [ 4.5; 5.0; 6.2; 8.; 7.2; 6.1 ]

(*parse >> <<hist7 *)
let hist7 =
  let pics =
    List.map Picture.tex [ "2000"; "2001"; "2002"; "2003"; "2004"; "2005" ]
  in
  Hist.simple ~width:(bp 210.)
    ~histlabel:(`Top, Hist.User pics)
    [ 4.5; 5.0; 6.2; 8.; 7.2; 6.1 ]

(*parse >> <<hist8 *)
let hist8 = Hist.compare [ [ 1.; 5.; 6.; 5.; 3. ]; [ 1.; 2.; 3.; 6.; -1. ] ]

(*parse >> <<hist9 *)
let hist9 =
  Hist.stack
    ~fill:[ lightred; lightblue; lightyellow; lightgreen ]
    [ [ 4.; 5.; 5. ]; [ 8.; 3.; 1. ]; [ 2.; 8.; 1.; 4. ] ]

(*parse >> <<hist10 *)
let hist10 =
  Hist.stack ~perspective:true ~padding:(bp 5.)
    ~fill:[ lightred; lightblue; lightyellow; lightgreen ]
    ~histlabel:(`Center, Hist.Values)
    [ [ 4.; 5.; 5. ]; [ 8.; 3.; 1. ]; [ 2.; 8.; 1.; 4. ] ]

(*parse >> *)

let _ =
  List.iter
    (fun (name, fig) -> Metapost.emit name fig)
    [
      ("hist1", hist1);
      ("hist2", hist2);
      ("hist3", hist3);
      ("hist4", hist4);
      ("hist5", hist5);
      ("hist6", hist6);
      ("hist7", hist7);
      ("hist8", hist8);
      ("hist9", hist9);
      ("hist10", hist10);
    ]
