open Mlpost
open Num
open Box
open Color

let fill = rgb8 253 215 117

let node s =
  round_rect ~stroke:None ~name:s (round_rect ~fill (tex ("\\sf " ^ s)))

let hbox = hbox ~padding:(bp 20.)

let vbox = vbox ~padding:(bp 20.)

let alt_ergo =
  let b1 =
    vbox [ hbox [ node "SMT parser"; node "Why parser" ]; node "Typing" ]
  in
  let b2 =
    round_rect ~dx:(bp 20.) ~dy:(bp 5.) ~fill:(color "blanched almond")
      (hbox ~pos:`Bot
         [
           vbox [ node "SAT-solver"; hbox [ node "Matching"; node "CC(X)" ] ];
           vbox [ tex "\\sf main loop"; node "UF(X)" ];
         ])
  in
  let c_typing = ctr (get "Typing" b1) in
  let c_sat = ctr (get "SAT-solver" b2) in
  let v = Point.add (Point.sub c_sat c_typing) (Point.pt (zero, bp 40.)) in
  let b = group [ shift v b1; b2 ] in
  let arrow x y = Helpers.box_arrow (get x b) (get y b) in
  [
    draw b;
    arrow "SMT parser" "Typing";
    arrow "Why parser" "Typing";
    arrow "Typing" "SAT-solver";
    arrow "SAT-solver" "Matching";
    arrow "SAT-solver" "CC(X)";
    arrow "Matching" "CC(X)";
    arrow "CC(X)" "UF(X)";
  ]

let () = Metapost.emit "alt_ergo" (Command.seq alt_ergo)

(*
Local Variables: 
compile-command: "mlpost -latex slides.tex -xpdf alt_ergo.ml"
End: 
*)
