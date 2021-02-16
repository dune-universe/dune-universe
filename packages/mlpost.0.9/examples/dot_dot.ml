(* mlpost -contrib dot dot.ml *)

open Mlpost
open Mlpost_dot
module Pi = Picture
open Command

(*parse <<togglescript>> *)

(*parse <<dot_dot1 *)

module G = Dot.Make (Picture)

let dot1 =
  let a = G.mknode (Pi.tex "Bonjour voici un A") in
  let b = G.mknode (Pi.tex "Bonjour voici un $\\frac{B}{B}$") in
  let c = G.mknode (Pi.tex "Bonjour voici un C") in
  let d = G.mknode (Pi.tex "Au revoir d") in
  let edges = [ (a, b); (b, c); (c, a); (d, b); (d, c); (a, d) ] in
  let nodes, edges = G.place [ a; b; c; d ] edges in
  seq nodes ++ seq (List.map Arrow.simple edges)

(*parse >> <<dot_dot2 *)

module G2 = Dot.Make (Box)

let dot2 =
  let tex s = Box.rect (Box.tex s) in
  let a = G2.mknode (tex "Bonjour voici un A") in
  let b = G2.mknode (tex "Bonjour voici un $\\frac{B}{B}$") in
  let c = G2.mknode (tex "Bonjour voici un C") in
  let d = G2.mknode (tex "Au revoir d") in
  let edges = [ (a, b); (b, c); (c, a); (d, b); (d, c); (a, d) ] in
  let nodes, edges = G2.place [ a; b; c; d ] edges in
  seq (List.map Box.draw nodes) ++ seq (List.map Arrow.simple edges)

(*parse >> *)

let () =
  List.iter
    (fun (n, f) -> Metapost.emit n f)
    [ ("dot_dot1", dot1); ("dot_dot2", dot2) ]
