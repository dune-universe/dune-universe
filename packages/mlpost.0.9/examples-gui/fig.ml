open Mlpost
open Num
open Path
open Command

module Edit = struct
  open Glexer

  let file = "fig.edit"

  let elements = ref []

  let table = Hashtbl.create 17

  let () =
    if Sys.file_exists file then (
      elements := Glexer.read_file file;
      let add = function
        | (Num (s, _) | Point (s, _, _)) as e -> Hashtbl.add table s e
      in
      List.iter add !elements )

  let mk_num = function
    | Pt -> Num.pt
    | Cm -> Num.cm
    | Mm -> Num.mm
    | Bp -> Num.bp
    | Inch -> Num.inch

  let num s v dimension =
    try
      match Hashtbl.find table s with
      | Num (_, (f, dim)) -> mk_num dim f
      | Point _ -> invalid_arg ("already a point of name " ^ s)
    with Not_found ->
      let e = Num (s, (v, dimension)) in
      elements := e :: !elements;
      Hashtbl.add table s e;
      mk_num dimension v

  let point s v1 dim1 v2 dim2 =
    try
      match Hashtbl.find table s with
      | Num _ -> invalid_arg ("already a num of name " ^ s)
      | Point (_, (n1, d1), (n2, d2)) -> Point.pt (mk_num d1 n1, mk_num d2 n2)
    with Not_found ->
      let e = Point (s, (v1, dim1), (v2, dim2)) in
      elements := e :: !elements;
      Hashtbl.add table s e;
      Point.pt (mk_num dim1 v1, mk_num dim2 v2)

  let () = at_exit (fun () -> Glexer.write_file file !elements)
end

(* Exemple d'utilisation de Gmlpost *)

open Color

(* un histogramme *)

let fill = [ lightblue; lightgreen; lightyellow; lightred ]

(* le padding est une valeur éditable *)

let padding = Edit.num "padding" 5. Glexer.Bp

let hist =
  Hist.simple ~width:(bp 50.) ~height:(bp 100.) ~fill ~padding
    [ 1.; 2.; 3.; 4. ]

(* et sa légende *)

let leg =
  Legend.legend
    [
      (lightblue, "2006");
      (lightgreen, "2007");
      (lightyellow, "2008");
      (lightred, "2009");
    ]

(* la légende est positionnée au point p1 qui est éditable *)

let p1 = Edit.point "legend" 100. Glexer.Bp 100. Glexer.Bp

let pic = Command.draw_pic (Picture.shift p1 (Picture.scale (bp 0.5) leg))

(* la figure est l'ensemble de l'histogramme et de sa légende *)

let example = hist ++ pic

let () = Metapost.emit "example" example
