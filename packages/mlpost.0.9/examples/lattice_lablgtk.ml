open Mlpost
open Command
open Num
open Helpers

(** Copy from misc.ml!!! *)

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
  let s = Bytes.of_string s in
  let len = fold_bit (fun l _ -> l + 1) 0 n in
  let w = Bytes.create len in
  let j = ref 0 in
  for i = 0 to Bytes.length s - 1 do
    if n land (1 lsl i) != 0 then (
      Bytes.set w !j (Bytes.get s i);
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

let lattice s = draw (subwords s)

(** End of the copy *)

open Mlpost_lablgtk
module L = Mlpost_lablgtk.Interface

let _ = GMain.Main.init ()

let word = ref "abcd"

let int = L.new_interface ()

let () = L.create_text int ~label:"lattice of subwords of" !word (( := ) word)

let aa ~width ~height _ =
  let p = Point.pt (Num.divf width 2., Num.divf height 2.) in
  [ Transform.shifted p ]

let aa2 ~width ~height pic =
  let p = Point.pt (Num.divf width 2., Num.divf height 2.) in
  [ Transform.shifted (Point.sub p (Picture.ctr pic)) ]

let () = L.add_pic ~auto_aspect:aa_fit_page int (fun () -> lattice !word)

let () = L.main int
