(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: tiles.ml,v 1.7 2004/09/07 13:52:28 barnier Exp $ *)

(* 
   How to cover a square with square tiles of the given sizes
   From http://www.icparc.ic.ac.uk/eclipse/examples/square_tiling.pl.txt

   4 data samples numbered 0 to 3 to pass as argument on the command line
*)

open Facile
open Easy

(* [| ( [|Tile sizes|], Big square size) |] *)
let data = [|
  ([|2; 1; 1; 1; 1; 1|], 3);
  ([|10; 9; 7; 6; 4; 4; 3; 3; 3; 3; 3; 2; 2; 2; 1; 1; 1; 1; 1; 1|], 19);
  ([|50; 42; 37; 35; 33; 29; 27; 25; 24; 19; 18; 17; 16; 15; 11; 9; 8; 7; 6; 4; 2|], 112);
  ([|81; 64; 56; 55; 51; 43; 39; 38; 35; 33; 31; 30; 29; 20; 18; 16; 14; 9; 8; 5; 4; 3; 2; 1|], 175);
  |]


let tile (sizes, size) =
  let n = Array.length sizes in
  let var i = Fd.interval 0 (size - sizes.(i)) in
  let xs = Array.init n var
  and ys = Array.init n var in

  let no_overlap i j =
     Cstr.post
      ((fd2e xs.(j) +~ i2e sizes.(j) <=~ fd2e xs.(i)) (* j on left of i *)
	||~~ (fd2e xs.(j) >=~ fd2e xs.(i) +~ i2e sizes.(i)) (* j on right of i *)
	||~~ (fd2e ys.(j) +~ i2e sizes.(j) <=~ fd2e ys.(i)) (* j below i *)
	||~~ (fd2e ys.(j) >=~ fd2e ys.(i) +~ i2e sizes.(i))) in (* j above i *)

  for i = 0 to n-1 do (* For all squares *)
    for j = i+1 to n-1 do (* For all ordered pairs of squares *)
      no_overlap i j
    done
  done;
   
  (* Redundant capacity constraints *)
  for i = 0 to size-1 do (* For all verticals and horizontals *)
    let full_line xy =
      let intersections =
       	Array.init n
	  (fun j -> Interval.is_member xy.(j) (i-sizes.(j)+1) i) in
      Cstr.post (Arith.scalprod_fd sizes intersections =~ i2e size) in
    
    full_line xs;
    full_line ys
  done;
  
  let min_min = (* minimum min strategy *)
    Goals.Array.choose_index (fun a1 a2 -> Var.Attr.min a1 < Var.Attr.min a2) in

  let try_min v = (* Instantiates to min or remove min *)
    match Fd.value v with
      Unk attr ->
	Goals.unify v (Var.Attr.min attr)
          ||~
	Goals.atomic
	  (fun () -> Fd.refine v (Domain.remove_min (Var.Attr.dom attr)))
      | _ -> failwith "Tiles.try_min: v should be bound" in
  
  let goal =
    Goals.Array.forall ~select:min_min try_min xs
      &&~
    Goals.Array.forall ~select:min_min try_min ys in
  
 if Goals.solve goal then begin
   Printf.printf "size: x y\n\n";
   for i = 0 to n - 1 do
     Printf.printf "%d: %a %a\n" sizes.(i) Fd.fprint xs.(i) Fd.fprint ys.(i)
   done
 end else
   Printf.printf "No solution\n";;

let _ =
  (* Gc.set {(Gc.get ()) with Gc.space_overhead = 600};  (* makes a big difference... *) *)
    tile data.(int_of_string Sys.argv.(1))
