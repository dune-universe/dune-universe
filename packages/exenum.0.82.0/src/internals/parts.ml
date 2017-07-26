open Convenience

(******************************************************************)
(*                        PARTS                                   *)
(******************************************************************)

(* A part is a _finite_ sub-enumeration corresponding to a given depth.
 * The depth is roughly the number of constructors. *)
type 'a part = {
    (* Cardinal of this part. *)
    p_cardinal : Big_int.big_int ;
    
    (* Compute the element corresponding to the given index. *)
    compute : (Big_int.big_int -> 'a) ;
  }

let get_cardinal p = p.p_cardinal

(* Debug: we try two modes. Standard and Shuffled. 
 * In shuffled mode, values in parts are (deterministically) shuffled. *)
type mode = Standard | Shuffled
let mode = Shuffled

let shuffle part =
  match mode with
  | Standard -> part
  | Shuffled -> 
      { p_cardinal = part.p_cardinal ;
	compute    = Shuffle.compute_shuffle part.p_cardinal part.compute }

(* The EMPTY part *)
let empty_part = {
  p_cardinal = bigzero ;
  compute = (fun _ -> assert false) ;
}

(* A DUMMY part, for cells that remain to be initialized. *)
let uninitialized_part = {
  p_cardinal = bigzero ;
  compute = (fun _ -> assert false) ;
}

(* Maps a part through a presumably bijective function f. *)
let map_part f part =
  { p_cardinal = part.p_cardinal ;
    compute    = (fun index -> f (part.compute index)) }

(* Builds a part from a finite list of values. *)
let part_from_list values =
  let avalues = Array.of_list values in
  { p_cardinal = boi (Array.length avalues) ;
    compute = (fun n -> avalues.(iob n)) }

(*** Union ***)

(* Finds in which part (of the given list) is the given index. *)
let rec standard_compute_union_aux partlist index =
  match partlist with
  | [] ->
      (* Index is out of part list. Cannot happen. *)
      assert false
  | p :: ps ->
      if p.p_cardinal <== index then standard_compute_union_aux ps (index -- p.p_cardinal)
      else (p, index)

(* Disjoint union of these parts. *)
let union_parts parts =

  let whichpart = standard_compute_union_aux parts in

  let compute index = 
    let (p, index) = whichpart index in
    p.compute index
  in

  (* The cardinal of the disjoint union is the sum of cardinals. *)
  let pre_result =
    { p_cardinal = myfold parts bigzero (fun acu p -> acu ++ p.p_cardinal) ;
      compute }
  in
  shuffle pre_result


(*** Product ***)

(* Split the index into coordinates in the different parts.
 * We use div & mod. *)
let rec compute_product_vector index part_revvector acu =
  match part_revvector with
  | [] -> acu
  | pcard :: others ->
      assert (sign pcard = 1) ;
      let (index', mod') = quomod index pcard in
      compute_product_vector index' others (mod' :: acu)

let standard_compute_product_aux parts =
  let part_revvector = myrevmap parts get_cardinal in
  fun index -> compute_product_vector index part_revvector []

(* Cartesian product of these parts. 
 * Caution! compute returns a (product) value in the reversed order of the part list. *)
let product_parts parts =
  
  let whichindexes = standard_compute_product_aux parts in

  let compute index =
    let vector = whichindexes index in
    (* Result is reversed. *)
    myrevmap2 vector parts (fun index p -> p.compute index)    
  in

  let p_cardinal = myfold parts bigone (fun acu p -> acu ** p.p_cardinal) in

  (* The cardinal of the product it the product of cardinals. *)
  let pre_result = 
    { p_cardinal ;
      compute }
  in
  shuffle pre_result


