open Parts
open Convenience

(******************************************************************)
(*                  ENUMERATIONS                                  *)
(******************************************************************)

(* An enumeration *)
type 'a t = {
    (* Number of elements. None means infinity. *)
    cardinal : Z.t option ;
    
    (* Number of parts. None means infinity. *)
    n_parts  : int option ;

    (* An extensible array of parts, in order to compute parts lazily. *)
    parts : ('a part) ExtArray.t ;
    
    (* Function used to create a new part when necessary. *)
    new_part : (int -> 'a part) ;

    (* A title, mostly for debugging. *)
    title : string Lazy.t ;
  }

let cardinal exen = exen.cardinal

(******************************************************************)
(*             ACCESSING ENUMERATIONS                             *)
(******************************************************************)

(* Check that the required part index exists in the given enumeration. *)
let check_array_index index exen =
  match exen.n_parts with
  | None -> true
  | Some n -> index < n

(* Get (possibly by forcing evaluation) the part corresponding to the given index. *)
let get_part exen array_index =

  if check_array_index array_index exen then
    let part = ExtArray.get exen.parts array_index in

    let part =
      if part == uninitialized_part then
	begin
	  (* This cell is uninitialized. Set it first. *)
	  let newcell = exen.new_part array_index in
	  ExtArray.set exen.parts array_index newcell ;
	  newcell
	end  
      else
	part
    in

    part

  else
      empty_part

(* Find in which part of an enumearation an index holds. *)
let rec get_in_parts exen value_index array_index =

  let part = get_part exen array_index in

  (* Is the index in range of this part ? *)
  if part.p_cardinal <= value_index then
    (* No. Go to next part. *)
    get_in_parts exen (value_index -- part.p_cardinal) (array_index + 1)
  else
    (* OK. Get the value in this part. *)
    part.compute value_index

(* Gets a value, given an absolute index. *)
let get exen index =
  begin match exen.cardinal with
  | None -> ()
  | Some max -> 
      if max <= index then 
	failwith (Printf.sprintf "Exenum.get: index is out of bounds : %s / %s" (sob index) (sob (max -- bigone)))
  end ;
  
  get_in_parts exen index 0

(******************************************************************)
(*              BUILDING SIMPLE ENUMERATIONS                      *)
(******************************************************************)

(* An enumeration from a single part. *)
let from_single_part title a_part =
  let parts = ExtArray.create 1 empty_part in
  ExtArray.set parts 0 a_part ;  

  { cardinal = Some (a_part.p_cardinal) ;
    n_parts  = Some 1 ;
    parts ;
    (* This enumeration is finite. We should never need to create a new cell. *)
    new_part = (fun _ -> assert false) ;
    title }

(* An enumeration from a list of values. *)
let from_list ?(name="unnamed") values = from_single_part (lazy name) (part_from_list values)


(******************************************************************)
(*           ALGEBRAIC OPERATIONS ON ENUMERATIONS                 *)
(******************************************************************)

(* Pay *)
let pay l_exen =

  let parts = ExtArray.create 1 uninitialized_part in
  ExtArray.set parts 0 empty_part ;

  (* On the first call to new_part, we will check that the argument function is indeed infinite. *)
  let is_infinite = lazy
      ((Lazy.force l_exen).cardinal = None)
  in

  let new_part array_index =
    assert (array_index > 0) ;
    assert (Lazy.force is_infinite) ;
    get_part (Lazy.force l_exen) (array_index - 1)
  in

  { cardinal = None ;
    n_parts = None ;
    parts ;
    new_part ;
    title = lazy ("pay (...)") }

(* Maps an enumeration *)
let map exen f = 

  let parts = ExtArray.create 1 uninitialized_part in

  let new_part array_index =
    let part = get_part exen array_index in
    map_part f part
  in

  { cardinal = exen.cardinal ;
    n_parts = exen.n_parts ;
    parts ;
    new_part ;
    title = lazy ("map (" ^ Lazy.force exen.title ^ ")") }


(* Finite sub-enumeration *)
let sub ~max exen =

  let newcardinal =
    match exen.cardinal with
    | None -> Some max
    | Some bound -> 
	if max <= bound then Some max
	else Some bound
  in

  { exen with cardinal = newcardinal }

(*** DISJOINT UNION ***)

(* Compute the possibly infinite sum of cardinals. *)
let rec sum_cardinals acu = function
  | [] -> Some acu
  | exen :: exens ->
      begin match exen.cardinal with
      | None -> None (* Infinite *)
      | Some n -> sum_cardinals (acu ++ n) exens
      end

(* max, taking account of infinity (None) *)
let infmax a b =
  match (a,b) with
  | (None, _) | (_, None) -> None
  | (Some x, Some y) -> Some (max x y)

(* Returns the maximal number of parts of these enumerations. *)
let max_parts exens = myfold exens (Some 0) (fun acu exen -> infmax acu exen.n_parts)

(* Disjoint union of enumerations. *)
let union exens =
  let cardinal = sum_cardinals bigzero exens in  
  let parts = ExtArray.create 1 uninitialized_part in
  let new_part array_index = union_parts (List.map (fun ex -> get_part ex array_index) exens) in

  { cardinal ;
    n_parts = max_parts exens ;
    parts ;
    new_part ;
    title = lazy ("union (" ^ (sep (fun e -> Lazy.force e.title) ", " exens) ^ ")") }


(*** CARTESIAN PRODUCT ***)

(* Compute the possibly infinite product of cardinals. 
 * If one of them is empty, the result is empty. *)
let rec prod_cardinals acu = function
  | [] -> acu
  | exen :: exens ->
      begin match (exen.cardinal, acu) with
      | None, _ -> prod_cardinals None exens
      | (Some z,_) when z = bigzero -> Some bigzero
      | Some n, None -> prod_cardinals None exens
      | Some n, Some k -> prod_cardinals (Some (n ** k)) exens
      end

(* Find all vectors that reach the given max depth. 
 * Input: the expected (max) depth. 
 *        the vector of maximal indexes reachable by each coordinate.
 *          (these maximal indexes are guaranteed to be <= max depth). *)

(* Algorithm: brut force enumeration of all tuples.
 * An optimization, though: we know if the max depth has been reached in the current temporary vector,
 * and we know at which position is the 'last chance' of reaching the max depth. *)

(* For efficiency, rev_max is the reversed list of max_indexes. 
 * flag indicates if max depth has been reached in the current temporary vector. 
 * acu is the list of all vectors. 
 * pos is the current position in the reversed max_indexes list. *)
let rec find_vectors_aux last_chance depth flag acu temp_vect pos rev_max = 

  match rev_max with
  (* This vector is finished. Add it to the accumulator. *)
  | [] ->
      assert flag ; (* The maximal depth must have been reached. *)
      temp_vect :: acu

  | current_vup :: others ->

      let (vlow, vup) =
	(* Particular case: if this is the last chance and flag is false. *)
	if (not flag) && pos = last_chance then
	  begin
	    assert (current_vup = depth) ;
	    (depth, depth)
	  end

	else (0, current_vup)
      in

      (* Iterate over all these values. *)
      let racu = ref acu in
      
      for current_index = vlow to vup do
	racu := find_vectors_aux last_chance depth (flag || current_index = depth) !racu (current_index :: temp_vect) (pos+1) others ;
      done ;

      (* Finished. *)
      !racu

      
let find_vectors depth rev_max_indexes = 
 
  (* Find the position of the last chance. *)
  let (_, last_chance) = myfold rev_max_indexes (0, (-1)) 
      begin fun (current_pos, last) v -> if v = depth then (current_pos+1, current_pos) else (current_pos+1, last) end
  in
  assert (last_chance >= 0) ; (* The maximal depth must be reachable. We never try to build a part that cannot be built. *)

  find_vectors_aux last_chance depth false [] [] 0 rev_max_indexes

(* Maps a vector to a rev_list of parts. *)
let vector_to_part_list exens vector = 
  (try myrevmap2 exens vector get_part with _ -> assert false)

(* Creates a new part with depth array_index corresponding to the cartesian product of the given enumerations. *)
let prod_parts array_index exens =
  (* This part is the union of cartesian products of PARTS, at least one of them being at the given depth. *)

  (* Def: a 'vector' is a vector of indexes. Each index corresponds to an enumeration. 
   * The length of the vector = the length of the list 'exens'. *)

  assert (exens <> []) ;

  (* Get the rev_vectors of maximal part indexes, but never further than array_index. *)
  let max_revindexes = myrevmap exens
      begin fun ex ->
	match ex.n_parts with
	| None -> array_index (* Infinite *)
	| Some n -> min (n-1) array_index
      end
  in

  (* List all vectors that satisfy the current depth (i.e. at least one index is the expected array_index). *)
  let all_vectors = find_vectors array_index max_revindexes in

  assert (all_vectors <> []) ;
  
  (* Map these vectors to part lists. *)
  let all_rev_parts = List.rev_map (vector_to_part_list exens) all_vectors in

  (* Map each part list to a product part. 
   * product_parts reverses the tuples. *)
  let all_prod_parts = List.rev_map product_parts all_rev_parts in

  (* Get the union *)
  union_parts all_prod_parts

(* Cartesian product. *)
let product exens =
  
  let cardinal = prod_cardinals (Some bigone) exens in
  let parts = ExtArray.create 1 uninitialized_part in

  { cardinal ;
    n_parts = max_parts exens ;
    parts ;
    new_part = (fun array_index -> prod_parts array_index exens) ;
    title = lazy ("product (" ^ (sep (fun e -> Lazy.force e.title) ", " exens) ^ ")") }

