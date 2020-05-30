open Set

module type S = sig
  include S
  val of_list : elt list -> t

  val (+) : t -> t -> t (** union *)
    
  val (-) : t -> t -> t (** diff *)
    
  val one : elt -> t (** singleton *)
    
  val unions : t list -> t
    
  val to_list : t -> elt list (** elements *)

  val unsafe_top_node : t -> (t * elt * t) option

  val unsafe_middle : t -> elt option (** find the middle element in [t] *)

  val unsafe_find : elt -> t -> elt option (** find the "same" element as [elt] in [t] *)

  val unsafe_find_node : elt -> t -> (t * elt * t) option (** find the node of the "same" element as [elt] in [t] *)
end

module Make(O : OrderedType) = struct
  include Make(O)

  (* Warning! Unsafe operation!
     Currently, there is no easy way to get the binary tree structure from a set,
     though it is very handy for efficient binary search.
     The following easily breaks when the internal implementation of Set.t is changed. 
     If the program crashes, check this first.
  *) 
  type t_internal = Empty | Node of t * elt * t * int
  let unsafe_internal t = (Obj.magic t : t_internal) 

  let of_list = 
    let rec of_list st = function
      | [] -> st
      | x::xs -> of_list (add x st) xs
    in
    of_list empty

  let (+) = union
  let (-) = diff
  let one = singleton
  let unions = List.fold_left union empty
  let to_list = elements

  let _dummy () =  [Empty ; Node (assert false, assert false, assert false, 0)]

  let unsafe_top_node t = match unsafe_internal t with
    | Empty -> None
    | Node (left, v, right, _) -> Some (left, v, right)

  let unsafe_middle t = match unsafe_internal t with
    | Empty -> None
    | Node (_, v, _, _) -> Some v

  let rec unsafe_find elt t = match unsafe_internal t with
    | Empty -> None
    | Node (left, elt', right, _) -> 
	match O.compare elt elt' with 
	| 0  -> Some elt'
	| -1 -> unsafe_find elt left
	| 1  -> unsafe_find elt right
	| _  -> assert false

  let rec unsafe_find_node elt t = match unsafe_internal t with
    | Empty -> None
    | Node (left, elt', right, _) -> 
        (* Format.eprintf "looking %a@." Ord.format elt'; *)
	match O.compare elt elt' with 
	| 0  -> Some (left, elt', right)
	| -1 -> unsafe_find_node elt left
	| 1  -> unsafe_find_node elt right
	| _  -> assert false
end

