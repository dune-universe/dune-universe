(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

module type OrderedType = sig
  type t
  val compare : t -> t 
    -> [`Left | `Right | `Includes | `Included | `Same | `Overwrap]
  val split : t -> by:t -> (t * t) option
  val format : Format.formatter -> t -> unit
end

module Make(Ord : OrderedType) = struct

  type elem = Ord.t
  type 'a node = Node of elem * 'a

  module rec Node : (Xset.OrderedType with type t = NodeSet.t node) = struct
    type t = NodeSet.t node
    let compare (Node (t1, _)) (Node (t2, _)) = 
      match Ord.compare t1 t2 with
      | `Left -> -1
      | `Right -> 1
      | _ -> 0
    let format ppf = function
      | Node (e, _) -> Ord.format ppf e
  end
  
  and NodeSet : Xset.S with type elt = 
    Node.t = Xset.Make(Node)

  include NodeSet

  let rec add_node (Node (elem, t0) as node) t = 
    match unsafe_find node t with 
    | None -> add node t
    | Some (Node (elem', t') as node') ->
	match Ord.compare elem elem' with
	| `Left | `Right -> assert false
	| `Includes | `Same -> 
	    add_node (Node (elem, add_node node' t0)) (remove node' t)
	| `Included ->
	    add (Node (elem', add_node node t')) (remove node' t)
	| `Overwrap -> 
	    match Ord.split elem ~by:elem' with
	    | None -> assert false
	    | Some (elem_left, elem_right) ->
		add_node (Node (elem_left, t0))
		  (add_node (Node (elem_right, t0)) t)

  let add_elem elem t = add_node (Node (elem, empty)) t  

  let rec find_path_contains_aux path node t =
(*
    Format.eprintf "FIND: %a in @[%a@]@."
      (fun ppf node -> match node with Node (e, _) -> Ord.format ppf e) node
      NodeSet.unsafe_dump t;
*)
    match unsafe_find node t with
    | None -> path
    | Some (Node (elem', t')) ->
	find_path_contains_aux ((elem', t') :: path) node t' 

  let find_path_contains elem t = 
    find_path_contains_aux [] (Node (elem, empty)) t

  let rec iter_elem ~parent f =
    NodeSet.iter (fun (Node (elem, t')) ->
      f ~parent elem;
      iter_elem ~parent: (Some elem) f t')

  let iter_elem f = 
    iter_elem ~parent:None f
    
end
