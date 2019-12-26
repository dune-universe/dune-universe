(** trie tree
  This module implements strict impure trie tree data structure.
  *)

module type Intf =
  sig
    (** type of path point *)
    type path

    (** type of trie node *)
    type 'a node

    (** create a new trie tree with an optional element *)
    val create : 'a option -> 'a node

    (** returns the value associated with the path *)
    val get : 'a node -> path -> 'a option

    (** associate the value with the path *)
    val set : 'a node -> path -> 'a -> unit

    (** remove an association of the path *)
    val unset : 'a node -> path -> unit

    (** returns the sub node associated with the path *)
    val sub : 'a node -> path -> 'a node option

    (** returns whether the node is a leaf of the tree *)
    val is_leaf : 'a node -> bool
  end

module Make (H : Hashtbl.HashedType) : Intf with type path= H.t list

