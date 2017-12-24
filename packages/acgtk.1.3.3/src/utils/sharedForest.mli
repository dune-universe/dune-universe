module SharedForest :
sig
  (** This type is the type of addresses of forests. It is a list of
      (position in the forest,position as a child). *)
  type address=(int*int) list
  (** This is the type of relative path from one forest to another
      one. The first argument is the number of steps to move up, then
      second argument is the address to reach from this point. *)
  type relative_path=int*address

  (** [diff add add'] returns the relative path to go from the
      forest (subtree) wich occurs at address [add] to the forest
      (subtree) wich occurs at address [add']. *)
  val diff : address -> address -> relative_path

  (** [path_to_string p] returns a string describing the path [p].*)
  val path_to_string : relative_path -> string
    
  (** [address_to_string add] returns a string describing the
      address [add]. *)
  val address_to_string : address -> string

  (** The type of a stack. *)  
  type 'a stack='a list
  (** A list context is a stack *)
  type 'a list_context ='a stack
    
  (** a focused list is a pair of a list context and of a list *)
  type 'a focused_list = 'a list_context * 'a list
    
    
  (** Recursive definition of a shared forest. *)
  type 'a forest = 'a tree focused_list
  and 'a tree = Node of 'a * 'a child list
  and 'a child = 
  | Forest of 'a forest
  | Link_to of relative_path

  (** Defintion of a "forest zipper" *)
  type 'a forest_zipper = 
  | Top of ('a tree) focused_list * int
  | Zip of
      'a * 
	(* The first element is the label of the node *)
	('a child) focused_list *
	(* the focused list of children of the tree. Just as for tree
	   zippers *)
	('a tree) focused_list *
	(* the focused list of the focuses child: a forest *)
	int *
	(* the position of the tree under focus in the current forest *)
	'a forest_zipper *
	(* the forest context *)
	'a forest_zipper option *
	(* a local context describing the way to reach the current
	   tree from top in case it was reached after a [Link_to] move,
	   so that if some other [Link_to] is met under thus subtree that
	   points higher, it goes to the right place. *)
	address
    (* The address of the current tree. Actually not used. *)

  (** Type definition for the focused forests *)
  type 'a focused_forest = 'a forest_zipper * 'a  tree
    
      (** Type definition for standard trees *)
  type 'a simple_tree = SimpleTree of 'a * 'a simple_tree list
    
  (** Type definition for standard tree zippers *)
  type 'a zipper =  ZTop | Zipper of ('a * 'a simple_tree focused_list * 'a zipper)
      
  (** Type definition for standard focused trees *)
  type 'a focused_tree = 'a zipper * 'a simple_tree
    
  (** An abstract type to give access to resumption when
      trees are built from a forest *)
  type 'a resumption (*= 'a simple_resumption * 'a delayed_resumption *)

  (** [empty] is the empty resumption *)
  val empty : 'a resumption
    
  (** [fold_depth_first (f,g) t] recursively computes [(g a) b_1
      .... b_n] where [a=f t_0] and [b_i= f t_i] and [t] is a tree of
      node [t_0] and of children [t_1...t_n]*)
  val fold_depth_first:  (('a -> 'b) * ('b -> 'b -> 'b)) -> 'a simple_tree -> 'b

  (** [init forest] builds the resumption with all the focused
      forest focusing on each of the tree of [forest] *)
  val init : 'a tree list -> 'a resumption
    
  (** [resumption resume] returns a pair [(Some t,resume')] where
      [t] is extracted from [resume], the latter being updated with
      possible alternatives met in building [t] to produce
      [resume']. It returns [(None,[])] if no tree can be
      extracted *)
  val resumption : 'a resumption ->  'a simple_tree option * ('a resumption)
    
  (** [is_empty resume] returns [true] if [resume] does not propose
      any other value on which to resume, [false] otherwise *)
  val is_empty : 'a resumption -> bool


end
