(* doubly linked list *)

type 'a t
  (** The type of the dllist *)

type 'a node
  (** The type of the dllist node *)

val create : unit -> 'a t
  (** Create an empty dllist *)

val length : 'a t -> int
  (** O(1). The length of the dllist *)

val is_empty : 'a t -> bool
  
val list : 'a node -> 'a t option
  (** [list node] returns [Some t] if [node] is an element of [t].
      If [node] is removed, it returns [None]. *)

val is_removed : 'a node -> bool

val value : 'a node -> 'a
  (** Get the value from the node *)    

val add : 'a t -> 'a -> 'a node
  (** O(1). [add t v] adds [v] to dllist [t] and returns the newly created
      node for [v]. The node is used to remove the value from [t] in constant
      time.
  *)

val remove : 'a node -> (unit, [> `Already_removed ]) result
  (** O(1). [remove node] removes [node] from the dllist it belongs to.
      Successful removal returns [`Ok]. If the node is already removed,
      [remove node] returns [`Already_removed]. *)

val hd : 'a t -> 'a node option
  (** [hd t] returns the first node of the dllist [t]. *)

val tl : 'a t -> 'a node option option
  (** [tl t] returns the second node of the dllist [t]. 

      None : t is null
      Some None : t is a singleton
      Some (Some n) : n is the second
  *)

val hd_tl : 'a t -> ('a node * 'a node option) option

val iter : ('a node -> unit) -> 'a t -> unit
  (** Iteration over the nodes of a dllist from the top to the bottom *)

val fold_left : ('a -> 'b node -> 'a) -> 'a -> 'b t -> 'a
  (** Folding the nodes of a dllist from the top to the bottom *)

val fold_right : ('b node -> 'a -> 'a) -> 'b t -> 'a -> 'a
  (** Folding the nodes of a dllist from the bottom to top *)

val scan_left : ('a -> 'b node -> [< `Continue of 'a | `Stop of 'a ]) ->
  'a -> 'b t -> 'a
 (** [fold_left] with stop *)

val scan_left_nodes : ('a -> 'b node -> [< `Continue of 'a | `Stop of 'a ]) ->
  'a -> 'b node -> 'a
  (** [scan] but starts with a node *)

(** list <=> dllist conversion functions *)    
val to_nodes : 'a t -> 'a node list
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t

val invariant : 'a t -> unit
  (** Invariant checks *)
