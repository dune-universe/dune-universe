module Bimap_single :
functor (ModuleA : Core.Comparable.S) (ModuleB : Core.Comparable.S) ->
sig
  type t
  val make_t : fwd_map:ModuleB.Map.Key.t ModuleA.Map.t -> rev_map:ModuleA.Map.Key.t ModuleB.Map.t -> t
  val get_fwd_map : t -> ModuleB.Map.Key.t ModuleA.Map.t
  val get_rev_map : t -> ModuleA.Map.Key.t ModuleB.Map.t
  val empty :t
  val set : t -> key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> t
                                                                      
  val set_reverse : t -> key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> t

  val change : t -> key:ModuleA.Map.Key.t ->
	       f:(ModuleB.Map.Key.t option -> ModuleB.Map.Key.t option) -> t

  val change_reverse :
		     t -> key:ModuleB.Map.Key.t ->
		     f:(ModuleA.Map.Key.t option -> ModuleA.Map.Key.t option) -> t

  (*CANNOT WRITE THESE RIGHT NOW -- 
        val comparator : unit -> ('a, 'c) Core_kernel__.Comparator.t
        val comparator_reverse :
		 unit -> ('b, 'd) Core_kernel__.Comparator.t*)
  val count : t -> f:(ModuleB.Map.Key.t -> bool) -> int
  val count_reverse : t -> f:(ModuleA.Map.Key.t -> bool) -> int
  val counti : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> bool) -> int
  val data : t -> ModuleB.Map.Key.t list
  val data_reverse : t -> ModuleA.Map.Key.t list
  val exists : t -> f:(ModuleB.Map.Key.t -> bool) -> bool
  val exists_reverse : t -> f:(ModuleA.Map.Key.t -> bool) -> bool
  val existsi : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> bool) -> bool
  val existsi_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> bool) -> bool
  val find : t -> key:ModuleA.Map.Key.t -> ModuleB.Map.Key.t option
  val find_reverse : t -> key:ModuleB.Map.Key.t -> ModuleA.Map.Key.t option
  val find_exn : t -> key:ModuleA.Map.Key.t -> ModuleB.Map.Key.t
  val find_exn_reverse : t -> key:ModuleB.Map.Key.t -> ModuleA.Map.Key.t
                                     
  val filter : t -> f:(ModuleB.Map.Key.t -> bool) -> t
  val filter_reverse : t -> f:(ModuleA.Map.Key.t -> bool) -> t
  val filteri : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> bool) -> t
  val filteri_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> bool) -> t
  val filter_keys : t -> f:(ModuleA.Map.Key.t -> bool) -> t
  val filter_keys_reverse : t -> f:(ModuleB.Map.Key.t -> bool) -> t
  val filter_map : t -> f:(ModuleB.Map.Key.t -> ModuleB.Map.Key.t option) -> t
  val filter_map_reverse : t -> f:(ModuleA.Map.Key.t -> ModuleA.Map.Key.t option) -> t
  (*	val forward_map : ('a,'b,_) Core.Map.t*)
  val fold : t -> init:'a -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> 'a -> 'a) -> 'a
  val fold_reverse : t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> 'e -> 'e) -> 'e
  val fold_range_inclusive : t -> min:ModuleA.Map.Key.t -> max:ModuleA.Map.Key.t -> init:'a -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> 'a -> 'a) -> 'a
  val fold_right : t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> 'e -> 'e) -> 'e
  val fold_right_reverse : t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> 'e -> 'e) -> 'e
  val for_all : t -> f:(ModuleB.Map.Key.t -> bool) -> bool
  val for_all_reverse : t -> f:(ModuleA.Map.Key.t -> bool) -> bool
  val is_empty : t -> bool
  val iter_keys : t -> f:(ModuleA.Map.Key.t -> unit) -> unit
  val iter : t -> f:(ModuleB.Map.Key.t -> unit) -> unit
  val iter_reverse : t -> f:(ModuleA.Map.Key.t -> unit) -> unit
  val iter_keys_reverse :  t -> f:(ModuleB.Map.Key.t -> unit) -> unit
  val iteri : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> unit) -> unit
  val iteri_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> unit) -> unit
  val keys : t -> ModuleA.Map.Key.t list
  val keys_reverse : t -> ModuleB.Map.Key.t list
  val length : t -> int
  val map : t -> f:(ModuleB.Map.Key.t -> ModuleB.Map.Key.t) -> t
  val map_reverse : t -> f:(ModuleA.Map.Key.t -> ModuleA.Map.Key.t) -> t
  val mapi : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> ModuleB.Map.Key.t) -> t
  val mapi_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> ModuleA.Map.Key.t) -> t
  val mem : t -> key:ModuleA.Map.Key.t -> bool
  val mem_reverse : t -> key:ModuleB.Map.Key.t -> bool
  val min_elt : t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t) option
  val min_elt_exn : t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t)
  val min_elt_reverse : t -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t) option
  val min_elt_exn_reverse : t -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t)
  val max_elt : t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t) option
  val max_elt_exn : t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t)
  val max_elt_reverse : t -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t) option
  val max_elt_exn_reverse : t -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t)
  val nth : t -> int:int -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t) option
  val nth_reverse : t -> int:int -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t) option
  val remove : t -> key:ModuleA.Map.Key.t -> t
  val remove_reverse : t -> key:ModuleB.Map.Key.t -> t
	    (*	val reverse_map : ('b, 'a, _) Core.Map.t*)
  val to_alist : ?key_order:[`Increasing | `Decreasing] -> t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t) list 
  val update : t -> key:ModuleA.Map.Key.t -> f:(ModuleB.Map.Key.t option -> ModuleB.Map.Key.t) -> t
end
