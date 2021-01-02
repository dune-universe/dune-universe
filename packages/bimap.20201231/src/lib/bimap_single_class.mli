module Bimap_single_class :
functor (ModuleA : Core.Comparable.S) (ModuleB : Core.Comparable.S) ->
sig
  class ['a, 'b] bimap_single_class :
	  'b ModuleA.Map.t ->
	  'a ModuleB.Map.t ->
	  object
            constraint 'a = ModuleA.Map.Key.t
            constraint 'b = ModuleB.Map.Key.t
            val mutable forward_map : 'b ModuleA.Map.t ref
            val mutable reverse_map : 'a ModuleB.Map.t ref
            method set : key:'a -> data:'b -> unit
            method set_reverse : key:'b -> data:'a -> unit
            method change :
		     key:'a ->
		     f:('b option -> 'b option) ->
		     unit
            method change_reverse :
		     key:'b ->
		     f:('a option -> 'a option) ->
		     unit
	    (*CANNOT WRITE THESE RIGHT NOW -- 
        method comparator : unit -> ('a, 'c) Core_kernel__.Comparator.t
        method comparator_reverse :
		 unit -> ('b, 'd) Core_kernel__.Comparator.t*)
	    method count : f:('b -> bool) -> int
	    method count_reverse : f:('a -> bool) -> int
	    method counti : f:(key:'a -> data:'b -> bool) -> int
            method data : 'b list
            method data_reverse : 'a list
	    method empty : unit -> unit
	    method exists : f:('b -> bool) -> bool
	    method exists_reverse : f:('a -> bool) -> bool
	    method existsi : f:(key:'a -> data:'b -> bool) -> bool
	    method existsi_reverse : f:(key:'b -> data:'a -> bool) -> bool
            method find : key:'a -> 'b option
            method find_exn : key:'a -> 'b
            method find_exn_reverse : key:'b -> 'a
            method find_reverse : key:'b -> 'a option
	    method filter : f:('b -> bool) -> unit
	    method filter_reverse : f:('a -> bool) -> unit
	    method filteri : f:(key:'a -> data:'b -> bool) -> unit
	    method filteri_reverse : f:(key:'b -> data:'a -> bool) -> unit
	    method filter_keys : f:('a -> bool) -> unit
	    method filter_keys_reverse : f:('b -> bool) -> unit
	    method filter_map : f:('b -> 'b option) -> unit
	    method filter_map_reverse : f:('a -> 'a option) -> unit
	    (*	method forward_map : ('a,'b,_) Core.Map.t*)
	    (*method fold : init:'a -> f:(key:'a -> data:'b -> 'a -> 'a) -> 'a*)
            method fold : init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e
	    method fold_reverse : init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e
            (*	    method fold_range_inclusive : init:'e -> min:'a -> max:'a -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e*)
	    method fold_right : init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e
	    method fold_right_reverse : init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e
	    method for_all : f:('b -> bool) -> bool
	    method for_all_reverse : f:('a -> bool) -> bool
	    method is_empty : bool
            method iter :
		     f:('b -> unit) -> unit
            method iter_reverse :
		     f:('a -> unit) -> unit  
            method iter_keys :
		     f:(ModuleA.Map.Key.t -> unit) -> unit
            method iter_keys_reverse :
		     f:(ModuleB.Map.Key.t -> unit) -> unit
            method iteri :
		     f:(key:'a -> data:'b -> unit) -> unit
            method iteri_reverse :
		     f:(key:'b -> data:'a -> unit) -> unit
            method keys : 'a list
            method keys_reverse : 'b list
            method length : int
            method map : f:('b -> 'b) -> unit
            method map_reverse : f:('a -> 'a) -> unit
            method mapi :
		     f:(key:'a -> data:'b -> 'b) -> unit
            method mapi_reverse :
		     f:(key:'b -> data:'a -> 'a) -> unit
            method mem : 'a -> bool
            method mem_reverse : 'b -> bool
	    method min_elt : ('a * 'b) option
	    method min_elt_exn : ('a * 'b)
	    method min_elt_reverse : ('b * 'a) option
	    method min_elt_exn_reverse : ('b * 'a)
	    method max_elt : ('a * 'b) option
	    method max_elt_exn : ('a * 'b)
	    method max_elt_reverse : ('b * 'a) option
	    method max_elt_exn_reverse : ('b * 'a)
	    method nth : int -> ('a * 'b) option
	    method nth_reverse : int -> ('b * 'a) option
            method remove : key:'a -> unit
            method remove_reverse : key:'b -> unit
	    (*	method reverse_map : ('b, 'a, _) Core.Map.t*)
            method to_alist : ?key_order:[`Increasing | `Decreasing] -> unit -> ('a * 'b) list 
            method update : key:'a -> f:('b option -> 'b) -> unit
	  end
(*  type ('a, 'b) t = ('a, 'b) bimap_class
  val make : ('a, 'b, 'c) Core.Map.t -> ('b, 'a, 'd) Core.Map.t -> ('a, 'b) t*)
end
