module Bimap_multi_module :
functor (ModuleA : Core.Comparable.S) (ModuleB : Core.Comparable.S) ->
sig
  type t
  val empty : t
  val add_multi : t -> key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> t
  val add_multi_reverse : t -> key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> t
  val change : t -> key:ModuleA.Map.Key.t ->
               f:(ModuleB.Map.Key.t list option -> ModuleB.Map.Key.t list option) -> t
  val change_reverse : t -> key:ModuleB.Map.Key.t ->
                       f:(ModuleA.Map.Key.t list option -> ModuleA.Map.Key.t list option) -> t
  val count : t -> f:(ModuleB.Map.Key.t list -> bool) -> int
  val count_reverse : t -> f:(ModuleA.Map.Key.t list -> bool) -> int
  val counti : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> bool) -> int
(*        method private create_forward_map_from_reverse_map : unit -> unit
          method private create_reverse_map_from_forward_map : unit -> unit *)
  val create_t : fwdmap:ModuleB.Map.Key.t list ModuleA.Map.t -> revmap:ModuleA.Map.Key.t list ModuleB.Map.t -> t
  val data : t -> ModuleB.Map.Key.t list list
  val data_reverse : t-> ModuleA.Map.Key.t list list
            (*        method private empty_forward_map : unit -> unit
          method private empty_reverse_map : unit -> unit *)
  val exists : t -> f:(ModuleB.Map.Key.t list -> bool) -> bool
  val exists_reverse : t -> f:(ModuleA.Map.Key.t list -> bool) -> bool
  val existsi : t ->  f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> bool) -> bool
  val existsi_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> bool) -> bool
  val filter : t -> f:(ModuleB.Map.Key.t list -> bool) -> t 
  val filter_keys : t -> f:(ModuleA.Map.Key.t -> bool) -> t
  val filter_reverse : t -> f:(ModuleA.Map.Key.t list -> bool) -> t
  val filter_keys_reverse : t -> f:(ModuleB.Map.Key.t -> bool) -> t
  val filter_map : t -> f:(ModuleB.Map.Key.t Core.List.t -> ModuleB.Map.Key.t list option) -> t
  val filter_map_reverse : t -> f:(ModuleA.Map.Key.t list -> ModuleA.Map.Key.t list option) -> t
  val filteri : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> bool) -> t
  val filteri_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> bool) -> t
  val find : t -> key:ModuleA.Map.Key.t -> ModuleB.Map.Key.t list option
  val find_exn : t -> key:ModuleA.Map.Key.t -> ModuleB.Map.Key.t list
  val find_exn_reverse : t -> key:ModuleB.Map.Key.t -> ModuleA.Map.Key.t list
  val find_reverse : t -> key:ModuleB.Map.Key.t -> ModuleA.Map.Key.t list option
  val fold : t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e
  val fold_reverse : t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> 'e -> 'e) -> 'e
  val fold_right : t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e
  val fold_right_reverse : t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> 'e -> 'e) -> 'e
  val for_all : t -> f:(ModuleB.Map.Key.t Core.List.t -> bool) -> bool
  val for_all_reverse : t -> f:(ModuleA.Map.Key.t Core.List.t -> bool) -> bool
  val get_fwd_map : t -> ModuleB.Map.Key.t list ModuleA.Map.t
  val get_rev_map : t -> ModuleA.Map.Key.t list ModuleB.Map.t
  val is_empty : t -> bool
  val iter : t -> f:(ModuleB.Map.Key.t list -> unit) -> unit
  val iter_keys : t -> f:(ModuleA.Map.Key.t -> unit) -> unit
  val iter_keys_reverse : t -> f:(ModuleB.Map.Key.t -> unit) -> unit
  val iter_reverse : t -> f:(ModuleA.Map.Key.t list -> unit) -> unit
  val iteri : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> unit) -> unit
  val iteri_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> unit) -> unit
  val keys : t -> ModuleA.Map.Key.t list
  val keys_reverse : t-> ModuleB.Map.Key.t list
  val length : t -> int
  val map : t -> f:(ModuleB.Map.Key.t list -> ModuleB.Map.Key.t list) -> t
  val map_reverse : t -> f:(ModuleA.Map.Key.t list -> ModuleA.Map.Key.t list) -> t
  val mapi : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> ModuleB.Map.Key.t list) -> t
  val mapi_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> ModuleA.Map.Key.t list) -> t
  val max_elt : t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t list) option
  val max_elt_exn : t -> ModuleA.Map.Key.t * ModuleB.Map.Key.t list
  val max_elt_exn_reverse : t -> ModuleB.Map.Key.t * ModuleA.Map.Key.t list
  val max_elt_reverse : t -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t list) option
                                                                          
  val mem : t -> key:ModuleA.Map.Key.t -> bool
  val mem_reverse : t -> key:ModuleB.Map.Key.t -> bool

  val min_elt : t -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t list) option
  val min_elt_exn : t -> ModuleA.Map.Key.t * ModuleB.Map.Key.t list
  val min_elt_exn_reverse : t -> ModuleB.Map.Key.t * ModuleA.Map.Key.t list
  val min_elt_reverse : t -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t list) option
  val nth : t -> int -> (ModuleA.Map.Key.t * ModuleB.Map.Key.t list) option
  val nth_reverse : t -> int -> (ModuleB.Map.Key.t * ModuleA.Map.Key.t list) option
  val remove : t -> key:ModuleA.Map.Key.t -> t
  (*exposed for use in the class implementation*)
  val remove_fwd_key_from_reverse_map : t -> fwd_values_list:ModuleB.Map.Key.t list -> key:ModuleA.Map.Key.t -> t
  val remove_reverse : t -> key:ModuleB.Map.Key.t -> t
  (*exposed for use in the class implementation*)
  val remove_rev_key_from_forward_map : t -> rev_values_list:ModuleA.Map.Key.t list -> key:ModuleB.Map.Key.t -> t
  val remove_multi : t -> key:ModuleA.Map.Key.t-> t
  val remove_reverse_multi : t -> key:ModuleB.Map.Key.t -> t
  val to_alist : t -> ?key_order:[ `Decreasing | `Increasing ] -> unit ->
                 (ModuleA.Map.Key.t * ModuleB.Map.Key.t list) list
  val update : t -> key:ModuleA.Map.Key.t -> f:(ModuleB.Map.Key.t list option -> ModuleB.Map.Key.t list) -> t
(*  val remove_fwd_key_from_reverse_map : fwd_values_list:ModuleB.Map.Key.t list -> key:ModuleA.Map.Key.t -> t*)
(*        method private remove_rev_key_from_forward_map :
          rev_values_list:'a Core.List.t -> key:'b -> unit  *)
  val remove_forward_keys : ModuleA.t list -> 'b ModuleA.Map.t -> 'b ModuleA.Map.t
  val remove_reverse_keys : ModuleB.t list -> 'a ModuleB.Map.t -> 'a ModuleB.Map.t
end
