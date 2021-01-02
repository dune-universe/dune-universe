module Bimap_multi_pure=Bimap_multi_module.Bimap_multi_module
(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
(*March 2019 -- prior commits...not a true bimap.
  Use a functor so we have access to something satisfying Comparable.S 
  and force type of map to use 'a list and 'b list.*)
module Bimap_multi_class (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct
  module Bimap_multi_module = Bimap_multi_pure(ModuleA)(ModuleB)
  class ['a, 'b] bimap_multi_class (m1 : 'b list ModuleA.Map.t) (m2 : 'a list ModuleB.Map.t) = object(self)
    val mutable forward_map : ('b list ModuleA.Map.t ref) = ref (m1 : 'b list ModuleA.Map.t)
    val mutable reverse_map : ('a list ModuleB.Map.t ref) = ref (m2 : 'a list ModuleB.Map.t)

    method private empty_forward_map () =
      forward_map := ModuleA.Map.empty
    method private empty_reverse_map () =
      reverse_map := ModuleB.Map.empty
    method private set_mutable_maps new_t =
      let () = forward_map := Bimap_multi_module.get_fwd_map new_t in
      reverse_map := Bimap_multi_module.get_rev_map new_t

    method add_multi ~key ~data =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.add_multi t_ ~key ~data in
      self#set_mutable_maps new_t

    method add_multi_reverse ~key ~data =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.add_multi_reverse t_ ~key ~data in
      self#set_mutable_maps new_t

    method private remove_fwd_key_from_reverse_map ~fwd_values_list ~key =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.remove_fwd_key_from_reverse_map t_ ~fwd_values_list ~key in
      self#set_mutable_maps new_t

    method private remove_rev_key_from_forward_map ~rev_values_list ~key =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.remove_rev_key_from_forward_map t_ ~rev_values_list ~key in
      self#set_mutable_maps new_t

    method change ~key ~f =
      (* A -> 1,2,3 | B-> 4,5,6 | C -> 7,8,9 
         1->A|2->A|3->A|4->B|5->B|6->B ... etc
         but after some change could end up with
         A -> 2,4,6 | B-> 4,5,6 | C -> 7,8,9
         2->A|4->A,B|5->B|6->A,B| ... etc
       *)
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.change t_ ~key ~f in
      self#set_mutable_maps new_t

    method change_reverse ~key ~f =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.change_reverse t_ ~key ~f in
      self#set_mutable_maps new_t

    method private create_reverse_map_from_forward_map () =
      let () = self#empty_reverse_map () in 
      ModuleA.Map.iter_keys !forward_map
	~f:(fun k ->
          let values = ModuleA.Map.find_exn !forward_map k in
          Core.List.iter values ~f:(fun v -> reverse_map := ModuleB.Map.add_multi !reverse_map ~key:v ~data:k)
	)

    method private create_forward_map_from_reverse_map () =
      let () = self#empty_forward_map () in 
      ModuleB.Map.iter_keys !reverse_map
	~f:(fun k ->
	  let values = ModuleB.Map.find_exn !reverse_map k in
          Core.List.iter values ~f:(fun v -> forward_map := ModuleA.Map.add_multi !forward_map ~key:v ~data:k)
	)

    method count ~f =
      ModuleA.Map.count !forward_map ~f
    method count_reverse ~f =
      ModuleB.Map.count !reverse_map ~f
    method counti ~f =
      ModuleA.Map.counti !forward_map ~f
    method data =
      ModuleA.Map.data !forward_map
    method data_reverse =
      ModuleB.Map.data !reverse_map
    method empty () =
      let () = self#empty_forward_map () in
      self#empty_reverse_map ()
    method exists ~f =
      ModuleA.Map.exists !forward_map ~f
    method exists_reverse ~f =
      ModuleB.Map.exists !reverse_map ~f
    method existsi ~f =
      ModuleA.Map.existsi !forward_map ~f
    method existsi_reverse ~f =
      ModuleB.Map.existsi !reverse_map ~f
    method find ~key =
      ModuleA.Map.find !forward_map key
    method find_reverse ~key =
      ModuleB.Map.find !reverse_map key 
    method find_exn ~(key:ModuleA.Map.Key.t) =
      ModuleA.Map.find_exn !forward_map key
    method find_exn_reverse ~key =
      Core.Map.find_exn !reverse_map key
    method filter ~f =
      let () = forward_map := (ModuleA.Map.filter !forward_map ~f) in
      (*Since each map is a multi map of 'a list or 'b list, with 'b or 'a as keys, respectively, the ~f used to 
        filter keys in one map cannot be used to filter values in the other...at least not by the filter function*)
      self#create_reverse_map_from_forward_map
    method filter_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_keys ~f =
      let () = forward_map := (ModuleA.Map.filter_keys !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_keys_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filteri ~f =
      let () = forward_map := (ModuleA.Map.filteri !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filteri_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_map ~f =
      let () = forward_map := (ModuleA.Map.filter_map !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method fold : 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleA.Map.fold !forward_map ~init ~f)
    method fold_reverse : 'e. init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold !reverse_map ~init ~f)
    (*method fold_range_inclusive ~min ~max ~init ~f =*)
(*    method fold_range_inclusive : 'e. min:ModuleA.Map.Key.t -> max:ModuleA.Map.Key.t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e =
      ModuleA.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleA.Map.fold_right !forward_map ~init ~f)
    method fold_right_reverse : 'e. init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold_right !reverse_map ~init ~f)
    method for_all ~f =
      ModuleA.Map.for_all !forward_map ~f
    method for_all_reverse ~f =
      ModuleB.Map.for_all !reverse_map ~f
    method is_empty =
      ModuleA.Map.is_empty !forward_map
    method iter_keys ~f =
      ModuleA.Map.iter_keys !forward_map ~f
    method iter_keys_reverse ~f =
      ModuleB.Map.iter_keys !reverse_map ~f
    method iter ~f =
      ModuleA.Map.iter !forward_map ~f
    method iter_reverse ~f =
      ModuleB.Map.iter !reverse_map ~f
    method iteri ~f =
      ModuleA.Map.iteri !forward_map ~f
    method iteri_reverse ~f =
      ModuleB.Map.iteri !reverse_map ~f
    method keys =
      ModuleA.Map.keys !forward_map
    method keys_reverse =
      ModuleB.Map.keys !reverse_map
    method length =
      ModuleA.Map.length !forward_map
    method map ~f =
      let () = forward_map := (ModuleA.Map.map !forward_map ~f) in
      self#create_reverse_map_from_forward_map ()
    method map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map () 
    method mapi ~f =
      let () = forward_map := (Core.Map.mapi !forward_map ~f) in
      self#create_reverse_map_from_forward_map ()
    method mapi_reverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      self#create_forward_map_from_reverse_map ()     
    method mem key =
      Core.Map.mem !forward_map key
    method mem_reverse key =
      Core.Map.mem !reverse_map key
    method min_elt =
      Core.Map.min_elt !forward_map
    method min_elt_exn =
      Core.Map.min_elt_exn !forward_map
    method min_elt_reverse =
      Core.Map.min_elt !reverse_map
    method min_elt_exn_reverse =
      Core.Map.min_elt_exn !reverse_map
    method max_elt =
      Core.Map.max_elt !forward_map
    method max_elt_exn =
      Core.Map.max_elt_exn !forward_map
    method max_elt_reverse =
      Core.Map.max_elt !reverse_map
    method max_elt_exn_reverse =
      Core.Map.max_elt_exn !reverse_map
    method nth int =
      Core.Map.nth !forward_map int
    method nth_reverse int =
      Core.Map.nth !reverse_map int
    method remove ~key =
      let reverse_keys = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.remove !forward_map key) in
      self#remove_reverse_keys reverse_keys
    method private remove_reverse_keys klist = 
      let rec remove_keys k =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.Map.remove !reverse_map h) in
	   remove_keys t
	| [] -> () in
      remove_keys klist
    method remove_reverse ~key =
      let fwd_keys = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.remove !reverse_map key) in
      Core.List.iter fwd_keys
        ~f:(fun k ->
          let fwd_values = self#find_exn ~key:k in
          let new_fwd_values = (Core.List.filter fwd_values ~f:(fun x -> not (x = key))) in
          let () = forward_map := (Core.Map.remove !forward_map k) in
          forward_map := (Core.Map.set !forward_map  ~key:k ~data:new_fwd_values)
        )
    method remove_multi ~key =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.remove_multi t_ ~key in
      self#set_mutable_maps new_t
      
    method remove_reverse_multi ~key =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.remove_reverse_multi t_ ~key in
      self#set_mutable_maps new_t

    method to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> Core.Map.to_alist !forward_map
      | true -> Core.Map.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
    (*update and change are identical except that the function f must be of a different type; see Core.Map documentation.*)
    method update ~key ~f =
      let t_ = Bimap_multi_module.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let new_t = Bimap_multi_module.update t_ ~key ~f in
      self#set_mutable_maps new_t
  end
end
