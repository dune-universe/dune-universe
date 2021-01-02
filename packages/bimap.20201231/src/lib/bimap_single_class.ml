module Bimap_single_pure=Bimap_single_module.Bimap_single
(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
module Bimap_single_class (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct
  module Bimap_single_module = Bimap_single_pure(ModuleA)(ModuleB)
  class ['a, 'b] bimap_single_class (m1 : 'b ModuleA.Map.t) (m2 : 'a ModuleB.Map.t) = object(self)
    val mutable forward_map : ('b ModuleA.Map.t ref) = ref (m1 : 'b ModuleA.Map.t)
    val mutable reverse_map : ('a ModuleB.Map.t ref) = ref (m2 : 'a ModuleB.Map.t)
                                                           
    method private empty_forward_map () =
      forward_map := ModuleA.Map.empty
    method private empty_reverse_map () =
      reverse_map := ModuleB.Map.empty
    method set ~key ~data =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.set t ~key ~data in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2 
    (*hardly worth using the Bimap_single_module for set and set_reverse*)
    method set_reverse ~key ~data =
      let () = reverse_map := (ModuleB.Map.set !reverse_map ~key ~data) in
      forward_map := ModuleA.Map.set !forward_map ~key:data ~data:key
    method change ~key ~f =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.change t ~key ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2 
    method change_reverse ~key ~f =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.change_reverse t ~key ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2 

    method private create_reverse_map_from_forward_map =
      let () = self#empty_reverse_map () in
      self#iter_keys
	     ~f:(fun k ->
		 reverse_map :=
		   ModuleB.Map.set !reverse_map ~key:(self#find_exn ~key:k) ~data:k)
    method private create_forward_map_from_reverse_map =
      let () = self#empty_forward_map () in 
      self#iter_keys_reverse
	     ~f:(fun k ->
		 forward_map :=
		   ModuleA.Map.set !forward_map
		     ~key:(self#find_exn_reverse ~key:k) ~data:k)

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
(*    method equal f ~other_bimap =
      Core.Map.equal f !forward_map !other_fwd_map*)
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
    method find_exn ~key =
      ModuleA.Map.find_exn !forward_map key
    method find_exn_reverse ~key =
      ModuleB.Map.find_exn !reverse_map key
    method filter ~f =
      let () = forward_map := (ModuleA.Map.filter !forward_map ~f) in
      reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f)
    method filter_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter !reverse_map ~f) in
      forward_map := (ModuleA.Map.filter_keys !forward_map ~f)
    method filter_keys ~f =
      let () = forward_map := (ModuleA.Map.filter_keys !forward_map ~f) in
      reverse_map := (ModuleB.Map.filter !reverse_map ~f)
    method filter_keys_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f) in
      forward_map := (ModuleA.Map.filter !forward_map ~f)
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
    (*    method forward_map = !forward_map*)
    method fold : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleA.Map.fold !forward_map ~init ~f)
    method fold_reverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold !reverse_map ~init ~f)
    (*~min ~max ~(init:'e) ~f:(key:'a -> data:'a -> 'e -> 'e) -> 'e =*)
(*    method fold_range_inclusive : 'e. init:'e -> min:'a -> max:'a -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e =
ModuleA.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleA.Map.fold_right !forward_map ~init ~f)
    method fold_right_reverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
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
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.map t ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2
    method map_reverse ~f =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.map_reverse t ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2
    method mapi ~f =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.mapi t ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2
    method mapi_reverse ~f =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.mapi_reverse t ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2
    method mem key =
      ModuleA.Map.mem !forward_map key
    method mem_reverse key =
      ModuleB.Map.mem !reverse_map key
    method min_elt =
      ModuleA.Map.min_elt !forward_map
    method min_elt_exn =
      ModuleA.Map.min_elt_exn !forward_map
    method min_elt_reverse =
      ModuleB.Map.min_elt !reverse_map
    method min_elt_exn_reverse =
      ModuleB.Map.min_elt_exn !reverse_map
    method max_elt =
      ModuleA.Map.max_elt !forward_map
    method max_elt_exn =
      ModuleA.Map.max_elt_exn !forward_map
    method max_elt_reverse =
      ModuleB.Map.max_elt !reverse_map
    method max_elt_exn_reverse =
      ModuleB.Map.max_elt_exn !reverse_map
    method nth int =
      ModuleA.Map.nth !forward_map int
    method nth_reverse int =
      ModuleB.Map.nth !reverse_map int
    method remove ~key =
      let reverse_key = ModuleA.Map.find_exn !forward_map key in 
      let () = forward_map := (ModuleA.Map.remove !forward_map key) in
      reverse_map := (ModuleB.Map.remove !reverse_map reverse_key)
    method remove_reverse ~key =
      let fwd_key = ModuleB.Map.find_exn !reverse_map key in 
      let () = reverse_map := (ModuleB.Map.remove !reverse_map key) in
      forward_map := (ModuleA.Map.remove !forward_map fwd_key)
(*    method reverse_map = !reverse_map*)
    method to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> ModuleA.Map.to_alist !forward_map
      | true -> ModuleA.Map.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
    method update ~key ~f =
      let t = Bimap_single_module.make_t ~fwd_map:!forward_map ~rev_map:!reverse_map in
      let t2 = Bimap_single_module.update t ~key ~f in
      let () = forward_map := Bimap_single_module.get_fwd_map t2 in 
      reverse_map := Bimap_single_module.get_rev_map t2
  end
end
