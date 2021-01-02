(*This functor generates a module, not a class, and has no mutable member, more closely 
  resembling the Map interface*)
module Bimap_single (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct
  (*better than passing tuples around and possibly getting order 
   backwards somewhere, after which all bets are off; so we saddle 
   client code writers with this type*)
  type t = {
      fwdmap : ModuleB.Map.Key.t ModuleA.Map.t;
      revmap : ModuleA.Map.Key.t ModuleB.Map.t
    }
  let make_t ~fwd_map ~rev_map = { fwdmap=fwd_map; revmap=rev_map }
  (*    let empty_forward_map () = ModuleA.Map.empty
    let empty_reverse_map () = ModuleB.Map.empty*)
  let get_rev_map t = t.revmap
  let get_fwd_map t = t.fwdmap
  let set t ~key ~data  =
    if ModuleA.Map.mem t.fwdmap key then
      let value = ModuleA.Map.find_exn t.fwdmap key in 
      let newfmap = ModuleA.Map.set t.fwdmap ~key ~data in
      let newrmap = ModuleB.Map.remove t.revmap value in
      let newrmap = ModuleB.Map.set newrmap ~key:data ~data:key in
      {fwdmap=newfmap; revmap=newrmap}
    else
      let newfmap = ModuleA.Map.set t.fwdmap ~key ~data in
      let newrmap = ModuleB.Map.set t.revmap ~key:data ~data:key in
      {fwdmap=newfmap; revmap=newrmap}
  let set_reverse t ~key ~data =
    if ModuleB.Map.mem t.revmap key then
      let value = ModuleB.Map.find_exn t.revmap key in
      let newrmap = ModuleB.Map.set t.revmap ~key ~data in
      let newfmap = ModuleA.Map.remove t.fwdmap value in
      let newfmap = ModuleA.Map.set newfmap ~key:data ~data:key in
      {fwdmap=newfmap; revmap=newrmap}
    else
      let newrmap = ModuleB.Map.set t.revmap ~key ~data in
      let newfmap = ModuleA.Map.set t.fwdmap ~key:data ~data:key in
      {fwdmap=newfmap; revmap=newrmap}
  let change t ~key ~f =
    let old_value = ModuleA.Map.find_exn t.fwdmap key in 
    let newfmap = ModuleA.Map.change t.fwdmap key ~f in
    let new_value = ModuleA.Map.find_exn newfmap key in
    let newrmap = ModuleB.Map.remove t.revmap old_value in
    let newrmap = ModuleB.Map.set newrmap ~key:new_value ~data:key in
    {fwdmap=newfmap; revmap=newrmap}
  let change_reverse t ~key ~f =
    let old_key = ModuleB.Map.find_exn t.revmap key in 
    let newrmap = ModuleB.Map.change t.revmap key ~f in
    let new_value = ModuleB.Map.find_exn newrmap key in
    let newfmap = ModuleA.Map.remove t.fwdmap old_key in
    let newfmap = ModuleA.Map.set newfmap ~key:new_value ~data:key in
    {fwdmap=newfmap; revmap=newrmap}
  let create_reverse_map_from_forward_map ~forward_map =
    let newrmap = ModuleB.Map.empty in
    let newrmapref = ref newrmap in 
    (*iteri fits the bill better than iter keys*)
    let () = ModuleA.Map.iteri
	       ~f:(fun ~key ~data ->
	         newrmapref :=
                   (ModuleB.Map.set !newrmapref ~key:data ~data:key))
               forward_map in
    !newrmapref
  let create_forward_map_from_reverse_map ~reverse_map =
    let newfmap = ModuleA.Map.empty in
    let newfmapref = ref newfmap in 
    let () = ModuleB.Map.iteri 
	       ~f:(fun ~key ~data ->
		 newfmapref :=
                   ModuleA.Map.set !newfmapref ~key:data ~data:key)
               reverse_map in
    !newfmapref

  let count t ~f =
    ModuleA.Map.count t.fwdmap ~f
  let count_reverse t ~f =
    ModuleB.Map.count t.revmap ~f
  let counti t ~f =
    ModuleA.Map.counti t.fwdmap ~f
  let data t =
    ModuleA.Map.data t.fwdmap
  let data_reverse t =
    ModuleB.Map.data t.revmap
  let empty =
    { fwdmap=ModuleA.Map.empty; revmap=ModuleB.Map.empty }
  let exists t ~f =
    ModuleA.Map.exists t.fwdmap ~f
  let exists_reverse t ~f =
    ModuleB.Map.exists t.revmap ~f
  let existsi t ~f =
    ModuleA.Map.existsi t.fwdmap ~f
  let existsi_reverse t ~f =
    ModuleB.Map.existsi t.revmap ~f
  let find t ~key =
    ModuleA.Map.find t.fwdmap key
  let find_reverse t ~key =
    ModuleB.Map.find t.revmap key
  let find_exn t ~key =
    ModuleA.Map.find_exn t.fwdmap key
  let find_exn_reverse t ~key =
    ModuleB.Map.find_exn t.revmap key
  let filter t ~f =
    let new_forward_map = (ModuleA.Map.filter t.fwdmap ~f) in
    let new_reverse_map = (ModuleB.Map.filter_keys t.revmap ~f) in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filter_reverse t ~f =
    let new_reverse_map = (ModuleB.Map.filter t.revmap ~f) in
    let new_forward_map = (ModuleA.Map.filter_keys t.fwdmap ~f) in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filter_keys t ~f =
    let new_forward_map = (ModuleA.Map.filter_keys t.fwdmap ~f) in
    let new_reverse_map = (ModuleB.Map.filter t.revmap ~f) in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filter_keys_reverse t ~f =
    let new_reverse_map = (ModuleB.Map.filter_keys t.revmap ~f) in
    let new_forward_map = (ModuleA.Map.filter t.fwdmap ~f) in 
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filteri t ~f =
    let new_forward_map = (ModuleA.Map.filteri t.fwdmap ~f) in
    let new_reverse_map = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filteri_reverse t ~f =
    let new_reverse_map = (ModuleB.Map.filteri t.revmap ~f) in
    let new_forward_map = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filter_map t ~f =
    let new_forward_map = (ModuleA.Map.filter_map t.fwdmap ~f) in
    let new_reverse_map = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let filter_map_reverse t ~f =
    let new_reverse_map = (ModuleB.Map.filter_map t.revmap ~f) in
    let new_forward_map = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in
    {fwdmap=new_forward_map; revmap=new_reverse_map}
  let fold t ~init ~f = 
    ModuleA.Map.fold t.fwdmap ~init ~f
  let fold_reverse t ~init ~f =
    ModuleB.Map.fold t.revmap ~init ~f
  let fold_range_inclusive t ~min ~max ~init ~f =
    ModuleA.Map.fold_range_inclusive t.fwdmap ~min ~max ~init ~f
  let fold_right t ~init ~f =
    ModuleA.Map.fold_right t.fwdmap ~init ~f
  let fold_right_reverse t ~init ~f =
    ModuleB.Map.fold_right t.revmap ~init ~f
  let for_all t ~f =
    ModuleA.Map.for_all t.fwdmap ~f
  let for_all_reverse t ~f =
    ModuleB.Map.for_all t.revmap ~f
  let is_empty t =
    ModuleA.Map.is_empty t.fwdmap
  let iter_keys t ~f =
    ModuleA.Map.iter_keys t.fwdmap ~f
  let iter_keys_reverse t ~f =
    ModuleB.Map.iter_keys t.revmap ~f
  let iter t ~f =
    ModuleA.Map.iter t.fwdmap ~f
  let iter_reverse t ~f =
    ModuleB.Map.iter t.revmap ~f
  let iteri t ~f =
    ModuleA.Map.iteri t.fwdmap ~f
  let iteri_reverse t ~f =
    ModuleB.Map.iteri t.revmap ~f
  let keys t =
    ModuleA.Map.keys t.fwdmap
  let keys_reverse t =
    ModuleB.Map.keys t.revmap
  let length t =
    ModuleA.Map.length t.fwdmap
  let map t ~f =
    let new_forward_map = (ModuleA.Map.map t.fwdmap ~f) in
    let new_rev_map = ref ModuleB.Map.empty in 
    let () =
      ModuleA.Map.iter_keys
	new_forward_map
	~f:(fun k -> new_rev_map :=
		       ModuleB.Map.set !new_rev_map
			 ~key:(ModuleA.Map.find_exn new_forward_map k) ~data:k) in
    { fwdmap=new_forward_map; revmap = !new_rev_map }
  let map_reverse t ~f =
    let new_reverse_map = (ModuleB.Map.map t.revmap ~f) in
    let new_fwd_map = ref ModuleA.Map.empty in 
    let () = ModuleB.Map.iter_keys
	       new_reverse_map
	       ~f:(fun k -> new_fwd_map :=
		              ModuleA.Map.set !new_fwd_map
                                ~key:(ModuleB.Map.find_exn new_reverse_map k) ~data:k) in
    { fwdmap = !new_fwd_map; revmap = new_reverse_map }
  let mapi t ~f =
    let new_forward_map = (ModuleA.Map.mapi t.fwdmap ~f) in
    let new_rev_map = ref ModuleB.Map.empty in 
    let () = ModuleA.Map.iter_keys
	       new_forward_map
	       ~f:(fun k -> new_rev_map :=
		              ModuleB.Map.set !new_rev_map
				~key:(ModuleA.Map.find_exn new_forward_map k) ~data:k) in
    { fwdmap = new_forward_map; revmap = !new_rev_map }
  let mapi_reverse t ~f =
    let new_reverse_map = (ModuleB.Map.mapi t.revmap ~f) in
    let new_fwd_map = ref ModuleA.Map.empty in 
    let () = ModuleB.Map.iter_keys
	       new_reverse_map
	       ~f:(fun k -> new_fwd_map :=
		              ModuleA.Map.set !new_fwd_map
				~key:(ModuleB.Map.find_exn new_reverse_map k) ~data:k) in
    { fwdmap = !new_fwd_map; revmap = new_reverse_map }
  let mem t ~key =
    ModuleA.Map.mem t.fwdmap key
  let mem_reverse t ~key =
    ModuleB.Map.mem t.revmap key
  let min_elt t =
    ModuleA.Map.min_elt t.fwdmap
  let min_elt_exn t =
    ModuleA.Map.min_elt_exn t.fwdmap
  let min_elt_reverse t =
    ModuleB.Map.min_elt t.revmap
  let min_elt_exn_reverse t =
    ModuleB.Map.min_elt_exn t.revmap
  let max_elt t =
    ModuleA.Map.max_elt t.fwdmap
  let max_elt_exn t =
    ModuleA.Map.max_elt_exn t.fwdmap
  let max_elt_reverse t =
    ModuleB.Map.max_elt t.revmap
  let max_elt_exn_reverse t =
    ModuleB.Map.max_elt_exn t.revmap
  let nth t ~int =
    ModuleA.Map.nth t.fwdmap int
  let nth_reverse t ~int =
    ModuleB.Map.nth t.revmap int
  let remove t ~key =
    let reverse_key = ModuleA.Map.find_exn t.fwdmap key in 
    let new_forward_map = ModuleA.Map.remove t.fwdmap key in
    let new_reverse_map = ModuleB.Map.remove t.revmap reverse_key in
    { fwdmap = new_forward_map; revmap = new_reverse_map }
  let remove_reverse t ~key =
    let fwd_key = ModuleB.Map.find_exn t.revmap key in 
    let new_reverse_map = ModuleB.Map.remove t.revmap key in
    let new_forward_map = ModuleA.Map.remove t.fwdmap fwd_key in
    { fwdmap = new_forward_map; revmap = new_reverse_map }
  (*    let reverse_map = !reverse_map*)
  let to_alist ?key_order t =
    match Core.Option.is_some key_order with
    | false -> ModuleA.Map.to_alist t.fwdmap
    | true -> ModuleA.Map.to_alist ~key_order:(Core.Option.value_exn key_order) t.fwdmap
  let update t ~key ~f =
    let oldvalue = ModuleA.Map.find_exn t.fwdmap key in
    let new_forward_map = ModuleA.Map.update t.fwdmap key ~f in
    let newvalue = ModuleA.Map.find_exn new_forward_map key in
    let new_reverse_map = ModuleB.Map.remove t.revmap oldvalue in 
    let new_reverse_map = ModuleB.Map.set new_reverse_map ~key:newvalue ~data:key in
    { fwdmap = new_forward_map; revmap = new_reverse_map }
end
