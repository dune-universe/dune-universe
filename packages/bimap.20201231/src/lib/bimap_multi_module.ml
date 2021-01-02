module Bimap_multi_module (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct

  type t = {
      fwdmap : ModuleB.Map.Key.t list ModuleA.Map.t;
      revmap : ModuleA.Map.Key.t list ModuleB.Map.t
    }
             
  let empty =
      {fwdmap=ModuleA.Map.empty; revmap=ModuleB.Map.empty}

  let create_t ~fwdmap ~revmap = { fwdmap=fwdmap;  revmap=revmap }
  let rec add_multi t ~key ~data =
    let update_maps t ~key ~data =
      let new_forward_map =
        (ModuleA.Map.add_multi t.fwdmap ~key ~data) in
      let newt = { fwdmap=new_forward_map; revmap=t.revmap } in 
      add_multi_reverse newt ~key:data ~data:key in
    if ModuleA.Map.mem t.fwdmap key then
      if Core.List.mem
           (ModuleA.Map.find_exn t.fwdmap key) data
           ~equal:(ModuleB.equal) then t
      else 
        update_maps t ~key ~data
    else
      update_maps t ~key ~data
  and add_multi_reverse t ~key ~data =
    let update_maps t ~key ~data =
      let new_reverse_map = (ModuleB.Map.add_multi t.revmap ~key ~data) in
      let newt = { fwdmap=t.fwdmap; revmap=new_reverse_map } in 
      add_multi newt ~key:data ~data:key in
      if ModuleB.Map.mem t.revmap key then
        if Core.List.mem
             (ModuleB.Map.find_exn t.revmap key) data
             ~equal:(ModuleA.equal) then t
        else 
          update_maps t ~key ~data
      else
        update_maps t ~key ~data

    let rec remove_fwd_key_from_reverse_map t ~fwd_values_list ~key =
      match fwd_values_list with
      | [] -> t
      | h :: tl -> 
         let old_reverse_bindings = ModuleB.Map.find_exn t.revmap h in
         if (Core.List.count old_reverse_bindings ~f:(fun _v -> true)) > 1 then
           let new_reverse_bindings = Core.List.filter old_reverse_bindings ~f:(fun v -> not (v = key)) in
           let new_reverse_map = ModuleB.Map.remove t.revmap h in
           let new_reverse_map = ModuleB.Map.add_exn new_reverse_map ~key:h ~data:new_reverse_bindings in
           let newt = { fwdmap=t.fwdmap; revmap=new_reverse_map} in 
           remove_fwd_key_from_reverse_map newt ~fwd_values_list:tl ~key
         else
           let newrevmap = ModuleB.Map.remove t.revmap h in
           let newt = { fwdmap=t.fwdmap; revmap=newrevmap} in
           remove_fwd_key_from_reverse_map newt ~fwd_values_list:tl ~key

    let rec remove_rev_key_from_forward_map t ~rev_values_list ~key =
      match rev_values_list with
      | [] -> t
      | h :: tl -> 
         let old_fwd_bindings = ModuleA.Map.find_exn t.fwdmap h in
         if (Core.List.count old_fwd_bindings ~f:(fun _v -> true)) > 1 then
           let new_fwd_bindings = Core.List.filter old_fwd_bindings ~f:(fun v -> not (v = key)) in
           let new_forward_map = ModuleA.Map.remove t.fwdmap h in
           let newfwdmap = ModuleA.Map.add_exn new_forward_map ~key:h ~data:new_fwd_bindings in
           let newt = { fwdmap=newfwdmap; revmap=t.revmap} in 
           remove_rev_key_from_forward_map newt ~rev_values_list:tl ~key
         else
           let newfwdmap = ModuleA.Map.remove t.fwdmap h in
           let newt = { fwdmap=newfwdmap; revmap=t.revmap} in
           remove_rev_key_from_forward_map newt ~rev_values_list:tl ~key

    let change t ~key ~f =
      (* A -> 1,2,3 | B-> 4,5,6 | C -> 7,8,9 
         1->A|2->A|3->A|4->B|5->B|6->B ... etc
         but after some change could end up with
         A -> 2,4,6 | B-> 4,5,6 | C -> 7,8,9
         2->A|4->A,B|5->B|6->A,B| ... etc   *)
      let old_value_list = ModuleA.Map.find_exn t.fwdmap key in 
      let new_forward_map = ModuleA.Map.change t.fwdmap key ~f in
      let new_values = ModuleA.Map.find_exn new_forward_map key in
    (*--- now only need to fixup the reverse mapping ---
       For each old value (key in reverse mapping):
         a) if the only binding is to ~key provided to this function then remove the old value (key in reverse mapping)
         b) else filter the list of values to remove the ~key provided to this function
       And then for each new value, add them to reverse mapping  *)
      let tempt = { fwdmap=new_forward_map; revmap=t.revmap } in 
      let newt = remove_fwd_key_from_reverse_map tempt ~fwd_values_list:old_value_list ~key in
      let rec add_new_rev_mappings t2 ~new_values =
        match new_values with
        | [] -> t2
        | h :: tl ->
           let new_reverse_map = ModuleB.Map.add_multi t2.revmap ~key:h ~data:key in
           let tempt = { fwdmap = t2.fwdmap; revmap = new_reverse_map } in
           add_new_rev_mappings tempt ~new_values:tl
      in
      add_new_rev_mappings newt ~new_values

    let change_reverse t ~key ~f =
      let old_value_list = ModuleB.Map.find_exn t.revmap key in 
      let new_reverse_map = ModuleB.Map.change t.revmap key ~f in
      let new_values = ModuleB.Map.find_exn new_reverse_map key in
      let tempt = { fwdmap=t.fwdmap; revmap=new_reverse_map } in 
      let newt = remove_rev_key_from_forward_map tempt ~rev_values_list:old_value_list ~key in
      let rec add_new_fwd_mappings t2 ~new_values =
        match new_values with
        | [] -> t2
        | h :: tl ->
           let new_forward_map = ModuleA.Map.add_multi t2.fwdmap ~key:h ~data:key in
           let tempt = { fwdmap = new_forward_map; revmap = t2.revmap } in
           add_new_fwd_mappings tempt ~new_values:tl
      in
      add_new_fwd_mappings newt ~new_values

    let create_reverse_map_from_forward_map ~forward_map =
      let newrevmap = ref ModuleB.Map.empty in 
      let () = ModuleA.Map.iter_keys forward_map
	         ~f:(fun k ->
                   let values = ModuleA.Map.find_exn forward_map k in
                   Core.List.iter values ~f:(fun v -> newrevmap := ModuleB.Map.add_multi !newrevmap ~key:v ~data:k)
	         ) in
      !newrevmap

    let create_forward_map_from_reverse_map ~reverse_map =
      let newfwdmap = ref ModuleA.Map.empty in 
      let () = ModuleB.Map.iter_keys reverse_map
	         ~f:(fun k ->
	           let values = ModuleB.Map.find_exn reverse_map k in
                   Core.List.iter values ~f:(fun v -> newfwdmap := ModuleA.Map.add_multi !newfwdmap ~key:v ~data:k)
	         ) in
      !newfwdmap

    let count t ~f =
      ModuleA.Map.count t.fwdmap ~f
    let count_reverse t ~f  =
      ModuleB.Map.count t.revmap ~f
    let counti t ~f =
      ModuleA.Map.counti t.fwdmap ~f
    let data t =
      ModuleA.Map.data t.fwdmap
    let data_reverse t =
      ModuleB.Map.data t.revmap
    let exists t ~f =
      ModuleA.Map.exists t.fwdmap ~f
    let exists_reverse t ~f =
      ModuleB.Map.exists t.revmap ~f
    let existsi t ~f =
      ModuleA.Map.existsi t.fwdmap ~f
    let existsi_reverse t ~f =
      ModuleB.Map.existsi t.revmap ~f
    let filter t ~f =
      let new_forward_map = ModuleA.Map.filter t.fwdmap ~f in
      (*Since each map is a multi map of 'a list or 'b list, with 'b or 'a as keys, respectively, the ~f used to 
        filter keys in one map cannot be used to filter values in the other.*)
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filter_keys t ~f =
      let new_forward_map = ModuleA.Map.filter_keys t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filter_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filter t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let filter_keys_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filter_keys t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let filter_map t ~f =
      let new_forward_map = ModuleA.Map.filter_map t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filter_map_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filter_map t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let filteri t ~f =
      let new_forward_map = ModuleA.Map.filteri t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filteri_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filteri t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let find t ~key =
      ModuleA.Map.find t.fwdmap key
    let find_reverse t ~key =
      ModuleB.Map.find t.revmap key 
    let find_exn t ~key =
      ModuleA.Map.find_exn t.fwdmap key
    let find_exn_reverse t ~key =
      ModuleB.Map.find_exn t.revmap key
    let fold : 'e. t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e = 
      (fun t ~init ~f -> ModuleA.Map.fold t.fwdmap ~init ~f)
    let fold_reverse : 'e. t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> 'e -> 'e) -> 'e =
      (fun t ~init ~f -> ModuleB.Map.fold t.revmap ~init ~f)
(*  method fold_range_inclusive ~min ~max ~init ~f =
      Core.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    let fold_right : 'e. t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e =
      (fun t ~init ~f -> ModuleA.Map.fold_right t.fwdmap ~init ~f)
    let fold_right_reverse : 'e. t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> 'e -> 'e) -> 'e =
      (fun t ~init ~f -> ModuleB.Map.fold_right t.revmap ~init ~f)
    let for_all t ~f = ModuleA.Map.for_all t.fwdmap ~f
    let for_all_reverse t ~f = ModuleB.Map.for_all t.revmap ~f
    let get_fwd_map t = t.fwdmap
    let get_rev_map t = t.revmap
    let is_empty t = ModuleA.Map.is_empty t.fwdmap
    let iter t ~f = ModuleA.Map.iter t.fwdmap ~f
    let iter_keys t ~f = ModuleA.Map.iter_keys t.fwdmap ~f
    let iter_keys_reverse t ~f = ModuleB.Map.iter_keys t.revmap ~f
    let iter_reverse t ~f = ModuleB.Map.iter t.revmap ~f
    let iteri t ~f = ModuleA.Map.iteri t.fwdmap ~f
    let iteri_reverse t ~f = ModuleB.Map.iteri t.revmap ~f
    let keys t = ModuleA.Map.keys t.fwdmap
    let keys_reverse t = ModuleB.Map.keys t.revmap
    let length t = ModuleA.Map.length t.fwdmap
    let map t ~f =
      let newfwdmap = ModuleA.Map.map t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:newfwdmap in
      { fwdmap = newfwdmap; revmap = newrevmap }
    let map_reverse t ~f =
      let newreversemap = ModuleB.Map.map t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:newreversemap in
      { fwdmap = newfwdmap ; revmap = newreversemap }
    let mapi t ~f =
      let newforwardmap = Core.Map.mapi t.fwdmap ~f in
      let newreversemap = create_reverse_map_from_forward_map ~forward_map:newforwardmap in
      { fwdmap = newforwardmap ; revmap = newreversemap }
    let mapi_reverse t ~f =
      let newreverse_map = Core.Map.mapi t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:newreverse_map in
      { fwdmap = newfwdmap; revmap=newreverse_map }
    let max_elt t = ModuleA.Map.max_elt t.fwdmap
    let max_elt_exn t = ModuleA.Map.max_elt_exn t.fwdmap
    let max_elt_exn_reverse t = ModuleB.Map.max_elt_exn t.revmap
    let max_elt_reverse t = ModuleB.Map.max_elt t.revmap

    let mem t ~key = ModuleA.Map.mem t.fwdmap key
    let mem_reverse t ~key = ModuleB.Map.mem t.revmap key
                                             
    let min_elt t = ModuleA.Map.min_elt t.fwdmap
    let min_elt_exn t = ModuleA.Map.min_elt_exn t.fwdmap
    let min_elt_exn_reverse t = ModuleB.Map.min_elt_exn t.revmap
    let min_elt_reverse t = ModuleB.Map.min_elt t.revmap
    let nth t n = ModuleA.Map.nth t.fwdmap n
    let nth_reverse t n = Core.Map.nth t.revmap n
    let rec remove_reverse_keys klist revmap = 
      match klist with
      | [] -> revmap
      | h :: tl ->
	 let new_reverse_map = ModuleB.Map.remove revmap h in
         remove_reverse_keys tl new_reverse_map;;
    let rec remove_forward_keys klist fwdmap = 
      match klist with
      | [] -> fwdmap
      | h :: tl ->
	 let new_fwd_map = ModuleA.Map.remove fwdmap h in
         remove_forward_keys tl new_fwd_map;;
    let remove t ~key =
      let reverse_keys = ModuleA.Map.find_exn t.fwdmap key in 
      let new_forward_map = (ModuleA.Map.remove t.fwdmap key) in
      let rec helper revmap ks = 
        match ks with
        | [] -> revmap
        | h :: t -> 
           let rev_values = ModuleB.Map.find_exn revmap h in
           (*if there is only one value we don't want the key to remain mapping to an empty list*)
           if Core.List.length rev_values = 1 then
             let new_revmap = ModuleB.Map.remove revmap h in
             helper new_revmap t
           else 
             let new_rev_values = (Core.List.filter rev_values ~f:(fun x -> not (x = key))) in
             let new_revmap = (ModuleB.Map.remove revmap h) in
             let newrevmap = ModuleB.Map.set new_revmap ~key:h ~data:new_rev_values in
             helper newrevmap t in
      let newrevmap = helper t.revmap reverse_keys in
      { fwdmap=new_forward_map; revmap=newrevmap }
    let remove_reverse t ~key =
      let fwd_keys = ModuleB.Map.find_exn t.revmap key in 
      let new_reverse_map = ModuleB.Map.remove t.revmap key in
      let rec helper fwdmap ks = 
        match ks with
        | [] -> fwdmap
        | h :: tl -> 
           let fwd_values = ModuleA.Map.find_exn fwdmap h in
           let new_fwd_values = (Core.List.filter fwd_values ~f:(fun x -> not (x = key))) in
           let new_forward_map = ModuleA.Map.remove fwdmap h in
           let newfwdmap = ModuleA.Map.set new_forward_map ~key:h ~data:new_fwd_values in
           helper newfwdmap tl in
      let newfwdmap = helper t.fwdmap fwd_keys in
      { fwdmap=newfwdmap; revmap=new_reverse_map }
    let remove_multi t ~key =
      try
	let values = ModuleA.Map.find_exn t.fwdmap key in
	let head_element = Core.List.hd_exn values in 
	let new_forward_map = ModuleA.Map.remove_multi t.fwdmap key in
	(*using head_element: if reverse_map binds head_element only to key then remove it else filter out key*)
        let tempt = { fwdmap=new_forward_map ; revmap=t.revmap } in 
        remove_fwd_key_from_reverse_map tempt ~fwd_values_list:[head_element] ~key
                                        (*--TODO--improve exception handling*)
      with _e -> raise (Failure "bimap_multi::remove_multi() failed")
    let remove_reverse_multi t ~key =
      try
	let values = ModuleB.Map.find_exn t.revmap key in
	let head_element = Core.List.hd_exn values in 
	let new_reverse_map = ModuleB.Map.remove_multi t.revmap key in
        let tempt = { fwdmap=t.fwdmap; revmap=new_reverse_map } in 
	(*using head_element: if reverse_map binds head_element only to key then remove it else filter out key*)
        remove_rev_key_from_forward_map tempt ~rev_values_list:[head_element] ~key
                                        (*--TODO--improve exception handling*)
      with _e -> raise (Failure "bimap_multi::remove_reverse_multi() failed")
    let to_alist t ?key_order () =
      match key_order with
      | None -> Core.Map.to_alist t.fwdmap
      | Some order -> ModuleA.Map.to_alist ~key_order:order t.fwdmap
    (*update and change are identical except that the function f must be of a different type; see Core.Map documentation.*)
    let update t ~key ~f =
      let oldvalues = ModuleA.Map.find_exn t.fwdmap key in
      let new_forward_map = ModuleA.Map.update t.fwdmap key ~f in
      let newvalues = ModuleA.Map.find_exn new_forward_map key in
      let tempt = { fwdmap=new_forward_map; revmap=t.revmap } in 
      (*remove or filter mappings in reverse map for oldvalues and then add_multi newvalues to reverse map*)
      let newt = remove_fwd_key_from_reverse_map tempt ~fwd_values_list:oldvalues ~key in
      let rec helper t l =
        match l with
        | [] -> t
        | h :: tl ->
           let newrevmap = ModuleB.Map.add_multi t.revmap ~key:h ~data:key in
           let newt = { fwdmap=t.fwdmap; revmap=newrevmap } in
           helper newt tl in 
      helper newt newvalues
end
