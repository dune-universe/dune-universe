(*
   POMAP - Library for manipulating partially ordered maps

   Copyright (C) 2001-2002  Markus Mottl  (OEFAI)
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Pomap_intf

module Make (PO : PARTIAL_ORDER) = struct
  module Store = Store_impl.IntStore
  module Ix = Store.Ix

  type key = PO.el

  type 'a node =
    {
      key : PO.el;
      el : 'a;
      sucs : Ix.Set.t;
      prds : Ix.Set.t;
    }

  type 'a pomap =
    {
      nodes : 'a node Store.t;
      top : Ix.Set.t;
      bot : Ix.Set.t;
    }

  type 'a add_find_result =
    | Found of Ix.t * 'a node
    | Added of Ix.t * 'a node * 'a pomap

  let empty = { nodes = Store.empty; top = Ix.Set.empty; bot = Ix.Set.empty }

  let is_empty pm = Store.is_empty pm.nodes
  let cardinal pm = Store.cardinal pm.nodes

  let create_node key el sucs prds =
    { key = key; el = el; sucs = sucs; prds = prds }

  let singleton key el =
    let node = create_node key el Ix.Set.empty Ix.Set.empty in
    let ix, nodes = Store.singleton node in
    let ix_set = Ix.Set.singleton ix in
    {
      nodes = nodes;
      top = ix_set;
      bot = ix_set;
    }

  let get_key node = node.key
  let get_el node = node.el
  let get_sucs node = node.sucs
  let get_prds node = node.prds

  let set_key node key = { node with key = key }
  let set_el node el = { node with el = el }
  let set_sucs node sucs = { node with sucs = sucs }
  let set_prds node prds = { node with prds = prds }

  let get_nodes pm = pm.nodes
  let get_top pm = pm.top
  let get_bot pm = pm.bot

  let coll_suc_set sucs (ix, _) = Ix.Set.add ix sucs

  let add_find_ix next_ix key el { nodes = nodes; top = top; bot = bot } =
    let ix_node_ref = ref None in
    let coll_nbrs ix node (suc_lst, prd_lst as nbrs) =
      match PO.compare node.key key with
      | PO.Lower ->
          let rec coll acc = function
            | [] -> suc_lst, (ix, node) :: acc
            | (_, old_node) as h :: t ->
                match PO.compare old_node.key node.key with
                | PO.Unknown -> coll (h :: acc) t
                | PO.Lower -> coll acc t
                | _ -> nbrs in
          coll [] prd_lst
      | PO.Greater ->
          let rec coll acc = function
            | [] -> (ix, node) :: acc, prd_lst
            | (_, old_node) as h :: t ->
                match PO.compare old_node.key node.key with
                | PO.Unknown -> coll (h :: acc) t
                | PO.Greater -> coll acc t
                | _ -> nbrs in
          coll [] suc_lst
      | PO.Unknown -> nbrs
      | PO.Equal -> ix_node_ref := Some (ix, node); raise Exit
    and graph = Store.get_ix_map nodes in
    try
      let suc_lst, prd_lst = Store.Ix.Map.fold coll_nbrs graph ([], []) in
      let sucs = List.fold_left coll_suc_set Ix.Set.empty suc_lst in
      let new_nodes, prds, top =
        let coll (new_nodes, prds, top) (ix, ({ sucs = node_sucs } as node)) =
          let new_sucs, new_top =
            if Ix.Set.is_empty node_sucs then node_sucs, Ix.Set.remove ix top
            else Ix.Set.diff node_sucs sucs, top in
          let new_node = { node with sucs = Ix.Set.add next_ix new_sucs } in
          Store.update ix new_node new_nodes, Ix.Set.add ix prds, new_top in
        List.fold_left coll (nodes, Ix.Set.empty, top) prd_lst in
      let new_nodes, bot =
        let coll (new_nodes, bot) (ix, ({ prds = node_prds } as node)) =
          let new_prds, new_bot =
            if Ix.Set.is_empty node_prds then node_prds, Ix.Set.remove ix bot
            else Ix.Set.diff node_prds prds, bot in
          let new_node = { node with prds = Ix.Set.add next_ix new_prds } in
          Store.update ix new_node new_nodes, new_bot in
        List.fold_left coll (new_nodes, bot) suc_lst
      and next_node = create_node key el sucs prds in
      let new_nodes = Store.update next_ix next_node new_nodes in
      let new_top =
        if Ix.Set.is_empty sucs then Ix.Set.add next_ix top else top in
      let new_bot =
        if Ix.Set.is_empty prds then Ix.Set.add next_ix bot else bot in
      let new_pm = { nodes = new_nodes; top = new_top; bot = new_bot } in
      Added (next_ix, next_node, new_pm)
    with Exit ->
      match !ix_node_ref with
      | Some (ix, node) -> Found (ix, node)
      | _ -> assert false

  let add_find key el pm = add_find_ix (Store.next_ix pm.nodes) key el pm

  let add_fun key el f pm =
    match add_find key el pm with
    | Found (ix, node) ->
        let new_node = { node with el = f node.el } in
        { pm with nodes = Store.update ix new_node pm.nodes }
    | Added (_, _, pm) -> pm

  let add key el pm =
    match add_find key el pm with
    | Found (ix, node) ->
        let new_node = { node with el = el } in
        { pm with nodes = Store.update ix new_node pm.nodes }
    | Added (_, _, pm) -> pm

  let add_ix next_ix key el pm =
    match add_find_ix next_ix key el pm with
    | Found (ix, node) ->
        let new_node = { node with el = el } in
        { pm with nodes = Store.update ix new_node pm.nodes }
    | Added (_, _, pm) -> pm

  let add_node node pm = add node.key node.el pm

  let remove_ix ix ({ nodes = nodes } as pm) =
    let { prds = prds; sucs = sucs } = Store.find ix nodes in
    let new_nodes = Store.remove ix nodes in
    if Ix.Set.is_empty prds then
      if Ix.Set.is_empty sucs then
        {
          nodes = new_nodes;
          top = Ix.Set.remove ix pm.top;
          bot = Ix.Set.remove ix pm.bot;
        }
      else
        let coll suc_ix (new_nodes, bot) =
          let suc = Store.find suc_ix new_nodes in
          let suc_prds = Ix.Set.remove ix suc.prds in
          let new_bot =
            if Ix.Set.is_empty suc_prds then Ix.Set.add suc_ix bot else bot in
          Store.update suc_ix { suc with prds = suc_prds } new_nodes, new_bot in
        let new_nodes, new_bot =
          Ix.Set.fold coll sucs (new_nodes, Ix.Set.remove ix pm.bot) in
        { nodes = new_nodes; top = pm.top; bot = new_bot }
    else
      if Ix.Set.is_empty sucs then
        let coll prd_ix (new_nodes, top) =
          let prd = Store.find prd_ix new_nodes in
          let prd_sucs = Ix.Set.remove ix prd.sucs in
          let new_top =
            if Ix.Set.is_empty prd_sucs then Ix.Set.add prd_ix top else top in
          Store.update prd_ix { prd with sucs = prd_sucs } new_nodes, new_top in
        let new_nodes, new_top =
          Ix.Set.fold coll prds (new_nodes, Ix.Set.remove ix pm.top) in
        { nodes = new_nodes; top = new_top; bot = pm.bot }
      else
        let coll_suc_lst suc_ix l =
          let suc = Store.find suc_ix new_nodes in
          (suc_ix, suc, ref (Ix.Set.remove ix suc.prds)) :: l in
        let suc_lst = Ix.Set.fold coll_suc_lst sucs [] in
        let nodes_ref = ref new_nodes
        and top_ref = ref pm.top
        and bot_ref = ref pm.bot
        and none_lower prd_sucs key =
          let some_lower prd_suc_ix =
            let prd_suc = Store.find prd_suc_ix new_nodes in
            PO.compare prd_suc.key key = PO.Lower in
          not (Ix.Set.exists some_lower prd_sucs) in
        let do_prds prd_ix =
          let prd = Store.find prd_ix new_nodes in
          let prd_sucs = Ix.Set.remove ix prd.sucs in
          let new_prd_sucs_ref = ref prd_sucs in
          let act (suc_ix, suc, suc_prds_ref) =
            if none_lower prd_sucs suc.key then begin
              suc_prds_ref := Ix.Set.add prd_ix !suc_prds_ref;
              new_prd_sucs_ref := Ix.Set.add suc_ix !new_prd_sucs_ref
            end in
          List.iter act suc_lst;
          let new_prd_sucs = !new_prd_sucs_ref in
          let new_prd = { prd with sucs = new_prd_sucs } in
          nodes_ref := Store.update prd_ix new_prd !nodes_ref;
          if Ix.Set.is_empty new_prd_sucs then
            top_ref := Ix.Set.add prd_ix !top_ref in
        Ix.Set.iter do_prds prds;
        let act (suc_ix, suc, suc_prds_ref) =
          let suc_prds = !suc_prds_ref in
          if Ix.Set.is_empty suc_prds then
            bot_ref := Ix.Set.add suc_ix !bot_ref;
          let new_suc = { suc with prds = suc_prds } in
          nodes_ref := Store.update suc_ix new_suc !nodes_ref in
        List.iter act suc_lst;
        { nodes = !nodes_ref; top = !top_ref; bot = !bot_ref }

  let rec find_ixset key nodes ixset =
    let found_ref = ref false and ix_node_ref = ref None in
    let iter_ixset ix =
      let node = Store.find ix nodes in
      match PO.compare node.key key with
      | PO.Greater -> raise Not_found
      | PO.Equal ->
          found_ref := true; ix_node_ref := Some (ix, node); raise Exit
      | PO.Lower -> ix_node_ref := Some (ix, node); raise Exit
      | PO.Unknown -> () in
    try Ix.Set.iter iter_ixset ixset; raise Not_found
    with Exit ->
      match !found_ref, !ix_node_ref with
      | true, Some (ix, node) -> ix, node
      | false, Some (_, node) -> find_ixset key nodes node.sucs
      | _ -> assert false

  let find key pm = find_ixset key pm.nodes pm.bot
  let find_ix ix pm = Store.find ix pm.nodes

  let remove key pm = try remove_ix (fst (find key pm)) pm with Not_found -> pm
  let remove_node node pm = remove node.key pm

  let take key pm =
    let ix, node = find key pm in
    ix, node, remove_ix ix pm

  let take_ix ix pm =
    let node = Store.find ix pm.nodes in
    node, remove_ix ix pm

  let mem key pm = try ignore (find key pm); true with Not_found -> false
  let mem_ix ix pm = Ix.Map.mem ix (Store.get_ix_map pm.nodes)

  let choose_bot pm =
    let ix = Ix.Set.choose pm.bot in
    ix, Store.find ix pm.nodes

  let choose = choose_bot

  let iter f pm = Store.iter f pm.nodes
  let iteri f pm = Store.iteri f pm.nodes

  let map f pm =
    let map_fun node = { node with el = f node } in
    { pm with nodes = Store.map map_fun pm.nodes }

  let mapi f pm =
    let mapi_fun ix node = { node with el = f ix node } in
    { pm with nodes = Store.mapi mapi_fun pm.nodes }

  let fold f pm acc = Store.fold f pm.nodes acc
  let foldi f pm acc = Store.foldi f pm.nodes acc

  let topo_fold f pm acc =
    if Ix.Set.is_empty pm.bot then acc
    else
      let coll ix (pm, acc) = remove_ix ix pm, f (Store.find ix pm.nodes) acc in
      let rec loop pm acc =
        let new_pm, new_acc = Ix.Set.fold coll pm.bot (pm, acc) in
        if Ix.Set.is_empty new_pm.bot then new_acc else loop new_pm new_acc in
      loop pm acc

  let topo_foldi f pm acc =
    if Ix.Set.is_empty pm.bot then acc
    else
      let coll ix (pm, acc) =
        remove_ix ix pm, f ix (Store.find ix pm.nodes) acc in
      let rec loop pm acc =
        let new_pm, new_acc = Ix.Set.fold coll pm.bot (pm, acc) in
        if Ix.Set.is_empty new_pm.bot then new_acc else loop new_pm new_acc in
      loop pm acc

  let topo_fold_ix f pm acc =
    if Ix.Set.is_empty pm.bot then acc
    else
      let coll ix (pm, acc) = remove_ix ix pm, f ix acc in
      let rec loop pm acc =
        let new_pm, new_acc = Ix.Set.fold coll pm.bot (pm, acc) in
        if Ix.Set.is_empty new_pm.bot then new_acc else loop new_pm new_acc in
      loop pm acc

  let rev_topo_fold f pm acc =
    if Ix.Set.is_empty pm.top then acc
    else
      let coll ix (pm, acc) = remove_ix ix pm, f (Store.find ix pm.nodes) acc in
      let rec loop pm acc =
        let new_pm, new_acc = Ix.Set.fold coll pm.top (pm, acc) in
        if Ix.Set.is_empty new_pm.top then new_acc else loop new_pm new_acc in
      loop pm acc

  let rev_topo_foldi f pm acc =
    if Ix.Set.is_empty pm.top then acc
    else
      let coll ix (pm, acc) =
        remove_ix ix pm, f ix (Store.find ix pm.nodes) acc in
      let rec loop pm acc =
        let new_pm, new_acc = Ix.Set.fold coll pm.top (pm, acc) in
        if Ix.Set.is_empty new_pm.top then new_acc else loop new_pm new_acc in
      loop pm acc

  let rev_topo_fold_ix f pm acc =
    if Ix.Set.is_empty pm.top then acc
    else
      let coll ix (pm, acc) = remove_ix ix pm, f ix acc in
      let rec loop pm acc =
        let new_pm, new_acc = Ix.Set.fold coll pm.top (pm, acc) in
        if Ix.Set.is_empty new_pm.top then new_acc else loop new_pm new_acc in
      loop pm acc

  let chain_fold f ({ nodes = nodes } as pm) acc =
    let rec coll chain ix acc =
      let { prds = prds } as node = Store.find ix nodes in
      let new_chain = node :: chain in
      if Ix.Set.is_empty prds then f new_chain acc
      else Ix.Set.fold (coll new_chain) prds acc in
    Ix.Set.fold (coll []) pm.top acc

  let chain_foldi f ({ nodes = nodes } as pm) acc =
    let rec coll chain ix acc =
      let { prds = prds } as node = Store.find ix nodes in
      let new_chain = (ix, node) :: chain in
      if Ix.Set.is_empty prds then f new_chain acc
      else Ix.Set.fold (coll new_chain) prds acc in
    Ix.Set.fold (coll []) pm.top acc

  let rev_chain_fold f ({ nodes = nodes } as pm) acc =
    let rec coll chain ix acc =
      let { sucs = sucs } as node = Store.find ix nodes in
      let new_chain = node :: chain in
      if Ix.Set.is_empty sucs then f new_chain acc
      else Ix.Set.fold (coll new_chain) sucs acc in
    Ix.Set.fold (coll []) pm.bot acc

  let rev_chain_foldi f ({ nodes = nodes } as pm) acc =
    let rec coll chain ix acc =
      let { sucs = sucs } as node = Store.find ix nodes in
      let new_chain = (ix, node) :: chain in
      if Ix.Set.is_empty sucs then f new_chain acc
      else Ix.Set.fold (coll new_chain) sucs acc in
    Ix.Set.fold (coll []) pm.bot acc

  let union pm1 pm2 = fold add_node pm1 pm2

  let inter pm1 pm2 =
    let inter_node ix node acc =
      if mem node.key pm2 then acc else remove_ix ix acc in
    foldi inter_node pm1 pm1

  let diff pm1 pm2 = fold remove_node pm2 pm1

  let filter p pm =
    let colli ix node acc = if p ix node then acc else remove_ix ix acc in
    Store.foldi colli pm.nodes pm

  let partition p pm =
    let colli ix node (yes, no) =
      if p ix node then yes, remove_ix ix no else remove_ix ix yes, no in
    Store.foldi colli pm.nodes (pm, pm)

  let remove_eq_prds eq pm =
    let colli ix _ ({ nodes = nodes } as pm) =
      let { el = el; prds = prds } = Store.find ix nodes in
      let has_same_el prd_ix = eq el (Store.find prd_ix nodes).el in
      if Ix.Set.for_all has_same_el prds then remove_ix ix pm else pm in
    topo_foldi colli (Ix.Set.fold remove_ix pm.bot pm) pm

  let fold_eq_classes eq f { nodes = nodes } glb_acc =
    let colli ix ({ el = ec_el } as node) (glb_acc, vis as acc) =
      if Ix.Set.mem ix vis then acc
      else
        let rec coll ix (ec, vis as acc) =
          if Ix.Set.mem ix vis then acc
          else
            let { el = el } as node = Store.find ix nodes in
            if eq ec_el el then
              let init = add_ix ix node.key el ec, Ix.Set.add ix vis in
              Ix.Set.fold coll node.prds (Ix.Set.fold coll node.sucs init)
            else acc in
      let ec, new_vis =
        let init = add_ix ix node.key ec_el empty, Ix.Set.add ix vis in
        Ix.Set.fold coll node.sucs (Ix.Set.fold coll node.prds init) in
      f ec_el ec glb_acc, new_vis in
    fst (Store.foldi colli nodes (glb_acc, Ix.Set.empty))

  let rec split_eq_class (ec, n as ec_info) nodes acc =
    if n = 0 then acc
    else if n = 1 then ec_info :: acc
    else
      match try Some (Store.choose nodes) with Not_found -> None with
      | Some (ix, node) ->
          let colli ec_ix ec_node (low, low_n, up, up_n) =
            match PO.compare node.key ec_node.key with
            | PO.Unknown ->
                let low_ec_node = Store.find ec_ix low.nodes in
                let up_ec_node = Store.find ec_ix up.nodes in
                let n_low_prds = Ix.Set.cardinal low_ec_node.prds in
                let n_up_prds = Ix.Set.cardinal up_ec_node.prds in
                if n_low_prds < n_up_prds then
                  low, low_n, remove_ix ec_ix up, up_n - 1
                else if n_low_prds > n_up_prds then
                  if n_up_prds = 0 then
                    low, low_n, remove_ix ec_ix up, up_n - 1
                  else remove_ix ec_ix low, low_n - 1, up, up_n
                else
                  let n_low_sucs = Ix.Set.cardinal low_ec_node.sucs in
                  let n_up_sucs = Ix.Set.cardinal up_ec_node.sucs in
                  if n_low_sucs > n_up_sucs then
                    low, low_n, remove_ix ec_ix up, up_n - 1
                  else remove_ix ec_ix low, low_n - 1, up, up_n
            | PO.Lower -> remove_ix ec_ix low, low_n - 1, up, up_n
            | PO.Greater -> low, low_n, remove_ix ec_ix up, up_n - 1
            | PO.Equal -> assert false (* impossible *) in
          let low, low_n, up, up_n = rev_topo_foldi colli ec (ec, n, ec, n) in
          let new_nodes = Store.remove ix nodes in
          let acc = split_eq_class (low, low_n) new_nodes acc in
          split_eq_class (up, up_n) new_nodes acc
      | None -> ec_info :: acc

  let fold_split_eq_classes eq f pm glb_acc =
    let node_eq n1 n2 = eq n1.el n2.el in
    let all_ec_infos = Store.eq_classes node_eq pm.nodes in
    let coll_ecs ec_el ec glb_acc =
      let coll_all ec_infos (all_ec_node, all_nodes) =
        if eq all_ec_node.el ec_el then ec_infos
        else
          let coll_ec acc ec_info = split_eq_class ec_info all_nodes acc in
          List.fold_left coll_ec [] ec_infos in
      let init = [(ec, Store.cardinal ec.nodes)] in
      let split_ecs = List.fold_left coll_all init all_ec_infos in
      List.fold_left (fun acc (ec, _) -> f ec_el ec acc) glb_acc split_ecs in
    fold_eq_classes eq coll_ecs pm glb_acc

  let cons_pm _ pm acc = pm :: acc

  let pm_cmp pm1 pm2 =
    let { nodes = nodes1 } = pm1 in
    let { nodes = nodes2 } = pm2 in
    let res = ref 0 in
    let cmp_key key1 ix2 =
      match PO.compare key1 (Store.find ix2 nodes2).key with
      | PO.Unknown -> ()
      | PO.Greater -> res := 1; raise Exit
      | PO.Lower -> res := -1; raise Exit
      | PO.Equal -> assert false (* impossible *) in
    begin try
      let act_bot ix =
        Ix.Set.iter (cmp_key (Store.find ix nodes1).key) pm2.bot in
      Ix.Set.iter act_bot pm1.bot;
      let act_top ix =
        Ix.Set.iter (cmp_key (Store.find ix nodes1).key) pm2.top in
      Ix.Set.iter act_top pm1.top
    with Exit -> () end;
    !res

  let preorder_eq_classes eq pm =
    List.fast_sort pm_cmp (fold_split_eq_classes eq cons_pm pm [])

  let topo_fold_reduced eq f ({ nodes = nodes } as pm) acc =
    let sub_coll ix acc = f (Store.find ix nodes) acc in
    let coll acc sub_pm = Ix.Set.fold sub_coll sub_pm.bot acc in
    List.fold_left coll acc (preorder_eq_classes eq pm)

  let unsafe_update pm ix node =
    { pm with nodes = Store.update ix node pm.nodes }

  let unsafe_set_nodes pm new_nodes = { pm with nodes = new_nodes }
  let unsafe_set_top pm new_top = { pm with top = new_top }
  let unsafe_set_bot pm new_bot = { pm with bot = new_bot }
end
