(** Time-stamp: <modified the 29/08/2019 (at 16:42) by Erwan Jahier> *)

(** topological sort of actions (that may optimize test openning) *)


let profile_info = Lv6Verbose.profile_info

(*********************************************************************************)
module TopoSortActions = 
  TopoSort.Make(
      struct 
        type elt = Action.t
        type store = ActionsDeps.t
        let find_dep   = ActionsDeps.find_deps
        let have_dep   = ActionsDeps.have_deps
        let remove_dep = ActionsDeps.remove_dep
      end
    )

let (topo_sort : Action.t list -> ActionsDeps.t -> Action.t list) =
  fun actions stbl -> 
  profile_info "topo_sort...\n";
  TopoSortActions.f stbl actions  


(*********************************************************************************)
(* From actions to gaos *)

let (gao_of_action: Action.t -> Soc.gao) = 
  fun (ck, il, ol, op, lxm) ->
    (* nb : the list (encoded in a tree) is in the reverse order  *)
    let rec unpack_clock acc = function
      | Lic.BaseLic -> acc
      | Lic.ClockVar _i -> acc (* should not occur? *)
      | Lic.On((value, cvar, _ctyp), outter_clock) ->
        let cc = Lv6Id.string_of_long false value in        
        let acc = Soc.Case (cvar, [cc, [acc]], lxm) in
        unpack_clock acc outter_clock
    in 
    unpack_clock (Soc.Call (ol, op, il, lxm)) ck

(*********************************************************************************)

(* In order to sort Soc.gao, I process in 3 stages ;
1. compute a total ordering of Actions.t according to the deps
2. transform Actions.t into Soc.gao (List.map)
3. factorize Soc.gao by looking at consecutive gao that have the same guard

*)

let (optimize_test_openning: Soc.gao list -> ActionsDeps.t -> Soc.gao list) =
  fun gaol _deps ->
    let rec aux acc gaol = match gaol with 
      | []  -> List.rev acc
      | [a] -> List.rev (a::acc)
      | Soc.Call(o,op,i,lxm)::tail -> aux (Soc.Call(o,op,i,lxm)::acc) tail 
      | a1::Soc.Call(o,op,i,lxm)::tail ->  aux (Soc.Call(o,op,i,lxm)::a1::acc) tail 
      | Soc.Case(v1,l1,lxm1)::Soc.Case(v2,l2,lxm2)::tail -> 
        if v1 <> v2 then aux (Soc.Case(v1,l1,lxm1)::acc) (Soc.Case(v2,l2,lxm2)::tail) else
          let l = merge_gaol l1 l2 [] in
          aux acc (Soc.Case(v1,l,lxm1)::tail) 
            
    and (merge_gaol : (string * Soc.gao list) list -> (string * Soc.gao list) list -> 
         (string * Soc.gao list) list -> (string * Soc.gao list) list) =
      fun l1 l2 acc -> 
        match l1 with
          | [] -> if l2 = [] then List.rev acc else List.rev_append acc l2
          | (x1,gaol1)::l1 -> 
            (match Lv6util.my_assoc x1 l2 with
              | None -> merge_gaol l1 l2 ((x1,gaol1)::acc)
              | Some(gaol2,l2) -> 
                let gaol = aux [] (gaol1@gaol2) in
                merge_gaol l1 l2 ((x1,gaol)::acc)
            )
    in
    aux [] gaol

(*********************************************************************************)
open Lv6MainArgs


let (f : Action.t list -> ActionsDeps.t -> Lxm.t -> Soc.gao list) = 
  fun actions deps lxm -> 
  (* =>  la liste d'actions en entrée contient des doublons !  *)
  try match global_opt.schedul_mode with
      | Simple -> (
        profile_info "SortActions.f: topo_sort...\n";
        let actions = topo_sort actions deps in
        profile_info "SortActions.f: gao_of_action...\n";
        let gaol = List.map gao_of_action actions in
        profile_info "SortActions.f: optimize_test_openning actions...\n";
        optimize_test_openning gaol deps 
      )
      | Sort -> ( (* experimental scheduling *)
        let actions = List.sort SortActionsExpe.compare_actions actions in
        let actions = topo_sort actions deps in
        let gaol = List.map gao_of_action actions in
        optimize_test_openning gaol deps 
      )
      | Reorder -> ( (* experimental scheduling *)
        let actions = topo_sort actions deps in
        let actions = SortActionsExpe.group actions deps in 
        let gaol = List.map (List.map gao_of_action) actions in 
        SortActionsExpe.optimize_test_openning gaol 
      )
  with TopoSortActions.DependencyCycle(_x,l) ->
    let name i = "a"^(string_of_int i) in
    let l = List.mapi (fun i x -> x, name i) l in
    let lstr = List.map (fun (a,n) -> n^": "^(Action.to_string a)) l in
    let legend = String.concat "\n\t" lstr in
    let _,names = List.split l in
    let dep = String.concat ">" names  in
    let msg = "A combinational cycle been detected "^
                (Lxm.details lxm)^": "^ dep ^
                  ">a0 where \n\t'>' means 'should be done after'\n\t" ^ legend ^
                    "\n\nHint: \n\t- try to use --expand-nodes or --expand-node-call; sometimes it works. \n\t- -knc migth ease to see where the cycle is.\n\t- -dbg deps will dump more (too much?) information\n"
                      (*                     ^ (ActionsDeps.to_string deps) ^ "\n" *)
    in
    raise (Lv6errors.Global_error msg)
