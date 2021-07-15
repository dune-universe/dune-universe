(* Time-stamp: <modified the 29/08/2019 (at 15:45) by Erwan Jahier> *)

open Lxm
open Lic

module IdMap = Map.Make(struct type t = Lv6Id.t let compare = compare end)
module IdSet = Set.Make(struct type t = Lv6Id.t let compare = compare end)

(* Associate to an ident the set of idents it depends on *)
type dependencies = (Lxm.t * IdSet.t) IdMap.t

(*********************************************************************************)
(* Compute the set of vars appearing in an expression *)
let (vars_of_exp : Lic.val_exp -> IdSet.t) =
  fun ve ->
  let rec aux s ve =
    vars_of_val_exp_core s ve.ve_core
    and
      vars_of_val_exp_core s = function
      | CallByPosLic ({ it=FBY ;_}, _) 
      | CallByPosLic ({ it=PRE ;_}, _) 
        -> s (* pre is not a dependance! *)
      | CallByPosLic (by_pos_op, vel) -> 
         let s = vars_of_by_pos_op s by_pos_op.it in
         List.fold_left aux s vel
      | Merge(ce, l) -> 
         let s = aux s ce in
         List.fold_left (fun s (_,ve) -> aux s ve) s l
      | CallByNameLic(_, _) -> s
    and 
      vars_of_by_pos_op s = function
      | VAR_REF id -> IdSet.add id s
      | PREDEF_CALL(_)
      | ARRAY_SLICE _ | ARRAY_ACCES _  | ARROW | FBY | CURRENT _ | WHEN _ 
      | ARRAY | HAT(_) | STRUCT_ACCESS _
      | TUPLE | CONCAT | CONST_REF _ | CALL _ | CONST _ -> s
      | PRE -> assert false
    and 
      _vars_of_static_arg s = function
      | ConstStaticArgLic(id,_)
      | TypeStaticArgLic(id,_)
      | NodeStaticArgLic(id,_) -> IdSet.add id s
  in
  aux IdSet.empty ve
(*********************************************************************************)

exception DepLoop of (Lxm.t * string)
exception Error of (Lxm.t * string * LicPrg.t)

type visit_status = Todo | Doing | Done
type visit_info = visit_status IdMap.t
let (status : Lv6Id.t -> visit_info -> visit_status) = IdMap.find
    
(* At init, all the idents are 'Todo' *)
let (visit_init: dependencies -> visit_info) =
  fun deps ->
    let f id _ acc = IdMap.add id Todo acc in
    IdMap.fold f deps IdMap.empty

let rec (visit : dependencies -> visit_info -> Lv6Id.t -> Lv6Id.t list -> visit_info) =
  fun deps vi id path ->
    assert (IdMap.mem id deps);
    let path = id::path in
    let lxm, iddeps = IdMap.find id deps in
    let iddeps =  (* filter out id that have no deps (inputs, pre, const)*)
      IdSet.filter (fun id -> IdMap.mem id deps) iddeps
    in
    let doing,iddeps = IdSet.partition (fun id -> status id vi = Doing) iddeps in
    let _ =
      if (not (IdSet.is_empty doing)) then 
        let id = IdSet.choose doing in
        let idl = List.rev (id::path) in
        let msg = "Dependency loop on "^id^": " ^ (String.concat "->" idl) ^ "\n" in
        raise (DepLoop (lxm,msg))
    in
    let to_visit,_ = IdSet.partition (fun id -> status id vi = Todo) iddeps in
    let vi = IdSet.fold 
      (fun id vi -> 
        let vi = IdMap.add id Doing vi in
        let vi = visit deps vi id path in
        let vi = IdMap.add id Done vi in
        vi
      ) 
      to_visit 
      vi 
    in
    vi

(* Update the dependency graph according to the information contained
   in the equation.  *)
let (update_dependencies : dependencies -> Lic.eq_info srcflagged -> dependencies) =
  fun deps { it = (ll,exp) ; src = lxm } ->
    let lvars = List.map (fun l -> (Lic.var_info_of_left l).var_name_eff) ll in
    let rvars = vars_of_exp  exp in
    let deps =
      List.fold_left
        (fun deps v -> 
          let deps = 
            try IdMap.add v (lxm,(IdSet.union (snd (IdMap.find v deps)) rvars)) deps 
            with Not_found -> IdMap.add v (lxm,rvars) deps
          in
          deps
        )
        deps
        lvars
    in
    deps

let (check_node : Lic.node_exp -> unit) =
  fun node -> 
    match node.def_eff with  
	   | ExternLic | MetaOpLic | AbstractLic _ -> ()
	   | BodyLic{ eqs_eff = eql;_ } ->
        let dependencies_init = IdMap.empty in
        let deps = List.fold_left update_dependencies dependencies_init eql in
        let vi = visit_init deps in
          let f id _ vi = visit deps vi id [] in
          ignore (IdMap.fold f deps vi)


(* exported *)
let (doit :  LicPrg.t -> unit) =
  fun inprg -> 
    let (do_node : Lic.node_key -> Lic.node_exp -> unit) = 
      fun _nk ne -> 
        check_node ne
    in
    try LicPrg.iter_nodes do_node inprg 
    with DepLoop(lxm,msg) -> raise (Error(lxm,msg,inprg))
