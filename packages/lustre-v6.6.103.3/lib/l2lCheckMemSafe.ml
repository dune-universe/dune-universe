(** Time-stamp: <modified the 29/08/2019 (at 15:40) by Erwan Jahier> *)

open Lxm
open Lic
 
let _dbg =  (Lv6Verbose.get_flag "check-mem")


(********************************************************************************)
(* exported *)
let  (is_memoryless_val_exp : LicPrg.t -> Lic.val_exp  -> bool) =
  fun licprg ve ->
    let rec (aux_val_exp : Lic.val_exp  -> bool) =
      fun ve -> aux_val_exp_core ve.ve_core
    and aux_val_exp_core ve_core = 
      match ve_core with
        | CallByPosLic(op, vel) -> (
          (List.for_all aux_val_exp vel) && aux_pos_op op.it 
        )
        | CallByNameLic(_,cl) -> (
          let vel = (snd (List.split cl)) in
          List.for_all aux_val_exp vel
        )
        | Merge(ve,cl) -> (
          let vel = ve::(snd (List.split cl)) in
          List.for_all aux_val_exp vel
        )
    and (aux_pos_op: Lic.by_pos_op -> bool) =
      function
        | CALL({it=nk;_}) | PREDEF_CALL({it=nk;_}) -> (
          match LicPrg.find_node licprg nk with
            | None -> true (* SNO *)
            | Some node_exp -> not node_exp.has_mem_eff
        )
        | CURRENT _ | PRE | ARROW | FBY 
          -> false
        | HAT (_) | ARRAY| ARRAY_SLICE(_) | ARRAY_ACCES(_) | STRUCT_ACCESS(_) 
        | WHEN _ | TUPLE  | CONCAT | CONST _ | CONST_REF  _ | VAR_REF _ 
          -> true
    in
    aux_val_exp ve


let (check_memory : LicPrg.t -> bool -> val_exp -> Lxm.t -> bool -> bool) =
  fun licprg should_have_mem ve lxm warning ->
    match should_have_mem, not (is_memoryless_val_exp licprg ve) with
      | false, false 
      | true,  true -> false (* disable the warning *) 
      | false, true ->
        let msg = "Error: this call uses memory from a function." in
        raise (Lv6errors.Compile_error(lxm,msg))
      | true, false -> warning

(********************************************************************************)
(* exported *)
let  (is_safe_val_exp : LicPrg.t -> Lic.val_exp -> bool) =
  fun licprg ve ->
    let rec (aux_val_exp : Lic.val_exp -> bool) =
      fun ve -> aux_val_exp_core ve.ve_core
    and aux_val_exp_core ve_core = 
      match ve_core with
        | CallByPosLic(op, vel) -> (
          (List.for_all aux_val_exp vel) && aux_pos_op op.it 
        )
        | CallByNameLic(_,cl) -> (
          let vel = (snd (List.split cl)) in
          List.for_all aux_val_exp vel
        )
        | Merge(ve,cl) -> (
          let vel = ve::(snd (List.split cl)) in
          List.for_all aux_val_exp vel
        )
    and (aux_pos_op: Lic.by_pos_op -> bool) =
      function
        | CALL({it=nk;_}) | PREDEF_CALL({it=nk;_}) -> (
          match LicPrg.find_node licprg nk with
            | None -> true (* SNO *)
            | Some node_exp -> node_exp.is_safe_eff
        )
        | CURRENT _ | PRE | ARROW | FBY 
        | HAT (_) | ARRAY| ARRAY_SLICE(_) | ARRAY_ACCES(_) | STRUCT_ACCESS(_) 
        | WHEN _ | TUPLE  | CONCAT | CONST _ | CONST_REF  _ | VAR_REF _ 
          -> true
    in
    aux_val_exp ve

let (check_safety : LicPrg.t -> bool -> val_exp -> Lxm.t -> bool -> bool) =
  fun licprg should_be_safe ve lxm warning ->
    match should_be_safe,  is_safe_val_exp licprg ve with
      | false, false
      | true,  true -> false (* disable the warning *) 
      | true, false ->
        let msg = "Error: calling an unsafe node from a safe one." in
        raise (Lv6errors.Compile_error(lxm,msg))
      | false, true -> warning


(********************************************************************************)
let (check_node : LicPrg.t -> Lic.node_exp -> unit) =
  fun licprg node -> 
    match node.def_eff with  
	   | ExternLic | MetaOpLic | AbstractLic _ -> ()
	   | BodyLic{ eqs_eff = eql ; asserts_eff = vel } ->
        let warning = true in
        let warning = List.fold_left
          (fun warn eq -> check_memory licprg node.has_mem_eff (snd eq.it) eq.src warn) 
          warning eql
        in
        let warning = List.fold_left 
          (fun warn eq -> check_memory licprg node.has_mem_eff eq.it eq.src warn) 
          warning vel
        in 
        if warning then (
          let id = Lic.string_of_node_key node.node_key_eff in
          let msg = Printf.sprintf
            "%s is declared as a node, but it uses no memory (i.e., it is a function)." 
            id 
          in
          Lv6errors.warning node.lxm msg;
        );
        let warning = true in
        let warning = List.fold_left 
          (fun warn eq -> check_safety licprg node.is_safe_eff (snd eq.it) eq.src warn) 
          warning eql
        in
        let warning = List.fold_left
          (fun warn eq -> check_safety licprg node.is_safe_eff eq.it eq.src warn) 
          warning vel
        in
        if warning then (
          let id = Lic.string_of_node_key node.node_key_eff in
          let msg = Printf.sprintf "%s is declared as unsafe, but is safe." id in
          Lv6errors.warning node.lxm msg
        )


(* exported *)
let (doit :  LicPrg.t -> unit) =
  fun inprg -> 
    let (do_node : Lic.node_key -> Lic.node_exp -> unit) = 
      fun _nk ne -> check_node inprg ne
    in
    LicPrg.iter_nodes do_node inprg
    
