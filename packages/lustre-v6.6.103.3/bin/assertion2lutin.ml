(* Time-stamp: <modified the 29/08/2019 (at 17:03) by Erwan Jahier> *)


open Lic
open Lv6MainArgs
open Lxm
    
(*  *)
exception Not_handled


let rec is_not_atomic = function
  | CallByPosLic({it=CONST_REF _;_} ,_) 
  | CallByPosLic({it=VAR_REF _;_}, _ )  -> false 
  | CallByPosLic({it=TUPLE;_} ,[ve])  -> is_not_atomic ve.ve_core
  | _ -> true


let rec (string_of_val_exp_eff : Lic.val_exp  -> string) =
  fun ve  -> 
    string_of_val_exp_eff_core ve.ve_core

and string_of_val_exp_eff_core ve_core = 
  match ve_core with
  | CallByPosLic (by_pos_op_eff, vel) ->
    (* ICI : on pourrait afficher en commentaire l'éventuel type_matches ? *)
    (string_of_by_pos_op_eff by_pos_op_eff vel) 

  | Merge _
  | CallByNameLic _ -> raise Not_handled

and (string_of_by_pos_op_eff: Lic.by_pos_op srcflagged -> Lic.val_exp list -> string) =
  fun posop vel -> 
    let tuple vel = (String.concat ", " (List.map string_of_val_exp_eff vel)) in
    let tuple_par vel = "(" ^ (tuple vel) ^ ")" in
    let str = 
      match posop.it,vel with
      | CONST c,_ -> LicDump.string_of_const_eff true c 
      | CALL ({it=("Lustre","not"),[];_}), [ve1]
      | PREDEF_CALL ({it=("Lustre","not"),[];_}), [ve1] ->
        ((AstPredef.op2string AstPredef.NOT_n) ^ " " ^ (tuple_par [ve1]))

      | CALL ({it=("Lustre","diese"),[];_}), _ 
      | CALL ({it=("Lustre","nor"),[];_}), _ -> raise Not_handled 
      | PREDEF_CALL ({it=("Lustre","nor"),[];_}), [_ve1] -> raise Not_handled

      | CALL ({it=("Lustre","if"),[];_}), [ve1; ve2; ve3] 
      | PREDEF_CALL ({it=("Lustre","if"),[];_}), [ve1; ve2; ve3] ->
        let ve2str = string_of_val_exp_eff ve2 in 
        let ve2str = if LicDump.is_a_tuple ve2 then "("^ve2str^")" else ve2str in
        let ve3str = string_of_val_exp_eff ve3 in 
        let ve3str = if LicDump.is_a_tuple ve3 then "("^ve3str^")" else ve3str in
        " if " ^ (string_of_val_exp_eff ve1) ^ 
        " then " ^ ve2str ^ " else " ^ ve3str

      | CALL(op), vel 
      | PREDEF_CALL(op), vel -> (
          if AstPredef.is_a_predef_op (snd(fst op.it)) then
            let op_str = snd (fst op.it) in
            let op_short_str = AstPredef.op2string (AstPredef.string_to_op op_str) in
            if AstPredef.is_infix (AstPredef.string_to_op op_str) then (
              match vel with 
              | [ve1; ve2] -> 
                "("^(string_of_val_exp_eff ve1) ^ ") " ^ op_short_str ^ 
                " (" ^ (string_of_val_exp_eff ve2) ^ ")"
              | _ -> assert false
            ) 
            else 
              (op_short_str ^
               (match op_str with
                | "true" | "false"  ->  tuple vel
                | _ -> tuple_par vel 
               )
              )
          else
            let nk = op.it in
            ((string_of_node_key nk) ^ (tuple_par vel))

        )
      | CONST_REF idl, _ -> LicDump.dump_long true idl
      | VAR_REF id, _ -> id
      | PRE, [ve] ->
        if is_not_atomic ve.ve_core then raise Not_handled else
          "pre "  ^ (string_of_val_exp_eff ve)
      | ARROW, [ve1; ve2] -> 
        (if LicDump.is_a_tuple ve1 then tuple_par [ve1] else string_of_val_exp_eff ve1) ^
        " fby loop { " ^ 
        (if LicDump.is_a_tuple ve1 then tuple_par [ve2] else string_of_val_exp_eff ve2) ^
        " } "
      | FBY, [ve1; ve2] -> 
        if is_not_atomic ve1.ve_core then raise Not_handled else
          (if LicDump.is_a_tuple ve1 then tuple_par [ve1] else string_of_val_exp_eff ve1)
          ^ " fby loop pre " ^ 
          (if LicDump.is_a_tuple ve1 then tuple_par [ve2] else string_of_val_exp_eff ve2)
      | TUPLE,_ -> (tuple vel)

      | ARRAY_ACCES(i), [ve1] ->
        (string_of_val_exp_eff ve1) ^ "_" ^ (string_of_int i) 
      | STRUCT_ACCESS(id), [ve1] ->
        (string_of_val_exp_eff ve1) ^ "_" ^ (Lv6Id.to_string id)

      | STRUCT_ACCESS _, _
      | PRE, _
      | WHEN _, _ 
      | CURRENT _ ,_ 
      | CONCAT, _
      | HAT (_), _
      | ARRAY, _ 
      | ARRAY_SLICE(_), _
      | ARROW, _ 
      | FBY, _ 
      | ARRAY_ACCES(_), _ -> raise Not_handled
    in
    let do_not_parenthesize = function 
      | VAR_REF _,_
      | CONST_REF _,_
      | PREDEF_CALL({it=("Lustre","true"),[];_}),_
      | PREDEF_CALL({it=("Lustre","false"),[];_}),_
      | ARRAY_ACCES _,_
      | STRUCT_ACCESS _,_ -> true   
      | _,_ ->  false 
    in 
    if 
      (* already parenthesized *)
      ( Str.string_match (Str.regexp "^(") str 0 && 
        Str.string_match (Str.regexp ")$") str 0 ) 
      || 
      (* ident or predef constants *)
      (do_not_parenthesize (posop.it,vel)) 
      ||
      global_opt.one_op_per_equation
    then 
      str 
    else 
      ("(" ^ str ^ ")")
    


let (f: Lic.val_exp -> string) =
  fun ve -> 
    try " loop {"^string_of_val_exp_eff ve^"}"
    with Not_handled -> ""
