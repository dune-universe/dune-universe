open UtilsLib.IdGenerator
open UtilsLib.Utils

module Var=
struct
  type t=Var of int
  let compare (Var i) (Var j)=i-j
  let succ (Var i)=Var (i+1)
  let start=Var 0
  let to_string (Var i) = 
    let c = Printf.sprintf "%c" (char_of_int (97+i)) in
    c
    
end
  
module VarGen = IdGen(Var)
  
module Const=
struct
  type t=Const of int
  let compare (Const i) (Const j)=i-j
  let start=Const 0
  let succ (Const i)=Const (i+1)
  let to_string (Const i) = string_of_int i
end

module ConstGen=IdGen(Const)

module AbstractSyntax =
  struct

  module Log = (val Logs.src_log (Logs.Src.create "ACGtkLib.datalog_AbstractSyntax" ~doc:"logs ACGtkLib datalog_AbstractSyntax events") : Logs.LOG) 
    
  (** These modules are the abstract syntactic representations of the
      predicates, of the rules, and of the programs *)
  module Predicate =
  struct
    type term = 
    | Var of VarGen.id
    | Const of ConstGen.id
	
    module VarMap = Map.Make (Var)
      
    let map_content_compare (k1,map1) (k2,map2) =
      try
	let val1 = VarMap.find k1 map1 in
	(try
	   val1-(VarMap.find k2 map2)
	 with
	 | Not_found -> 1)
      with
      | Not_found -> 
	(try
	   let _ = VarMap.find k2 map2 in
	   -1	    
	 with
	 | Not_found -> 0)
	  
	  
	  
    let term_compare l1 l2 =
      let rec term_compare_aux l1 l2 (l1_vars,l2_vars) pos =
	match l1,l2 with
	| [],[] -> 0
	| [],_ -> -1
	| _,[] -> 1
	| (Const _)::_,(Var _)::_ -> 1
	| (Var _)::_,(Const _)::_ -> -1
	| (Const a1)::tl1,(Const a2)::tl2 ->
	  let res = ConstGen.compare a1 a2 in
	  if ConstGen.compare a1 a2 <> 0 then
	    res
	  else
	    term_compare_aux tl1 tl2 (l1_vars,l2_vars) (pos+1)
	| (Var a1)::tl1,(Var a2)::tl2 ->
	  let res = map_content_compare (a1,l1_vars) (a2,l2_vars) in
	  if VarGen.compare a1 a2 <> 0 then
	    res
	  else
	    term_compare_aux tl1 tl2 (VarMap.add a1 pos l1_vars,VarMap.add a2 pos l2_vars) (pos+1) in
      term_compare_aux l1 l2 (VarMap.empty,VarMap.empty) 0
	
	
    let term_to_string t cst_id_table = 
      match t with
      | Var v -> Var.to_string v
      | Const c -> (* Const.to_string c *)
	ConstGen.Table.find_sym_from_id c cst_id_table
	  
    type pred_id=int
      
    module PredIdMap = IntIdGen.IdMap
    module PredIdTable = IntIdGen.Table
      
    type predicate={p_id:pred_id;
		    arity:int;
		    arguments: term list
		   (** It is assumed that the size of the list is the
		       arity *)
		   }      
      
    let to_string predicate (*{p_id=p_id;arguments=parameters}*) pred_id_table cst_id_table=
      Printf.sprintf "%s(%s)"
	(PredIdTable.find_sym_from_id predicate.p_id pred_id_table)
	(string_of_list "," (fun p -> term_to_string p cst_id_table) predicate.arguments)
	
    let compare ({p_id=id1;arity=a1;arguments=l1}:predicate) ({p_id=id2;arity=a2;arguments=l2}:predicate) =
      let res = compare id1 id2 in
      if res<>0 then
	res
      else
	let res = a1-a2 in
	if res<>0 then
	  res
	else
	  term_compare l1 l2

    let fake_pred_id = -1
	    
    module PredIds=IntSet
      
  end
    
  module Proto_Rule =
  struct
    type t = {proto_id:int;
	      proto_lhs:Predicate.predicate;
	      proto_rhs:Predicate.predicate list;
	     (** represents the predicates of the rule *)
	     }
      
    let to_string r pred_id_table cst_id_table=
      let head=Predicate.to_string r.proto_lhs pred_id_table cst_id_table in
      let tail=
	match r.proto_rhs with
	| [] -> "."
	| _ -> 
	  Printf.sprintf
	    ":- %s."
	    (string_of_list "," (fun p -> Predicate.to_string p pred_id_table cst_id_table) r.proto_rhs) in
      Printf.sprintf "%s%s\n" head tail
	
	
    let to_buffer rules  pred_id_table cst_id_table = 
      let buff=Buffer.create 4 in
      let () =
	List.iter
	  (fun r -> Buffer.add_string
	    buff
	    (Printf.sprintf "%s\n" (to_string r pred_id_table cst_id_table)))
	  rules in
      buff

    let to_log rules  pred_id_table cst_id_table src level = 
      List.iter
	(fun r -> Logs.msg ~src level (fun m -> m "@;<4>%s" (to_string r pred_id_table cst_id_table)))
	rules
        
  end

  module Rule =
  struct
    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:(Predicate.predicate*int) list;
	       i_rhs:(Predicate.predicate*int) list;
	       i_rhs_num:int;
	      }
      
    let to_string r  pred_id_table cst_id_table =
      let head=Predicate.to_string r.lhs pred_id_table cst_id_table in
      let string_of_predicate_list lst = string_of_list "," (fun p -> Predicate.to_string p pred_id_table cst_id_table) lst in
      let vdash,e_i_sep =
	match r.e_rhs,r.i_rhs with
	| [],[] -> "",""
	| [],_ -> ":- "," "
	| _,[] -> ":- "," "
	| _,_ -> ":- "," , " in
      Printf.sprintf "%s%s%s%s%s." head vdash ((string_of_predicate_list >> fst >> List.split)  r.e_rhs) e_i_sep ((string_of_predicate_list >> fst >> List.split) r.i_rhs)
	
    module Rules=Set.Make(struct
      type t=rule
      let compare {id=i} {id=j} = i-j
    end)

    module RuleMap=Map.Make(struct
      type t=rule
      let compare {id=i} {id=j} = i-j
    end)


    let ids_to_rules ids id_to_rule_map =
      IntSet.fold
	(fun e acc -> Rules.add (IntMap.find e id_to_rule_map) acc)
	ids
	Rules.empty

    let to_buffer rules pred_id_table cst_id_table = 
      let buff=Buffer.create 4 in
      let () =
	Rules.iter
	  (fun r -> 
	    let () =
	      Buffer.add_string
		buff
		(to_string r pred_id_table cst_id_table) in
	    Buffer.add_string buff "\n")
	  rules in
      buff

    let to_log rules pred_id_table cst_id_table src level = 
      Rules.iter
	(fun r -> Logs.msg ~src level (fun m -> m "@;<4>%s" (to_string r pred_id_table cst_id_table)))
	rules


        
    let init_split_rhs proto_preds intensional_pred =
      let i_num,i_p,e_p,_=
	List.fold_left
	  (fun (i_num,i_preds,e_preds,i) ({Predicate.p_id=p_id} as pred) ->
	    if Predicate.PredIds.mem p_id intensional_pred 
	    then
	      (i_num+1,(pred,i)::i_preds,e_preds,i+1)
	    else
	      (i_num,i_preds,(pred,i)::e_preds,i+1))
	  (0,[],[],1)
	  proto_preds in
      i_num,i_p,e_p

    let update_split_rhs init proto_preds intensional_pred =
      List.fold_left
	(fun (i_preds,e_preds) (({Predicate.p_id=p_id},_) as pred) ->
	  if Predicate.PredIds.mem p_id intensional_pred 
	  then
	    (pred::i_preds,e_preds)
	    else
	      (i_preds,pred::e_preds))
	init
	proto_preds

    let extend_map_to_set k v map_to_set =
      let current_set = 
	try
	  Predicate.PredIdMap.find k map_to_set
	with
	| Not_found -> IntSet.empty in
      Predicate.PredIdMap.add k (IntSet.add v current_set) map_to_set


	
    let proto_rule_to_rule proto_rule intensional_pred =
      let i_num,i_preds,e_preds = 
	init_split_rhs proto_rule.Proto_Rule.proto_rhs intensional_pred in
      {id=proto_rule.Proto_Rule.proto_id;
       lhs=proto_rule.Proto_Rule.proto_lhs;
       e_rhs=List.rev e_preds;
       i_rhs=List.rev i_preds;
       i_rhs_num=i_num}	

    let update rule intensional_pred =
      let i_preds,e_preds = 
	update_split_rhs (rule.i_rhs,[]) rule.e_rhs intensional_pred in
      {rule with e_rhs=e_preds;i_rhs=i_preds}
  end

  module Proto_Program =
  struct
    type t =   {rules:Proto_Rule.t list;
		pred_table: Predicate.PredIdTable.table;
		const_table: ConstGen.Table.table;
		i_preds:Predicate.PredIds.t;
		rule_id_gen:IntIdGen.t;
		pred_to_rules:IntSet.t Predicate.PredIdMap.t}

    type tables = Predicate.PredIdTable.table*(VarGen.Table.table*ConstGen.Table.table)
      
    let empty = {rules=[];
		 pred_table=Predicate.PredIdTable.empty;
		 const_table=ConstGen.Table.empty;
		 i_preds=Predicate.PredIds.empty;
		 rule_id_gen=IntIdGen.init ();
		 pred_to_rules=Predicate.PredIdMap.empty}

    let extension pred_table const_table rule_id_gen=
      {rules=[];
       pred_table;
       const_table;
       i_preds=Predicate.PredIds.empty;
       rule_id_gen;
       pred_to_rules=Predicate.PredIdMap.empty}

    let add_proto_rule (f_lhs,f_rhs) prog =
      let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id prog.rule_id_gen in
      let lhs,(new_pred_id_table,new_tables)=f_lhs (prog.pred_table,(VarGen.Table.empty,prog.const_table)) in
      let rhs,(new_pred_id_table',(_,new_const_table))=f_rhs (new_pred_id_table,new_tables) in
      let new_i_preds=
	match rhs with
	| [] -> prog.i_preds
	| _ -> Predicate.PredIds.add lhs.Predicate.p_id prog.i_preds in
      let new_rule =  {Proto_Rule.proto_id=rule_id;
		       Proto_Rule.proto_lhs=lhs;
		       Proto_Rule.proto_rhs=rhs} in
      {rules=new_rule::prog.rules;
       pred_table=new_pred_id_table';
       const_table=new_const_table;
       i_preds=new_i_preds;
       rule_id_gen=new_rule_id_gen;
       pred_to_rules=
	  List.fold_left
	    (fun acc p -> Rule.extend_map_to_set p.Predicate.p_id rule_id acc)
	    prog.pred_to_rules
	    rhs
      }
      

  end

  module Program =
  struct
    type program =  {rules:Rule.Rules.t;
		     pred_table: Predicate.PredIdTable.table;
		     const_table: ConstGen.Table.table;
		     i_preds:Predicate.PredIds.t;
		     rule_id_gen:IntIdGen.t;
		     e_pred_to_rules: Rule.Rules.t Predicate.PredIdMap.t}

    type modifier = {modified_rules:Rule.Rules.t;
		     new_pred_table: Predicate.PredIdTable.table;
		     new_const_table: ConstGen.Table.table;
		     new_i_preds:Predicate.PredIds.t;
		     new_e_preds:Predicate.PredIds.t;
		     new_rule_id_gen:IntIdGen.t;}

      
    let make_program {Proto_Program.rules;Proto_Program.pred_table;Proto_Program.const_table;Proto_Program.i_preds;Proto_Program.rule_id_gen;Proto_Program.pred_to_rules}=
      let actual_rules,ids_to_rule_map = 
	List.fold_left 
	  (fun (acc,ids_to_rule_map) p_rule -> 
	    let rule = Rule.proto_rule_to_rule p_rule i_preds in
	    Rule.Rules.add rule acc,
	    IntMap.add p_rule.Proto_Rule.proto_id rule ids_to_rule_map)
	  (Rule.Rules.empty,IntMap.empty)
	  rules in
      {rules=actual_rules;
       pred_table=pred_table;
       const_table=const_table;
       i_preds=i_preds;
       rule_id_gen=rule_id_gen;
       e_pred_to_rules=
	  Predicate.PredIdMap.fold
	    (fun p rule_ids acc -> 
	      if Predicate.PredIds.mem p i_preds then
		Predicate.PredIdMap.remove p acc
	      else
		Predicate.PredIdMap.add p (Rule.ids_to_rules rule_ids ids_to_rule_map) acc)
	    pred_to_rules
      Predicate.PredIdMap.empty}
	
	
    let extend prog {Proto_Program.rules;Proto_Program.pred_table;Proto_Program.const_table;Proto_Program.i_preds;Proto_Program.rule_id_gen;Proto_Program.pred_to_rules}=
      let new_i_preds = Predicate.PredIds.union prog.i_preds i_preds in
      let updated_e_pred_map_to_r,updated_rules =
	(* all the rules that were pointed to by an extensional
	   predicate that has be turned into an intensional predicate
	   because of the program extension have to be updated *)
	(* We check if some the new intensional predicates also are
	   keys of the e_pred_to_rules map of the previous program *)
	Predicate.PredIds.fold
	  (fun p_id ((e_p_to_r,rules_acc) as acc) -> 
	    try
	      (* First we check wether this predicate was considered
		 as an extensional one *)
	      let to_be_modified_rules =
		Predicate.PredIdMap.find p_id prog.e_pred_to_rules in
	      (* If yes, we nee to remove it from the map *)
	      Predicate.PredIdMap.remove p_id e_p_to_r,
	      (* And to modify the rules it pointed to *)
	      Rule.Rules.fold
		(fun r acc -> 
		  Rule.Rules.add (Rule.update r new_i_preds) (Rule.Rules.remove r acc))
		to_be_modified_rules
		rules_acc
	    with
	    (* If no, don't do anything *)
	    | Not_found -> acc)
	  i_preds
	  (prog.e_pred_to_rules,prog.rules) in
      let new_rules,id_to_rule_map = 
	List.fold_left 
	  (fun (acc,id_to_rule_map) p_rule -> 
	    let rule=Rule.proto_rule_to_rule p_rule new_i_preds in
	    Rule.Rules.add rule acc,
	    IntMap.add p_rule.Proto_Rule.proto_id rule id_to_rule_map)
	  (updated_rules,IntMap.empty)
	  rules in
      {rules=new_rules;
       pred_table=pred_table;
       const_table=const_table;
       i_preds=new_i_preds;
       rule_id_gen=rule_id_gen;
       e_pred_to_rules=
	  Predicate.PredIdMap.merge
	    (fun p opt_rule_ids opt_rules ->
	      match opt_rule_ids, opt_rules with
	      | None,None -> None
	      | None,_ -> opt_rules
	      | Some ids,None -> Some (Rule.ids_to_rules ids id_to_rule_map)
	      | Some ids,Some rules ->
		Some (Rule.Rules.union rules (Rule.ids_to_rules ids id_to_rule_map)))
	    pred_to_rules
	    updated_e_pred_map_to_r
      }

	
	
    let to_buffer prog =
      let buff = Rule.to_buffer prog.rules prog.pred_table prog.const_table in
      let () = Buffer.add_string buff "Intensional predicates are:\n" in
      let () =
	Predicate.PredIds.iter
	  (fun elt -> Buffer.add_string buff (Printf.sprintf "\t%s\n%!" (Predicate.PredIdTable.find_sym_from_id elt prog.pred_table)))
	  prog.i_preds in
      buff
        
    let log_content ?(src=Logs.default) level prog =
      let () = Rule.to_log prog.rules prog.pred_table prog.const_table src level in
      let () = Logs.msg ~src level (fun m -> m "Intensional predicates are:") in
      let () = Predicate.PredIds.iter
	         (fun elt ->
                   Logs.msg
                     ~src
                     level
                     (fun m -> m "@;<4>%s" (Predicate.PredIdTable.find_sym_from_id elt prog.pred_table)))
	         prog.i_preds in
      Logs.msg
        ~src
        level
        (fun m -> m "Done.")
  end
    
  end

