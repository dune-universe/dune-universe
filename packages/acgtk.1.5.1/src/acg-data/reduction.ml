open Logic.Lambda
open UtilsLib.Utils
open DatalogLib.Datalog_AbstractSyntax
open DatalogLib.Datalog
  
module Make(Sg:Interface.Signature_sig with type  term = Lambda.term and type stype = Lambda.stype) =
struct

  let src = Logs.Src.create "ACGtkLib.reduction" ~doc:"logs ACGtkLib reduction events"
  module Log = (val Logs.src_log src : Logs.LOG) 

  
  let rec sequentialize_rev stype sequence =
    match stype with
    | Lambda.Atom i -> i::sequence
    | Lambda.DAtom _ -> failwith "Bug: type definition should be unfolded"
    | Lambda.LFun (alpha,beta)
    | Lambda.Fun (alpha,beta) -> sequentialize_rev beta (sequentialize_rev alpha sequence)
    | _ -> failwith "Bug: Not a 2nd order type"

  let sequentialize stype = List.rev (sequentialize_rev stype [])

  (** [map_types abs_type obj_type sg] returns a list of triple
      [(id_n,name_n,image_n);...;(id_2,name_2,image_2);(id_1,name_1,image_1)]
      where [abst_type=Atom(id_1) -> Atom(id_2) -> ... Atom(id_n)] and
      is defined as [name_1 -> name_2 -> ... -> name_n] and
      [obj_type=image_1 -> image_2 -> ... -> image_n]. Note that the
      list is in the {em reverse order} and that [abs_type] should be
      2nd order. *)

  let map_types abs_type obj_type sg  =
    let rec map_types_aux abs_type obj_type lst =
      Log.debug (fun m -> m "Mapping (aux) type:%s" (Sg.type_to_string abs_type sg));
      Log.debug (fun m -> m "On (aux):          %s" (Lambda.raw_type_to_string obj_type));
      match abs_type,obj_type with
      | Lambda.Atom i,_ -> (i,Sg.type_to_string abs_type sg,obj_type)::lst
      | Lambda.DAtom _,_ -> failwith (Printf.sprintf "Bug: type definition in \"%s\" as \"%s\" should be unfolded" (Sg.type_to_string abs_type sg) (Lambda.raw_type_to_string abs_type))
      | Lambda.LFun (Lambda.Atom i as alpha,beta),Lambda.Fun (alpha',beta')
      | Lambda.Fun (Lambda.Atom i as alpha,beta),Lambda.Fun (alpha',beta') -> 
	map_types_aux beta beta' ((i,Sg.type_to_string alpha sg,alpha')::lst)
      | Lambda.LFun _,Lambda.Fun _
      | Lambda.Fun _,Lambda.Fun _ -> failwith "Bug: should be 2nd order type for abstract constant"
      | _,_ -> failwith "Bug: Not a 2nd order type or not corresponding abstract and object type" in
    Log.debug (fun m -> m "Mapping type:%s (%s)" (Sg.type_to_string abs_type sg) (Lambda.raw_type_to_string abs_type));
    Log.debug (fun m -> m "On:          %s" (Lambda.raw_type_to_string obj_type));
    map_types_aux abs_type obj_type []


  (** [build_predicate_w_var_args (name,obj_type)
      (prog,var_gen,type_to_var_map)] returns [(prog',var_gen',type_to_var_map')] where:
      - [name] is the name of an abstract type of some ACG that
      has to be turned into a predicate of the associated datalog
      program
      - [ob_type] is the principal type of its realisation by this ACG
      - [prog] is the current associated datalog program
      - [var_gen] is a variable generator that records the variable
      associated with this predicate (according to the principal type
      [obj_type] of its image). It can be updated to [var_gen'] if
      some new variables are needed
      - [type_to_var_map] records to which variable each atomic type
      of the principal type is associated with. It can be updated to
      [type_to_var_map'] if some new variables were needed.
      - [prog'] is [prog] where the bew predicate has been added
  *)
  let build_predicate_w_var_args (name,obj_type) (prog,var_gen,type_to_var_map) =
    let atom_sequence = sequentialize_rev obj_type [] in
    Log.debug (fun m -> m "Build predicate from %s:%s   ([%s])" name (Lambda.raw_type_to_string obj_type) (string_of_list ";" string_of_int atom_sequence));
    let var_sequence,var_gen,type_to_var_map =
      List.fold_left
	(fun (l_var_seq,l_var_gen,l_type_to_var_map) i ->
	  let var,l_var_gen,l_type_to_var_map=
	    try
	      IntMap.find i l_type_to_var_map,l_var_gen,l_type_to_var_map
	    with
	    | Not_found ->
	      let var,l_var_gen=VarGen.get_fresh_id l_var_gen in
	      var,l_var_gen,IntMap.add i var l_type_to_var_map in
	  (AbstractSyntax.Predicate.Var var)::l_var_seq,l_var_gen,l_type_to_var_map)
	([],var_gen,type_to_var_map)
	atom_sequence in
    let p_id,prog=Datalog.Program.add_pred_sym name prog in
    AbstractSyntax.Predicate.({p_id=p_id;arity=List.length var_sequence;arguments=var_sequence}),
    (prog,var_gen,type_to_var_map)

  (** [build_predicate_w_cst_args (name,obj_type) prog] returns a fully instantiated predicate where:
      - [name] is the name of an abstract type of some ACG that has to
      be turned into a the querying predicate for the associated
      datalog program
      - [ob_type] is the principal type of its realisation by this ACG
      that is interpreted as Datalog constants
      - [prog] is the current associated datalog program
  *)
  let build_predicate_w_cst_args (name,obj_type) prog =
    let atom_sequence = sequentialize obj_type in
    Log.debug (fun m -> m "Build predicate from %s:%s   ([%s])" name (Lambda.raw_type_to_string obj_type) (string_of_list ";" string_of_int atom_sequence));
    let const_sequence,prog =
      List.fold_left
	(fun (l_const_seq,l_prog) i ->
	  let const_id,l_prog=Datalog.Program.get_fresh_cst_id (string_of_int i) l_prog in
	  (AbstractSyntax.Predicate.Const const_id)::l_const_seq,l_prog)
	([],prog)
	atom_sequence in
    let p_id,prog=Datalog.Program.add_pred_sym name prog in
    AbstractSyntax.Predicate.({p_id=p_id;arity=List.length const_sequence;arguments=List.rev const_sequence}),prog

    
  (** [generate_and_add_rule ~abs_cst ~obj_princ_type ~obj_typing_env
      prog abs_sig obj_sig] returns a pair [(r,prog')] where:
      - [r] is the generated rule
      - [prog'] is [prog] where the rule [r] has been added
      - [abs_cst] is the abstract constant from the abstract signature
      [abs_sig] that triggers the rule generation
      - [obj_princ_type] is the principal type of the image by the
      lexicon of [abs_cst]
      - [obj_typing_env] is its typing environment. The latter maps
      the position of the object constants in the realisation of
      [abs_cst] to a pair [(t,ty)] where [t] is the object constant
      itself, and [ty] the type associated by the principal typing
      environment.
      - [prog] is the current datalog program
      - [abs_sig] and [obj_sig] are the abstract signature and the
      object signature of some ACG. *)

  let generate_and_add_rule
      ~abs_cst:(_,abs_t_type)
      ~obj_princ_type:principle_type
      ~obj_typing_env:env
      prog
      ~abs_sig
      ~obj_sig =
    let rule_id,prog=Datalog.Program.get_fresh_rule_id prog in
    let type_lst = map_types abs_t_type principle_type abs_sig in
    match type_lst with
    | [] -> failwith "Bug: there should be a type correspondance"
    | (_,name,image)::tl ->
      let lhs,(prog,var_gen,type_to_var_map) = build_predicate_w_var_args (name,image) (prog,VarGen.init (),IntMap.empty) in
      let i_rhs,length,(prog,var_gen,type_to_var_map) =
	List.fold_left
	  (fun (rhs,l_length,l_tables) (_,l_name,l_image) ->
	    let new_pred,new_tables=build_predicate_w_var_args (l_name,l_image) l_tables in
	    let l_length=l_length+1 in
	    (new_pred,l_length)::rhs,l_length,new_tables)
	  ([],0,(prog,var_gen,type_to_var_map))
	  tl
      in
      let e_rhs,_,(prog,_,_) =
	IntMap.fold
	  (fun _ (cst,cst_type) (rhs,l_length,l_tables) ->
	    let const_name=Sg.term_to_string cst obj_sig in
	    let () = assert (fst (Sg.is_constant const_name obj_sig)) in
	    let new_pred,new_tables = build_predicate_w_var_args (const_name,cst_type) l_tables in
	    let l_length=l_length+1 in
	    (new_pred,l_length)::rhs,l_length,new_tables)
	  env
	  ([],0,(prog,var_gen,type_to_var_map) ) in
      Log.debug (fun m -> m "Correctly set the number of intensional predi in rhs: %d" (let () = assert (length=List.length i_rhs) in length));
      let new_rule = AbstractSyntax.Rule.({id=rule_id;lhs;e_rhs;i_rhs;i_rhs_num=length}) in
      new_rule,Datalog.Program.add_rule ~intensional:true new_rule prog

  (** [edb_and_query ~obj_term ~obj_type ~obj_typing_env ~dist_type
      prog ~abs_sig ~obj_sig] returns a pair [(q,prog')] where:
      - [q] is the predicate corresponding to the query generated by
      the object term [obj_term] to parse
      - [prog'] is [prog] where the extensional database resulting
      from the reduction of the object term [obj_term]
      - [obj_type] is the principal type of [obj_term]
      - [obj_typing_env] is its typing environment. The latter maps
      the position of the object constants in the realisation of
      [abs_cst] to a pair [(t,ty)] where [t] is the object constant
      itself, and [ty] the type associated by the principal typing
      environment.
      - [dist_type] is the distinguished type of the ACG
      - [prog] is the current datalog program
      - [abs_sig] and [obj_sig] are the abstract signature and the
      object signature of some ACG. *)
  let edb_and_query ~obj_term ~obj_type ~obj_typing_env ~dist_type prog ~abs_sig ~obj_sig = 
    (* It makes the assumption that no constant has been
     previously defined or used in the program *)
    let type_lst = map_types dist_type obj_type abs_sig in
    match type_lst with
    | [] -> failwith "Bug: there should be a type correspondance"
    | [_,name,image] ->
      let e_facts,prog=
	IntMap.fold
	  (fun _ (cst,cst_type) (l_facts,l_prog) ->
	    let const_name=Sg.term_to_string cst obj_sig in
	    let () = assert (fst (Sg.is_constant const_name obj_sig)) in
	    let new_pred,l_prog = build_predicate_w_cst_args (const_name,cst_type) l_prog in
	    let rule_id,l_prog=Datalog.Program.get_fresh_rule_id l_prog in
	    let new_fact = AbstractSyntax.Rule.({id=rule_id;lhs=new_pred;e_rhs=[];i_rhs=[];i_rhs_num=0}) in
	    (new_fact::l_facts),l_prog)
	  obj_typing_env
	  ([],prog) in
      let prog=Datalog.Program.add_e_facts prog (e_facts,prog.Datalog.Program.const_table,prog.Datalog.Program.rule_id_gen) in
      build_predicate_w_cst_args (name,image) prog 
    | (_,_,_)::tl -> failwith "Bug: querying non atomic types is not yet implemented"
		
		
end
