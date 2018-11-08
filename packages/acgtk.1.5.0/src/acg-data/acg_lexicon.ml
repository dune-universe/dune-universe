(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2018 INRIA                             *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open UtilsLib
open Logic       
open Abstract_syntax
open Lambda
open Signature 

module Log = (val Logs.src_log (Logs.Src.create "ACGtkLib.acg_lexicon" ~doc:"logs ACGtkLib acg_lexicon events") : Logs.LOG) 
       
module Make (Sg:Interface.Signature_sig with type  term = Lambda.term and type stype = Lambda.stype) =
struct

  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation
  exception Missing_interpretation of string

  (*  module Dico = Utils.StringMap *)
  type kind =
    | Type
    | Cst

  module Pair =
    struct
      type t = string * kind
      let compare (s,k) (s',k') =
        match k,k' with
        | Type , Cst -> -1
        | Cst , Type -> 1
        | _ , _ -> String.compare s s'
    end

  module Dico = Map.Make(Pair)
                  
  module Signature=Sg



                     
  type signature = Sg.t

  type resume = int SharedForest.SharedForest.resumption


  type interpretation =
    | Type of (Abstract_syntax.location * Lambda.stype )
    | Constant of (Abstract_syntax.location * Lambda.term )


  let interpretation_to_string abstract_type_or_cst_id fun_type_from_id i sg =
    match i with
    | Type (_,t) -> Printf.sprintf "\t%s" (Signature.type_to_string t sg)
    | Constant (_,c) -> 
	let eta_long = Sg.eta_long_form c (fun_type_from_id abstract_type_or_cst_id) sg  in
	  Printf.sprintf "\t%s [eta-long form: %s {%s}]" (Sg.term_to_string c sg) (Sg.term_to_string eta_long sg ) (Lambda.raw_to_string eta_long)


  module Datalog=DatalogLib.Datalog.Datalog
  module DatalogAbstractSyntax=DatalogLib.Datalog_AbstractSyntax.AbstractSyntax

  module RuleToCstMap=Utils.IntMap

  type datalog_correspondance = {prog:Datalog.Program.program;
				 rule_to_cst: Lambda.term RuleToCstMap.t;
				 cst_to_rule: int Utils.IntMap.t}
    
  type 'a build = 
    | Interpret of (Sg.t*Sg.t)
    | Compose of ('a*'a)

  type t = {name:string*Abstract_syntax.location;
	    dico:interpretation Dico.t;
	    syntax_dico:Abstract_syntax.lex_entry Dico.t option;
	    non_linear_interpretation:bool;
	    abstract_sig:Sg.t;
	    object_sig:Sg.t;
	    (*	    datalog_prog:(Datalog.Program.program * Lambda.term RuleToCstMap.t) option; *)
	    datalog_prog: datalog_correspondance option;
	    build:t build;
	    timestamp:float}


  type dependency =
    | Signatures of (signature*signature)
    | Lexicons of (t*t)

  type data =
    | Signature of signature
    | Lexicon of t



  let get_dependencies lex = 
    match lex.build with
    | Interpret s -> Signatures s
    | Compose l -> Lexicons l

  let name {name=n}=n

  let get_sig {abstract_sig=abs;object_sig=obj} = abs,obj

  let empty name  ?(non_linear=false) ~abs ~obj = 
    let prog =
      (*      if (Sg.is_2nd_order abs) && (not non_linear) then *)
      if (Sg.is_2nd_order abs) then
	(*	Some (Datalog.Program.empty,RuleToCstMap.empty)  *)
	Some {prog=Datalog.Program.empty;
	      rule_to_cst=RuleToCstMap.empty;
	      cst_to_rule=Utils.IntMap.empty}
      else
	None in
    {name=name;
     dico=Dico.empty;
     syntax_dico=Some Dico.empty;
     abstract_sig=abs;
     object_sig=obj;
     datalog_prog=prog;
     non_linear_interpretation=non_linear;
     build = Interpret (abs,obj);
     timestamp = Unix.time ()}

      
  let is_linear {non_linear_interpretation} = not non_linear_interpretation

  let emit_missing_inter lex lst =
    let lex_name,loc = name lex in
    let abs_name,_ = Sg.name lex.abstract_sig in
      raise (Error.Error (Error.Lexicon_error (Error.Missing_interpretations(lex_name,abs_name,lst),loc)))

  let rec interpret_type abs_ty ({abstract_sig=abs_sg;dico=dico} as lex) =
    match abs_ty with
    | Lambda.Atom i -> 
       (let abs_ty_as_str = Sg.type_to_string abs_ty abs_sg in
	match Dico.find (abs_ty_as_str,Type) dico with
	| Type (_,obj_ty) -> Sg.expand_type obj_ty lex.object_sig
	| Constant _ -> failwith "Bug"
        | exception Not_found -> emit_missing_inter lex [abs_ty_as_str])
    | Lambda.DAtom i -> interpret_type (Sg.unfold_type_definition i abs_sg) lex
    | Lambda.LFun (ty1,ty2) ->
       if lex.non_linear_interpretation then
         Lambda.Fun (interpret_type ty1 lex,interpret_type ty2 lex)
       else
         Lambda.LFun (interpret_type ty1 lex,interpret_type ty2 lex)
    | Lambda.Fun (ty1,ty2) -> Lambda.Fun (interpret_type ty1 lex,interpret_type ty2 lex)
    | _ -> failwith "Not yet implemented"
      
  let rec interpret_term abs_t ({abstract_sig=abs_sg;dico=dico} as lex) =
    match abs_t with
    | Lambda.Var _ -> abs_t
    | Lambda.LVar i -> 
       if lex.non_linear_interpretation then
         Lambda.Var i
       else
         abs_t
    | Lambda.Const i -> 
       (let abs_term_as_str = Sg.term_to_string abs_t abs_sg in
	match Dico.find (abs_term_as_str,Cst) dico with
	| Constant (_,obj_t) -> obj_t
	| Type _ -> failwith "Bug"
        | exception Not_found -> emit_missing_inter lex [abs_term_as_str] )
    | Lambda.DConst i -> 
      interpret_term (Sg.unfold_term_definition i abs_sg) lex
    | Lambda.Abs(x,t) -> Lambda.Abs(x,interpret_term t lex)
    | Lambda.LAbs(x,t) -> 
       if lex.non_linear_interpretation then
         Lambda.Abs(x,interpret_term t lex)
       else
         Lambda.LAbs(x,interpret_term t lex)
    | Lambda.App(t,u) -> Lambda.App(interpret_term t lex,interpret_term u lex)
    | _ -> failwith "Not yet implemented"
    
  let interpret t ty lex = 
    let t_interpretation = interpret_term t lex in
    let t_interpretation = Lambda.normalize ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig) t_interpretation in
    let ty_interpretation = interpret_type ty lex in
      t_interpretation,ty_interpretation

  module Reduction=Reduction.Make(Sg)


  (*  let add_rule_for_cst_in_prog name abs_type interpreted_term lex (prog,rule_to_cst) = *)
  let add_rule_for_cst_in_prog name duplicated abs_type interpreted_term lex {prog;rule_to_cst;cst_to_rule} =
    let interpreted_type = (interpret_type abs_type lex) in
    let eta_long_term = 
      Sg.eta_long_form 
	interpreted_term
	interpreted_type 
	lex.object_sig in
    Log.info (fun m -> m "term: %s:%s" (Sg.term_to_string interpreted_term lex.object_sig) (Sg.type_to_string interpreted_type lex.object_sig));
    Log.info (fun m -> m "eta-long form: %s" (Sg.term_to_string eta_long_term lex.object_sig));
    Log.info (fun m -> m "eta-long form (as caml term): %s" (Lambda.raw_to_caml eta_long_term));
    Log.info (fun m -> m "Datalog rule addition: lexicon \"%s\", constant \"%s:%s\" mapped to \"%s:%s\"" (fst lex.name) name (Sg.type_to_string abs_type lex.abstract_sig) (Sg.term_to_string eta_long_term lex.object_sig) (Sg.type_to_string interpreted_type lex.object_sig));
    let obj_princ_type,obj_typing_env = TypeInference.Type.inference eta_long_term in
    Log.info (fun m -> m "Interpreting \"%s\" as \"%s=%s\" with principle type: \"%s\"" name (Sg.term_to_string eta_long_term lex.object_sig) (Lambda.raw_to_caml eta_long_term) (Lambda.raw_type_to_string obj_princ_type));
    Log.info (fun m -> m "In the context of:");
    Log.info (fun m ->
        let () =
          (Utils.IntMap.iter
             (fun k (t,ty) -> Log.debug (fun m -> m "%d --> %s : %s" k (Lambda.raw_to_string t) (Lambda.raw_type_to_string ty)))
             obj_typing_env) in
        m "Done.");
    let rule,new_prog =
      Reduction.generate_and_add_rule
        ~abs_cst:(name,abs_type)
        ~obj_princ_type
      ~obj_typing_env
      prog
      ~abs_sig:lex.abstract_sig
      ~obj_sig:lex.object_sig in
    let cst_id,_ = Sg.find_term name lex.abstract_sig in
    let cst_id_as_int = 
      match cst_id with
      | Lambda.Const i -> i
      | _ -> failwith "Bug: Trying to add a rule that does not correspond to a constant" in
    let new_prog'=
      if duplicated then
	Datalog.Program.remove_rule (Utils.IntMap.find cst_id_as_int cst_to_rule) rule.DatalogAbstractSyntax.Rule.lhs.DatalogAbstractSyntax.Predicate.p_id new_prog
      else
	new_prog in
    (*    new_prog,RuleToCstMap.add rule.DatalogAbstractSyntax.Rule.id cst_id rule_to_cst *)
    {prog=new_prog';
     rule_to_cst=RuleToCstMap.add rule.DatalogAbstractSyntax.Rule.id cst_id rule_to_cst;
     cst_to_rule=Utils.IntMap.add cst_id_as_int rule.DatalogAbstractSyntax.Rule.id cst_to_rule
    }
      
      
  let insert e ({dico=d} as lex) = match e with
    | Abstract_syntax.Type (id,loc,ty) ->
       let interpreted_type = Sg.convert_type ty lex.object_sig in
       {lex with dico=Dico.add (id,Type) (Type (loc,interpreted_type)) d}
    | Abstract_syntax.Constant (id,loc,t) ->
       let abs_type=Sg.expand_type (Sg.type_of_constant id lex.abstract_sig) lex.abstract_sig in
       let interpreted_type = interpret_type abs_type lex in
       let unfold i = Sg.unfold_term_definition i lex.object_sig in	
       let interpreted_term = 
	 Lambda.normalize
	   ~id_to_term:unfold
	   (Sg.typecheck t interpreted_type lex.object_sig) in
       let prog = match lex.datalog_prog with
	 | None -> None
	 | Some p ->
	    let duplicated_entry = Dico.mem (id,Cst) d in
	    let new_prog= add_rule_for_cst_in_prog id duplicated_entry abs_type (Signature.expand_term interpreted_term lex.object_sig) lex p in
	    Some new_prog in
       {lex with
	 dico=Dico.add (id,Cst) (Constant (loc,interpreted_term)) d;
	 datalog_prog =prog}
	
  let rebuild_prog lex =
    match lex.datalog_prog with
    | None -> lex
    | Some _ ->
      let new_prog=
	Dico.fold
	  (fun (id,_) inter acc ->
	    match inter with
	    | Type (l,stype) -> acc
	    | Constant (l,t) -> 
	      add_rule_for_cst_in_prog
		id
		false (* When rebuilding, no risk of dublicated interpretations *)
		(Sg.expand_type (Sg.type_of_constant id lex.abstract_sig) lex.abstract_sig) 
		t
		lex
		acc)
	  lex.dico
	  (*	  (Datalog.Program.empty,RuleToCstMap.empty) in *)
	  {prog=Datalog.Program.empty;rule_to_cst=RuleToCstMap.empty;cst_to_rule=Utils.IntMap.empty} in
      {lex with datalog_prog=Some new_prog}
      

  let parse term dist_type lex =
    match lex.datalog_prog,Sg.expand_type dist_type lex.abstract_sig with
    | None,_ ->
       let () = Logs.warn (fun m -> m "Parsing is not implemented for non 2nd order ACG.") in
      SharedForest.SharedForest.empty
    | Some {prog}, (Lambda.Atom _ as dist_type) ->
       Log.info (fun m -> m "Before parsing. Program is currently:");
       DatalogAbstractSyntax.Program.log_content Logs.Info (Datalog.Program.to_abstract prog) ;
       Log.info (fun m -> m "That's all.");
       let dist_type_image = interpret_type dist_type lex in
       let term =
	 Lambda.normalize
 	   ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	   (Signature.expand_term term lex.object_sig) in
       Log.info (fun m -> m "Term for the query: %s" (Signature.term_to_string term lex.object_sig));
       let obj_term= 
	 Sg.eta_long_form
	   term
	   dist_type_image
	   lex.object_sig in
       let obj_princ_type,obj_typing_env = TypeInference.Type.inference obj_term in
       Log.debug (fun m -> m "Going to set a query for the distinguised type \"%s(%s)\"" (Signature.type_to_string dist_type lex.abstract_sig) (Lambda.raw_type_to_string dist_type));
       Log.debug (fun m -> m "whose image is \"%s(%s)\"" (Signature.type_to_string dist_type_image lex.object_sig) (Lambda.raw_type_to_string dist_type_image));
       Log.debug (fun m -> m "resulting int the principle type \"%s\"" (Lambda.raw_type_to_string obj_princ_type));
       let query,temp_prog =
	 Reduction.edb_and_query
	   ~obj_term
	   ~obj_type:obj_princ_type
	   ~obj_typing_env
	   ~dist_type
	   prog
	   ~abs_sig:lex.abstract_sig
	   ~obj_sig:lex.object_sig in
       Log.info (fun m -> m "Going to solve the query: \"%s\" with the program:" (DatalogAbstractSyntax.Predicate.to_string query temp_prog.Datalog.Program.pred_table temp_prog.Datalog.Program.const_table));
       DatalogAbstractSyntax.Program.log_content Logs.Debug (Datalog.Program.to_abstract temp_prog);
      let starting_parse_time = Sys.time () in
      let derived_facts,derivations = Datalog.Program.seminaive temp_prog in
      let ending_parse_time = Sys.time () in
      let parse_forest = Datalog.Program.build_forest ~query:query derivations temp_prog in
      let ending_building_forest_time = Sys.time () in
      let () = Logs.app (fun m -> m "") in
      let () = Logs.app (fun m -> m "Parsing time:             %.3f seconds" (ending_parse_time -. starting_parse_time)) in
      let () = Logs.app (fun m -> m "@[<v>Building forest time:     %.3f seconds@,@]" (ending_building_forest_time -. ending_parse_time)) in
      let resume = 
	match parse_forest with
	| [] -> SharedForest.SharedForest.empty
	| [f] -> SharedForest.SharedForest.init f
(*	| a::b::tl -> SharedForest.SharedForest.init a *)
	| _ -> failwith "Bug: not fully specified query"
      in 
      resume
    | Some _ , _ -> 
      let () = 
	Logs.warn (fun m -> m "Parsing is not yet implemented for non atomic distinguished type") in
      SharedForest.SharedForest.empty
      
    
  let get_analysis resume lex =
    Log.debug (fun m -> m "Trying to get some analysis");
    match lex.datalog_prog with
    | None -> let () = Logs.warn (fun m -> m "Parsing is not yet implemented for non atomic distinguished type") in None,resume
    | Some {rule_to_cst=rule_id_to_cst} ->
      match SharedForest.SharedForest.resumption resume with
      | None,resume -> None,resume
      | Some t,resume ->
	Log.debug (fun m -> m "Got a result. Ready to map it");
	Some (SharedForest.SharedForest.fold_depth_first
		((fun rule_id -> RuleToCstMap.find rule_id rule_id_to_cst),
		 (fun x y -> Lambda.App(x,y)))
		t),
	resume

  let is_empty = SharedForest.SharedForest.is_empty
          
  let to_string ({name=n,_;dico=d;abstract_sig=abs_sg;object_sig=obj_sg} as lex) =
    let buff=Buffer.create 80 in
    let () = Printf.bprintf
      buff
      "lexicon %s(%s): %s =\n%send"
      n
      (fst (Sg.name abs_sg))
      (fst (Sg.name obj_sg))
      (match 
	  Dico.fold
	    (fun (k,_) i -> function
	    | None -> Some (Printf.sprintf "\t%s := %s;" k (interpretation_to_string k (fun id -> interpret_type (Sg.type_of_constant id abs_sg) lex) i obj_sg))
	    | Some a -> Some (Printf.sprintf "%s\n\t%s := %s;" a k (interpretation_to_string k (fun id -> interpret_type (Sg.type_of_constant id abs_sg) lex) i obj_sg)))
	    d
	    None
	with
	| None -> ""
	| Some s -> Printf.sprintf "%s\n" s) in
    let () = Printf.bprintf buff "\n************************\n" in
    let () = match lex.datalog_prog with
      | None -> Printf.bprintf buff "This lexicon was not recognized as having a 2nd order abstract signature\n" 
      (*      | Some (p,_) -> *)
      | Some {prog=p} ->
	 let () = Printf.bprintf buff "This lexicon recognized as having a 2nd order abstract signature. The associated datalog program is:\n" in
		  Buffer.add_buffer buff (DatalogAbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract p)) in
    Buffer.contents buff

  let check ({dico=d;abstract_sig=abs} as lex) =
    let missing_interpretations =
      Signature.fold
	(fun e acc ->
	   match Sg.is_declared e abs with
	   | Some s ->
              (match Sg.entry_to_data e with
               | Sg.Type _ ->
                  if Dico.mem (s,Type) d then
                    acc
                  else
                    s::acc
               | Sg.Term _ ->
                  if Dico.mem (s,Cst) d then
                    acc
                  else
                    s::acc)
	   | None -> acc)
	[]
	abs in
      match missing_interpretations with
	| [] -> ()
	| lst -> emit_missing_inter lex lst
	    

  let rebuild_interpetation lex =
    match lex.syntax_dico with
    | None -> failwith "bug: an rebuild of a lexicon defined as composition was triggered"
    | Some syntax_dico ->
       let prog =
	 (*	 if (Sg.is_2nd_order lex.abstract_sig) && (not lex.non_linear_interpretation) then *)
	 if (Sg.is_2nd_order lex.abstract_sig)  then
	   (*	   Some (Datalog.Program.empty,RuleToCstMap.empty)  *)
	   Some {prog=Datalog.Program.empty;rule_to_cst=RuleToCstMap.empty;cst_to_rule=Utils.IntMap.empty} 
	 else
	   None in
       let new_lex =  
	 Dico.fold
	   (fun _ abs_syntax_tree lex -> insert abs_syntax_tree lex)
	   syntax_dico
	   {lex with
	     dico=Dico.empty;
	     datalog_prog=prog}in
       {new_lex with timestamp=Unix.time ()}

  let compose lex1 lex2 n =
    Log.info (fun m -> m "Compose %s(%s) as %s" (fst(name lex1)) (fst(name lex2)) (fst n));
    let temp_lex=
      {name=n;
       dico = 
	 Dico.fold
	   (fun key inter acc ->
	    match inter with
	    | Type (l,stype) -> Dico.add key (Type (l,interpret_type stype lex1)) acc
	    | Constant (l,t) -> Dico.add key (Constant (l,Lambda.normalize ~id_to_term:(fun i -> Sg.unfold_term_definition i lex1.object_sig) (interpret_term t lex1))) acc)
	   lex2.dico
	   Dico.empty;
       syntax_dico=None;
       abstract_sig = lex2.abstract_sig;
       object_sig=lex1.object_sig;
       datalog_prog=lex2.datalog_prog;
       non_linear_interpretation=(lex1.non_linear_interpretation) || (lex2.non_linear_interpretation);
       build = Compose (lex1,lex2);
       timestamp = Unix.time ()} in
    rebuild_prog temp_lex

  let program_to_buffer lex =
    let buff=Buffer.create 80 in
    let () = match lex.datalog_prog with
      | None -> Printf.bprintf buff "This lexicon was not recognized as having a 2nd order abstract signature\n" 
      (*      | Some (p,_) ->  *)
      | Some {prog=p} -> 
	let () = Printf.bprintf buff "This lexicon recognized as having a 2nd order abstract signature. The associated datalog program is:\n" in
	Buffer.add_buffer buff (DatalogAbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract p)) in
    buff

  let program_to_log src level lex = 
    match lex.datalog_prog with
    | None -> Logs.msg ~src Logs.Warning (fun m -> m "This lexicon was not recognized as having a 2nd order abstract signature" )
    | Some {prog=p} -> 
       let () = Logs.msg ~src level (fun m -> m "This lexicon is recognized as having a 2nd order abstract signature.") in
       let () = Logs.msg ~src level (fun m -> m "The associated datalog program is:") in
       DatalogAbstractSyntax.Program.log_content ~src level (Datalog.Program.to_abstract p)


  let query_to_buffer term dist_type lex =
    match lex.datalog_prog,Sg.expand_type dist_type lex.abstract_sig with
    | None,_ -> 
      let buff=Buffer.create 80 in
      let () = Printf.bprintf buff "Parsing is not implemented for non 2nd order ACG\n!" in
      buff
    | Some {prog}, (Lambda.Atom _ as dist_type) ->
      let dist_type_image = interpret_type dist_type lex in
      let obj_term= 
	Sg.eta_long_form
	  (Lambda.normalize
 	     ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	     (Signature.expand_term term lex.object_sig))
	  dist_type_image
	  lex.object_sig in
      let obj_princ_type,obj_typing_env = TypeInference.Type.inference obj_term in
      let query,temp_prog =
	Reduction.edb_and_query
	  ~obj_term
	  ~obj_type:obj_princ_type
	  ~obj_typing_env
	  ~dist_type
	  prog
	  ~abs_sig:lex.abstract_sig
	  ~obj_sig:lex.object_sig in
      let buff = Datalog.Program.edb_to_buffer temp_prog in
      let () = Printf.bprintf buff "Query:\n\t%s?\n" (DatalogAbstractSyntax.Predicate.to_string query temp_prog.Datalog.Program.pred_table temp_prog.Datalog.Program.const_table) in
      buff
    | Some _ , _ -> 
      let buff=Buffer.create 80 in
      let () = 
	Printf.bprintf buff "Parsing is not yet implemented for non atomic distinguished type\n%!" in
      buff

  let query_to_log src level term dist_type lex =
    match lex.datalog_prog,Sg.expand_type dist_type lex.abstract_sig with
    | None,_ -> Logs.msg ~src Logs.Warning (fun m -> m "Parsing is not implemented for non 2nd order ACG")
    | Some {prog}, (Lambda.Atom _ as dist_type) ->
       let dist_type_image = interpret_type dist_type lex in
       let obj_term= 
	 Sg.eta_long_form
	   (Lambda.normalize
 	      ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	      (Signature.expand_term term lex.object_sig))
	   dist_type_image
	   lex.object_sig in
       let obj_princ_type,obj_typing_env = TypeInference.Type.inference obj_term in
       let query,temp_prog =
	 Reduction.edb_and_query
	   ~obj_term
	   ~obj_type:obj_princ_type
	   ~obj_typing_env
	   ~dist_type
	   prog
	   ~abs_sig:lex.abstract_sig
	   ~obj_sig:lex.object_sig in
       let () = Datalog.Program.edb_to_log src level temp_prog in
       let () = Logs.msg ~src level (fun m -> m "Query:") in
       Logs.msg ~src level (fun m -> m "@;<4>%s?" (DatalogAbstractSyntax.Predicate.to_string query temp_prog.Datalog.Program.pred_table temp_prog.Datalog.Program.const_table)) 
    | Some _ , _ -> 
	Logs.msg ~src Logs.Warning (fun m -> m "Parsing is not yet implemented for non atomic distinguished type")

        

(*  let timestamp lex = {lex with timestamp=Unix.time ()} *)

  let update lex e = failwith "Not yet implemented"

end

module Data_Lexicon = Make (Data_Signature)
