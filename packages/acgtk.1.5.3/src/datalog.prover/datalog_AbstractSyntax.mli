open UtilsLib
open IdGenerator
  
module VarGen:IdGen_TYPE
module ConstGen:IdGen_TYPE

(** These modules are the abstract syntactic representations of
    predicates and rules *)


module AbstractSyntax :
sig
  module Predicate :
  sig
    type term = 
    | Var of VarGen.id
    | Const of ConstGen.id
	
    type pred_id
    
    module PredIdMap:Map.S with type key=pred_id
    module PredIdTable:CorrespondanceTableTYPE with type identifier=pred_id
    module PredIds : Set.S with type elt=pred_id
      
    type predicate={p_id:pred_id;
		    arity:int;
		    arguments:term list 
		 (** It is assumed that the size of the list is the
		     arity *)
		   }      
      
    val to_string : predicate-> ?position:int -> PredIdTable.table -> ConstGen.Table.table -> string
    val compare : predicate -> predicate -> int 

    val fake_pred_id : pred_id

  end



  module Proto_Rule:
  sig
    type t={proto_id:int;
	    proto_lhs:Predicate.predicate;
	    proto_rhs:Predicate.predicate list;
	   (** represents the predicates of the rule.*)
	   }
    val to_string : t -> Predicate.PredIdTable.table -> ConstGen.Table.table -> string

  end


  module Rule:
  sig

    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:(Predicate.predicate*int) list;
	       (** represents the extensionnal predicates of the
		   rule. The [int] represents the position in the rule *)
	       i_rhs:(Predicate.predicate*int) list; 
	      (** represents the intensionnal predicates of the rule.
		  The [int] represents the position in the rule *)
	       i_rhs_num:int;
	      (* stores the number of intensional predicates occurring in the
		 rule *)
	      }
    val to_string : rule -> ?with_position:bool -> Predicate.PredIdTable.table -> ConstGen.Table.table -> string
    val proto_rule_to_rule : Proto_Rule.t -> Predicate.PredIds.t -> rule
      
    module Rules : Set.S with type elt=rule
    module RuleMap : Map.S with type key=rule
  end

  module Proto_Program :
  sig
    type t=   {rules:Proto_Rule.t list;
	       pred_table: Predicate.PredIdTable.table;
	       const_table: ConstGen.Table.table;
	       i_preds:Predicate.PredIds.t;
	       rule_id_gen:IntIdGen.t;
	       pred_to_rules:Utils.IntSet.t Predicate.PredIdMap.t}

    type tables = Predicate.PredIdTable.table*(VarGen.Table.table*ConstGen.Table.table)

    val empty : t
      
    (** [extension pred_table const_table id_gen] returns an almost
       empty proto program. This almost empty proto program is meant
       to serve as extension of an actual program *)
    val extension : Predicate.PredIdTable.table -> ConstGen.Table.table -> IntIdGen.t -> t

    val add_proto_rule : ((tables -> (Predicate.predicate*tables))*(tables -> ((Predicate.predicate list)*tables))) -> t -> t

  end
    
  module Program : 
  sig 
    
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
		     
      
    val make_program : Proto_Program.t -> program
    val extend : program -> Proto_Program.t -> program
    val to_buffer : program -> Buffer.t
    val log_content : ?src:Logs.src -> Logs.level -> program -> unit
  end
end


