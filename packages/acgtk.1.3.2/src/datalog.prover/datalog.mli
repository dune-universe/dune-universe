open UtilsLib

module ASPred:module type of Datalog_AbstractSyntax.AbstractSyntax.Predicate 
  with type pred_id=Datalog_AbstractSyntax.AbstractSyntax.Predicate.pred_id 
  and type PredIdTable.table = Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table

module ASRule:module type of Datalog_AbstractSyntax.AbstractSyntax.Rule
  with type rule=Datalog_AbstractSyntax.AbstractSyntax.Rule.rule

module ASProg:module type of Datalog_AbstractSyntax.AbstractSyntax.Program
  with type program = Datalog_AbstractSyntax.AbstractSyntax.Program.program


module type Datalog_Sig=
sig
  exception Fails
  module UF:UnionFind.S
    
  module Predicate :
  sig
    type predicate = { p_id : ASPred.pred_id; arity : int; }
    val make_predicate : Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate -> predicate
    module PredMap : Map.S with type key = ASPred.pred_id
    module FactSet :Set.S with type elt = ASPred.predicate
    val conditionnal_add :
      FactSet.elt -> FactSet.t -> FactSet.t -> FactSet.t -> FactSet.t
    val facts_to_string : FactSet.t PredMap.t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> string
    module PredicateMap : Map.S with type key = ASPred.predicate
    module Premise :
    sig
      type t = ASPred.predicate list * int * int (* the first int parameter is meant to be the rule id and the second one to be the number of intensional predicates occurring in it*)
      val to_string : t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> string
    end
    module PremiseSet : Set.S with type elt = Premise.t
    val add_map_to_premises_to_buffer : Buffer.t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> PremiseSet.t PredicateMap.t -> unit
    val format_derivations2 : ?query:Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> PremiseSet.t PredicateMap.t -> unit




    val add_pred_arguments_to_content :
      ASPred.term list ->
      Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
        int Datalog_AbstractSyntax.VarGen.IdMap.t ->
      Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
        int Datalog_AbstractSyntax.VarGen.IdMap.t
	
  end
    
  module Rule :
  sig
    type rule = {
      id : int;
      lhs : Predicate.predicate;
      e_rhs : (Predicate.predicate*int) list;
      i_rhs : (Predicate.predicate*int) list;
      i_rhs_num:int;
      content : Datalog_AbstractSyntax.ConstGen.id UF.t;
    }
    val make_rule : ASRule.rule -> rule
    val cyclic_unify : int -> int -> 'a UF.t -> 'a UF.t
    val extract_consequence :
      rule -> Datalog_AbstractSyntax.ConstGen.id UF.t -> ASPred.predicate
    module FactArray :
    sig
      type row = Predicate.FactSet.t
      type array = row list
      val collect_results :
        ('a -> (int * Datalog_AbstractSyntax.ConstGen.id UF.t) * Predicate.FactSet.elt list -> 'a) ->
        'a ->
        (int * Datalog_AbstractSyntax.ConstGen.id UF.t) * Predicate.FactSet.elt list -> array -> 'a
    end
    val immediate_consequence_of_rule :
      rule -> FactArray.row Predicate.PredMap.t -> ASPred.predicate list
      
    module Rules:Set.S with type elt=rule
  end
    
  module Program :
  sig
    type program = {
      (*      rules : Rule.rule list Predicate.PredMap.t; *)
      rules : Rule.Rules.t Predicate.PredMap.t;
      edb : ASPred.pred_id list;
      edb_facts:Predicate.FactSet.t Predicate.PredMap.t;
      idb : ASPred.pred_id list;
      pred_table: ASPred.PredIdTable.table;
      const_table: Datalog_AbstractSyntax.ConstGen.Table.table;
      rule_id_gen:IdGenerator.IntIdGen.t;
    }
    val empty : program
    val make_program : ASProg.program -> program
    val temp_facts :
      Rule.rule ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      (ASPred.predicate * Predicate.FactSet.elt list -> Rule.rule -> 'a -> 'a) -> 'a -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> 'a
    val p_semantics_for_predicate :
      Predicate.PredMap.key ->
      program ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t -> Predicate.PremiseSet.t Predicate.PredicateMap.t -> Predicate.FactSet.t * Predicate.PremiseSet.t Predicate.PredicateMap.t 
    val seminaive : program -> Rule.FactArray.row Predicate.PredMap.t * Predicate.PremiseSet.t Predicate.PredicateMap.t
    val to_abstract : program -> ASProg.program
      
    val extend : program -> ASProg.modifier -> program
      
    val add_e_facts : program -> (ASRule.rule list*Datalog_AbstractSyntax.ConstGen.Table.table*IdGenerator.IntIdGen.t) -> program
      
    (** [add_rule i r p] adds a [ASRule.rule] to a [Datalog.Program]
	with the assumption that it will not change the {em nature} of
	any predicate (that is making it change from extensional to
	intensional). If [i] is set to true, then the rule concerns an
	intensional predicate. If it is set to [false] then it
	concerns an extensional predicate and the rhs of the rule
	should be empty.*)
      
    val add_rule : intensional:bool -> ASRule.rule -> program -> program

    (** [remove_rule id p] returns the program [p] from which the rule
	with id [id] has been removed.
	
	IMPORTANT: This function only deals with rules introducing
	intensional predicate, because it is used when a constant is
	given several interpretations in a lexicon.
	
    *)
    val remove_rule : int -> ASPred.pred_id -> program -> program

      
    val get_fresh_rule_id : program -> (int * program)
    val get_fresh_cst_id : string -> program -> (Datalog_AbstractSyntax.ConstGen.id * program)
    val add_pred_sym : string -> program -> (ASPred.pred_id*program)

    val build_forest : ?query:Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate -> Predicate.PremiseSet.t Predicate.PredicateMap.t -> program -> int SharedForest.SharedForest.tree list list

    val edb_to_buffer : program -> Buffer.t

  end
end
  
  

module Make :
  functor (S : UnionFind.Store) -> Datalog_Sig

module Datalog:Datalog_Sig
