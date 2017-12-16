open Datalog_signature
open Int_set

module Program =
  struct

    module Signature1 = Datalog_signature

    type predicate = Pred of (Signature1.predicate*(int list))
      (*
	a predicate is constituted with:
	the identifier it has in the signature
	the list of its variables (identified with integers)
      *)
    type clause = Cl of (predicate*predicate list)
      (*
	a clause is a pair of:
	a predicate (its left hand side)
	a list of predicates (its right had side)
      *)
    type program = Prog of (Signature1.signature*clause list)
      (*
	a program is constituted with:
	the signature in which predicates are declared
	the list of its clauses
      *)

    let empty = Prog (Signature1.empty, [])

    let make_pred p l = Pred(p,l)

    let make_clause p l = Cl(p,l)

    let make_program s l = Prog(s,l)

    let get_signature =  function Prog (sign,_) -> sign

    let get_clauses = function Prog (_,cls) -> cls

    let get_predicate p prog =
      let sign = get_signature prog in
	Signature1.get_predicate p sign

    let get_predicate_name p prog= 
      let sign = get_signature prog in
	Signature1.get_name p sign
	  
    let get_predicate_arity p prog =
      let sign = get_signature prog in
	Signature1.get_arity p sign

    let get_identifier_of_name name prog = 
      let sign = get_signature prog in
	Signature1.get_identifier_of_name name sign

    let is_p_predicate_of_clause p =
      function Cl (Pred(p1,_),_) -> p1=p

    let get_clauses_defining_predicate_p p prog=
      let cls = get_clauses prog in
	List.filter (is_p_predicate_of_clause p) cls

    let add_pred_var_to_set =
      function Pred(p,vars) ->
	function var_set ->
	List.fold_left
	  (function s ->
	    function v ->
	      Int_set.add v s
	  )
	  var_set
	  vars

    let add_some_var_to_set prop=
      function Pred(p,vars) ->
	function var_set ->
	List.fold_left
	  (function s ->
	    function v ->
              if prop v
	      then Int_set.add v s
              else s
          )
          var_set
          vars

    let transform_prog 
      (init_prog_context:Signature1.signature -> 'prog_context)
      (update_prog_context:'prog_context -> clause -> 'prog_context)
      (extract_result: 'prog_context -> 'result)
        =
      function program ->
        (match program with
            Prog(sign,clauses) ->
              let prog_context = init_prog_context sign in
              let prog_context = 
                List.fold_left
                  update_prog_context
                  prog_context
                  clauses 
              in
                extract_result prog_context
        )

    let transform_clause_from_left
        init_clause_context
        update_clause_context
        extract_result = 
      function prog_context ->
        function clause ->
          (match clause with
              Cl(rhs,lhs) ->
                let clause_context = 
                  init_clause_context prog_context rhs
                in
                let clause_context = 
                  List.fold_left
                    update_clause_context
                    clause_context
                    lhs
                in
                  extract_result clause_context
                    
          )

    let transform_clause_from_right
        init_clause_context
        update_clause_context
        extract_result = 
      function prog_context ->
        function clause ->
          (match clause with
              Cl(rhs,lhs) ->
                let clause_context = 
                  init_clause_context prog_context rhs
                in
                let clause_context = 
                  List.fold_right
                    (
                      function clause ->
                        function clause_context->
                          update_clause_context clause_context clause
                    )
                    lhs
                    clause_context
                in
                  extract_result clause_context
                    
          )
          


    let transform_left
        init_prog_context 
        extract_result 
        init_clause_context
        update_clause_context
        extract_prog_context
        program = 
      transform_prog
        init_prog_context
        (transform_clause_from_left
            init_clause_context
            update_clause_context
            extract_prog_context
        )
        extract_result
        program
    
    let transform_right
        init_prog_context 
        extract_result 
        init_clause_context
        update_clause_context
        extract_prog_context
        program = 
      transform_prog
        init_prog_context
        (transform_clause_from_right
            init_clause_context
            update_clause_context
            extract_prog_context
        )
        extract_result
        program
    
end
