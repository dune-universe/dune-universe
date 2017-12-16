open Datalog_signature
open Program
open Int_set

module Adornment =
struct 
  type adornment = Ad of Datalog_signature.predicate*((int*bool) list)
  type adorned_predicate = AdP of Datalog_signature.predicate*(bool list)
  type adorned_clause = ACl of adornment*(adornment list)

  exception No_order_found
  exception No_solution_for_initial_adornment

  module Ordered_adorned_predicates =
  struct
    type t=adorned_predicate
    let compare = 
      function AdP (p1,l1)->
	function AdP (p2,l2)->
	  if(p1=p2)
	  then
	    let rec comp_list l1 l2 =
	      (match (l1,l2) with
		  ([],[]) -> 0
		| (true::ll1,false::ll2)  -> 1
		| (true::ll1,true::ll2)   -> comp_list ll1 ll2
		| (false::ll1,true::ll2)  -> -1
		| (false::ll1,false::ll2) -> comp_list ll1 ll2
		|_ -> failwith "comp_list: lists do not have the same length"
	      )
	    in
	      comp_list l1 l2
	  else 
	    (Datalog_signature.compare_predicate p1 p2) 
	    
  end

  module Adorned_predicate_set=Set.Make (Ordered_adorned_predicates)
    
  type adorned_program = AProg of 
    Datalog_signature.signature*Adorned_predicate_set.t*(adorned_clause list)
      
  type prog_context = 
      Prog_context of
        bool*
          (Program.program)*
          (Adorned_predicate_set.t) *
          (Adorned_predicate_set.t) * 
          (adorned_predicate -> bool) *
          (Adorned_predicate_set.t) *
          (adornment -> adornment ->int)

  type clause_context = 
      Cl_context of 
        bool*
          Int_set.t*
          (Program.predicate list) *
          Adorned_predicate_set.t * 
          (adorned_predicate -> bool) *
          Adorned_predicate_set.t *
          (adornment -> adornment -> int)

  type prog_context_snd_pass = 
      Prog_context_snd_pass of
      (Program.program)*
        (Adorned_predicate_set.t) *
        (adorned_predicate -> bool) *
        (Adorned_predicate_set.t) *
        (adornment -> adornment ->int)*
        adorned_clause list
        

  type clause_context_snd_pass = 
      Cl_context_snd_pass of
        adornment *
          Int_set.t*
          (Program.predicate list) *
          adornment list *
           Adorned_predicate_set.t * 
          (adorned_predicate -> bool) *
          Adorned_predicate_set.t *
          (adornment -> adornment -> int)

  let string_of_adornment sign= 
    function AdP (k,l) ->
      let trans b= if b then "b" else "f" in
      let suffix = String.concat "" (List.map (trans) l) in
        String.concat "_" [(Datalog_signature.get_name k sign);suffix]

  let predicate_of_adornment sign = 
    function AdP(k,l) ->
      let (arity,name) = Datalog_signature.get_predicate k sign in
      let trans b= if b then "b" else "f" in
      let suffix = String.concat "" (List.map (trans) l) in
        (arity,name^"_"^suffix)

  let get_max_elt compare l =
    let rec get_max l res =
      match l with
          [] -> res
        | a::tl ->
            if (compare res a) >=0
            then get_max tl res
            else get_max tl a
    in
      match l with 
          [] -> failwith "get_max_elt: empty list"
        | a::tl -> get_max tl a


  let add_some_var_to_set prop =
    function Ad(p,ad_var) ->
      function var_set ->
        List.fold_left
          (function s ->
            function (n,b) ->
              if prop (n,b)
              then
                Int_set.add n s
              else s
          )
          var_set
          ad_var


  let add_bound_var_to_set = add_some_var_to_set (function (_,b) -> b)

  let add_free_var_to_set = add_some_var_to_set (function (_,b) -> not b)

  let add_all_var_to_set = add_some_var_to_set (function _ -> true)

  let is_lower adp1 adp2 =
    (match (adp1,adp2) with
        (AdP(k,ad1),AdP(l,ad2))->
          (l=k) &&
            (List.fold_left2
                (function b_res ->
                  function b1 ->
                    function b2 ->
                      b_res && ((not b1) || b2) 
                )
                true 
                ad1
                ad2
            )
    )       

  let has_lower ad ad_set = 
    Adorned_predicate_set.exists (function ad_cl -> is_lower ad_cl ad) ad_set

  let has_higher ad ad_set = 
    Adorned_predicate_set.exists (function ad_cl -> is_lower ad ad_cl) ad_set

  let from_predicate_to_adornment =
    function Program.Pred(p,vars)->
      function bound_variables ->
        let ad_vars =
          List.map
            (function v ->
              (v,Int_set.mem v bound_variables)
            ) 
            vars 
        in 
          Ad(p,ad_vars)

  let to_adorned_predicate = 
    function Ad(p,ad_vars) ->
      let (_,adornments) = List.split ad_vars in
        AdP (p,adornments)

  let to_pred =
    function Ad(p,ad_vars) ->
      let (vars,_) =List.split ad_vars in
      Program.Pred(p,vars)

  let ad_pred_to_string = 
    function AdP(k,adornment) ->
      let rec print_adornment res adornment =
        match adornment with
            [] -> res
          | true::adornment -> print_adornment (res^"b") adornment
          | false::adornment -> print_adornment (res^"f") adornment
      in
        (print_adornment ("AdP("^(string_of_int k)^",") adornment)^")"


  let init_prog_context program initial_interesting_adornments is_permissible compare_adornments = 
      Prog_context
      (true,
      program,
      initial_interesting_adornments,
      initial_interesting_adornments,
      is_permissible,
      Adorned_predicate_set.empty,
      compare_adornments)
          
  let init_clause_context prog_context clause ad_pred= 
    let Prog_context
        (has_changed,
        program,
        initial_interesting_adornments,
        interesting_adornments,
        is_impermissible,
        impermissible_adornment,
        compare_adornments) = prog_context 
    in
    let AdP(_,adornment) = ad_pred in
    let Program.Cl(Program.Pred(k,vars),lhs) = clause in
    let ad = Ad(k,List.combine vars adornment) in
    let bound_vars = add_bound_var_to_set ad Int_set.empty in
      Cl_context
        (has_changed,
        bound_vars,
        lhs,
        interesting_adornments,
        is_impermissible,
        impermissible_adornment,
        compare_adornments)
      

  let is_permissible is_impermissible impermissible_adornments ad_pred = 
    not((is_impermissible ad_pred) 
         || 
         (has_higher(ad_pred) impermissible_adornments))
      
  let is_permissible_in_clause_context clause_context ad = 
    let Cl_context(_,_,_,_,is_impermissible,impermissible_adornments,_) =
      clause_context
    in
    let ad_pred = to_adorned_predicate ad in
      is_permissible is_impermissible impermissible_adornments ad_pred


  let is_permissible_in_prog_context prog_context ad_pred = 
    let Prog_context(_,_,_,_,is_impermissible,impermissible_adornments,_) 
        = prog_context 
    in
      is_permissible is_impermissible impermissible_adornments ad_pred

  let add_to_interesting_adorned_predicate clause_context ad = 
    let Cl_context(
      has_changed,
      bound_vars,
      lhs,
      interesting_adornments,
      is_impermissible,
      impermissible_adornment,
      compare_adornments) 
        = 
      clause_context 
    in
    let ad_pred = to_adorned_predicate ad in
      (if has_changed
      then 
        Cl_context(
          has_changed,
          bound_vars,
          lhs,
          (Adorned_predicate_set.add ad_pred interesting_adornments),
          is_impermissible,
          impermissible_adornment,
          compare_adornments)
        else
          (if Adorned_predicate_set.mem ad_pred interesting_adornments 
          then clause_context
            else 
              Cl_context(
                true,
                bound_vars,
                lhs,
                (Adorned_predicate_set.add ad_pred interesting_adornments),
                is_impermissible,
                impermissible_adornment,
                compare_adornments)
          )
      )

  let is_finished_clause_context clause_context = 
    match clause_context with
        Cl_context(_,_,[],_,_,_,_) -> true
      |_ -> false
         
  let update_clause_context clause_context = 
    let Cl_context(
      has_changed,
      bound_vars,
      lhs,
      interesting_adornments,
      is_impermissible,
      impermissible_adornment,
      compare_adornments) 
        = 
      clause_context 
    in
    let adorned_lhs =
      List.map 
        (function pred -> from_predicate_to_adornment pred bound_vars)
        lhs
    in
    let permissible_adorned_predicates = 
      List.filter 
        (function ad -> is_permissible_in_clause_context clause_context ad)
        adorned_lhs
    in
      (if permissible_adorned_predicates =[]
      then raise No_order_found
        else 
          let new_ad = 
            get_max_elt 
              compare_adornments 
              permissible_adorned_predicates 
          in
          let new_pred = to_pred new_ad in
          let lhs =List.filter (function pred -> not(pred=new_pred)) lhs in
          let bound_vars = add_all_var_to_set new_ad bound_vars in
          let clause_context = 
            Cl_context(
              has_changed,
              bound_vars,
              lhs,
              interesting_adornments,
              is_impermissible,
              impermissible_adornment,
              compare_adornments) 
          in
            add_to_interesting_adorned_predicate clause_context new_ad
      )
        
  let extract_clause_context clause_context prog_context =
    let Cl_context(has_changed,_,_,interesting_adornments,_,_,_)
        = 
      clause_context 
    in
    let 
        Prog_context(_,program,initial_interesting_adornments,_,
                    is_impermissible,impermissible_adornment,compare_adornments) 
        = prog_context 
    in
      Prog_context
        (has_changed,
        program,
        initial_interesting_adornments,
        interesting_adornments,
        is_impermissible,
        impermissible_adornment,
        compare_adornments)

  let rec process_clause_context clause_context prog_context = 
    if(is_finished_clause_context clause_context)
    then extract_clause_context clause_context prog_context
    else 
      process_clause_context 
        (update_clause_context clause_context) prog_context

  let process_clause ad_pred prog_context clause = 
    let clause_context = init_clause_context prog_context clause ad_pred in
      process_clause_context clause_context prog_context

 let update_prog_context ad_pred prog_context = 
    let Prog_context
            (has_changed,
            program,
            initial_interesting_adornments,
            interesting_adornments,
            is_impermissible,
            impermissible_adornments,
            compare_adornments)
        =
      prog_context
    in
      if(not (is_permissible_in_prog_context prog_context ad_pred))
      then prog_context
      else 
        let clauses = 
          (match ad_pred with
              AdP(p,_)-> Program.get_clauses_defining_predicate_p p program
          )
        in
          (try
              List.fold_left
                (process_clause ad_pred)
                prog_context
                clauses
            with No_order_found -> 
	      (if Adorned_predicate_set.mem ad_pred initial_interesting_adornments
	      then
		raise No_solution_for_initial_adornment
		else
		 Prog_context (true, 
                              program,
                              initial_interesting_adornments,
		              Adorned_predicate_set.remove ad_pred interesting_adornments, 
                              is_impermissible,
		              Adorned_predicate_set.add ad_pred impermissible_adornments,
                              compare_adornments)
	      )
          )

let reset_prog_context prog_context = 
    let Prog_context
            (_,
            program,
            initial_interesting_adornments,
            interesting_adornments,
            is_impermissible,
            impermissible_adornments,
            compare_adornments) = prog_context in
      Prog_context
            (false,
            program,
            initial_interesting_adornments,
            interesting_adornments,
            is_impermissible,
            impermissible_adornments,
            compare_adornments)

  let rec process_prog_context prog_context = 
    match prog_context with
        Prog_context(false,_,_,interesting_adornments,_,impermissible_adornments,_) ->
          prog_context
      | Prog_context(_,_,_,interesting_adornments,_,_,_) ->
            let prog_context = 
             Adorned_predicate_set.fold
               update_prog_context
               interesting_adornments
               (reset_prog_context prog_context)
              
           in
             process_prog_context prog_context

  let from_fst_pass_to_snd_pass prog_context = 
    let Prog_context
            (_,
            program,
            _,
            interesting_adornments,
            is_impermissible,
            impermissible_adornments,
            compare_adornments) = prog_context 
    in
    let interesting_adornments = 
      Adorned_predicate_set.filter
        (function ad_pred ->
          Adorned_predicate_set.for_all
            (function ad_pred0 ->
              (not(is_lower ad_pred0 ad_pred)) || (ad_pred=ad_pred0)
            )
            interesting_adornments
        )
        interesting_adornments
    in
      Prog_context_snd_pass
        (program,interesting_adornments,is_impermissible,
        impermissible_adornments,compare_adornments,[])

  let extract_snd_pass_prog_context prog_context= 
    let Prog_context_snd_pass(
      Program.Prog(sign,_),interesting_adornments,_,_,_,ad_clauses) = 
      prog_context 
    in
      AProg(sign,interesting_adornments,List.rev(ad_clauses))

  let init_snd_pass_clause_context prog_context clause ad_pred= 
    let Prog_context_snd_pass(
      _,
      interesting_adornments,
      is_impermissible,
      impermissible_adornments,
      compare_adornments,
      new_clauses) =
      prog_context
    in
    let AdP(_,adornment) = ad_pred in
    let Program.Cl(Program.Pred(k,vars),lhs) = clause in
    let ad = Ad(k,List.combine vars adornment) in
    let bound_vars = add_bound_var_to_set ad Int_set.empty in
      Cl_context_snd_pass(
        ad,
        bound_vars,
        lhs,
        [],
        interesting_adornments,
        is_impermissible,
        impermissible_adornments,
        compare_adornments
      )

  let is_permissible_in_snd_pass_clause_context clause_context ad = 
    let Cl_context_snd_pass(_,_,_,_,_,is_impermissible,impermissible_adornments,_) 
        =clause_context
    in
    let ad_pred = to_adorned_predicate ad in
      is_permissible is_impermissible impermissible_adornments ad_pred

  let interesting_version_of_adornment interesting_adornments ad =
    let ad_pred = to_adorned_predicate ad in
    let lowers = 
      Adorned_predicate_set.filter
        (function ad_pred0 -> is_lower ad_pred0 ad_pred) 
        interesting_adornments
    in
    let AdP(_,adornment) = 
      try 
      Adorned_predicate_set.min_elt lowers 
      with Not_found -> failwith "unknown interesting adornments"
    in
    let Ad(k,ad_vars) = ad in
    let (vars,_) = List.split ad_vars in
      Ad(k,List.combine vars adornment)

  let update_snd_pass_clause_context clause_context = 
    let Cl_context_snd_pass(
      ad,
      bound_vars,
      lhs,
      new_lhs,
      interesting_adornments,
      is_impermissible,
      impermissible_adornment,
      compare_adornments) 
        = 
      clause_context 
    in
    let adorned_lhs =
      List.map 
        (function pred -> from_predicate_to_adornment pred bound_vars)
        lhs
    in
    let permissible_adorned_predicates = 
      List.filter 
        (function ad -> is_permissible_in_snd_pass_clause_context clause_context ad)
        adorned_lhs
    in
    let new_ad = 
      get_max_elt 
        compare_adornments 
        permissible_adorned_predicates 
    in
    let new_pred = to_pred new_ad in
    let lhs =List.filter (function pred -> not(pred=new_pred)) lhs in
    let bound_vars = add_all_var_to_set new_ad bound_vars in
    let new_ad = interesting_version_of_adornment interesting_adornments new_ad in
      Cl_context_snd_pass(
        ad,
        bound_vars,
        lhs,
        (new_ad::new_lhs),
        interesting_adornments,
        is_impermissible,
        impermissible_adornment,
        compare_adornments) 
        
  let is_finished_sbd_pass_clause_context clause_context = 
    let Cl_context_snd_pass(_,_,lhs,_,_,_,_,_) = clause_context in
      lhs = []


  let add_clause_to_snd_pass_prog_context prog_context ad_clause = 
    let Prog_context_snd_pass(
      program,
      interesting_adornments,
      is_impermissible,
      impermissible_adornments,
      compare_adornments,
      new_clauses) = 
      prog_context
    in
      Prog_context_snd_pass(
        program,
        interesting_adornments,
        is_impermissible,
        impermissible_adornments,
        compare_adornments,
        (ad_clause::new_clauses)
      )

  let extract_result_snd_pass_clause_context clause_context prog_context =
    let Cl_context_snd_pass(ad,_,_,new_lhs,_,_,_,_) = clause_context in
    let ad_clause = ACl(ad,List.rev new_lhs) in
      add_clause_to_snd_pass_prog_context prog_context ad_clause

  let rec process_snd_pass_clause_context clause_context prog_context = 
    if(is_finished_sbd_pass_clause_context clause_context)
    then extract_result_snd_pass_clause_context clause_context prog_context
    else 
      let clause_context = 
        update_snd_pass_clause_context clause_context
      in
        process_snd_pass_clause_context clause_context prog_context

  let process_clause_snd_pass prog_context clause = 
    let Program.Cl(Program.Pred(k,_),_) = clause in
    let Prog_context_snd_pass(_,interesting_adornments,_,_,_,_) = prog_context in
    let adorned_predicates = 
      Adorned_predicate_set.filter
        (function AdP(l,_) -> l=k)
        interesting_adornments
    in
      Adorned_predicate_set.fold
        (function ad_pred ->
          function prog_context ->
            let clause_context = init_snd_pass_clause_context prog_context clause ad_pred in
              process_snd_pass_clause_context clause_context prog_context
        )
        adorned_predicates
        prog_context

  let adorn_program program ad_pred is_impermissible compare_adornments =
    let initial_interesting_adornments = Adorned_predicate_set.singleton ad_pred in
    let prog_context = init_prog_context program initial_interesting_adornments is_impermissible compare_adornments in
    let prog_context = process_prog_context prog_context in  
    let prog_context_snd_pass = from_fst_pass_to_snd_pass prog_context in
    let Program.Prog(_,clauses) =program in
    let prog_context_snd_pass = 
      List.fold_left
        process_clause_snd_pass
        prog_context_snd_pass
        clauses
    in
      extract_snd_pass_prog_context prog_context_snd_pass

        
  let program_of_adorned_program ad_prog = 
    let AProg(sign,interesting_adornments,clauses) = ad_prog in
    let ad_list = Adorned_predicate_set.elements interesting_adornments in
    let ad_sign =  List.map (predicate_of_adornment sign) ad_list in
    let convert_adornment ad =
      let ad_pred = to_adorned_predicate ad in
      let rec get_pos_rec n l = 
        match l with 
            [] -> failwith "get_position: not found"
          | ad_pred0::l -> 
              if ad_pred=ad_pred0
              then n
              else get_pos_rec (n+1) l
      in
      let k = get_pos_rec 0 ad_list in
      let Ad(_,adornment) = ad in
      let (vars,_) = List.split adornment in
        Program.Pred(k,vars)
    in
      match sign with
          Datalog_signature.S(_,eq,neq) ->
            Program.Prog
              (Datalog_signature.S (ad_sign,eq,neq),
              List.map
                (function ACl(rhs,lhs) ->
                  Program.Cl(
                    convert_adornment rhs,
                    List.map convert_adornment lhs
                  )
                )
                clauses
              )

  let compare_adornments ad1 ad2=
    match (ad1,ad2) with
	(Ad(k1,ad_vars1),Ad(k2,ad_vars2)) ->
          let count_bound_free_vars = 
	    List.fold_left
	      (function (free, bound) ->
		function (v,bool) ->
		  if bool
		  then (free,bound+1)
		  else (free+1,bound))
	      (0,0)
	  in
	  let (free1,bound1) = count_bound_free_vars ad_vars1 in
	  let (free2,bound2) = count_bound_free_vars ad_vars2 in
	  let compare (free1,bound1,free2,bound2) = 
	    (if(free1<free2)
	    then 1
	      else(
		if(free1>free2)
		then -1
		else
		  let com1 = 
		    Int_set.cardinal 
		      (Int_set.inter
			  (add_all_var_to_set ad1 Int_set.empty)
			  (add_free_var_to_set ad2 Int_set.empty)
		      )
		  in
		  let com2 =
		    Int_set.cardinal
		      (Int_set.inter
			  (add_all_var_to_set ad2 Int_set.empty)
			  (add_free_var_to_set ad1 Int_set.empty)
		      )
		  in
		    (if (com1 >com2)
		    then 1
		      else
			(if (com1<com2)
			then -1
			  else
			    (if (bound1>bound2)
			    then 1
			      else 
				(if (bound1<bound2)
				then -1
				  else 0
				)
			    )
			)
		    )
	      )
	    )
	  in
            (match (bound1,bound2) with
		(0,0) -> 
		  compare(free1,bound1,free2,bound2)
	      | (_,0) -> 1
	      | (0,_) -> -1
	      | (_,_) -> compare(free1,bound1,free2,bound2)
            )           

  let compare_adornments2 ad1 ad2=
    match (ad1,ad2) with
	(Ad(k1,ad_vars1),Ad(k2,ad_vars2)) ->
	  let count_bound_free_vars = 
	    List.fold_left
	      (function (free, bound,fb,parity,last) ->
		function (v,bool) ->
                  let (free, bound)=
		    if bool
		    then (free,bound+1)
		    else (free+1,bound) 
                  in
                  let fb = 
                    if parity&&(not last)&&bool
                    then fb+1
                    else fb
                  in
                    (free,bound,fb,not parity,bool)
              )
	      (0,0,0,false,true)
	  in
	  let (free1,bound1,fb1,_,_) = count_bound_free_vars ad_vars1 in
	  let (free2,bound2,fb2,_,_) = count_bound_free_vars ad_vars2 in
	  let compare (free1,bound1,free2,bound2) = 
	    (if(free1<free2)
	    then 1
	      else(
		if(free1>free2)
		then -1
		else
		  let com1 = 
		    Int_set.cardinal 
		      (Int_set.inter
			  (add_all_var_to_set ad1 Int_set.empty)
			  (add_free_var_to_set ad2 Int_set.empty)
		      )
		  in
		  let com2 =
		    Int_set.cardinal
		      (Int_set.inter
			  (add_all_var_to_set ad2 Int_set.empty)
			  (add_free_var_to_set ad1 Int_set.empty)
		      )
		  in
		    (if (com1 >com2)
		    then 1
		      else
			(if (com1<com2)
			then -1
			  else
			    (if (bound1>bound2)
			    then 1
			      else 
				(if (bound1<bound2)
				then -1
				  else 0
				)
			    )
			)
		    )
	      )
	    )
	  in
            if fb1<fb2
            then 1
            else 
              if fb1>fb2
              then -1
            else
	      (match (bound1,bound2) with
		  (0,0) -> 
		    compare(free1,bound1,free2,bound2)
	        | (_,0) -> 1
	        | (0,_) -> -1
	        | (_,_) -> compare(free1,bound1,free2,bound2)
	      )


  let compare_adornments3 neq ad1 ad2=
    match (ad1,ad2) with
	(Ad(k1,ad_vars1),Ad(k2,ad_vars2)) ->
          (if Some(k1)=neq 
          then 
            (match ad_vars1 with
                [(_,true);(_,true)] -> 1
              |_ -> -1
            )
            else 
              let (fst1,_) = List.hd ad_vars1 in
              let (lst1,_) = List.hd (List.rev ad_vars1) in
              let (fst2,_) = List.hd ad_vars2 in
              let (lst2,_) = List.hd (List.rev ad_vars2) in
	      let count_bound_free_vars = 
	        List.fold_left
	          (function (free, bound) ->
		    function (v,bool) ->
		      if bool
		      then (free,bound+1)
		      else (free+1,bound))
	          (0,0)
	      in
	      let (free1,bound1) = count_bound_free_vars ad_vars1 in
	      let (free2,bound2) = count_bound_free_vars ad_vars2 in
	      let compare (free1,bound1,free2,bound2) = 
	        (if(free1<free2)
	        then 1
	          else(
		    if(free1>free2)
		    then -1
		    else
		      let com1 = 
		        Int_set.cardinal 
		          (Int_set.inter
			      (add_all_var_to_set ad1 Int_set.empty)
			      (add_free_var_to_set ad2 Int_set.empty)
		          )
		      in
		      let com2 =
		        Int_set.cardinal
		          (Int_set.inter
			      (add_all_var_to_set ad2 Int_set.empty)
			      (add_free_var_to_set ad1 Int_set.empty)
		          )
		      in
		        (if (com1 >com2)
		        then 1
		          else
			    (if (com1<com2)
			    then -1
			      else
			        (if (bound1>bound2)
			        then 1
			          else 
				    (if (bound1<bound2)
				    then -1
				      else 0
				    )
			        )
			    )
		        )
	          )
	        )
	      in
                (if lst1<lst2
                then 1
                  else 
                    (if lst2<lst1
                    then -1
                      else
                        (if fst1<fst2
                        then 1
                          else
                            (if fst2<fst1
                            then -1
                              else
	                        (match (bound1,bound2) with
		                    (0,0) -> 
		                      compare(free1,bound1,free2,bound2)
	                          | (_,0) -> 1
	                          | (0,_) -> -1
	                          | (_,_) -> compare(free1,bound1,free2,bound2)
                                )
                            )
                        )
                    )
                )
          )
end   
