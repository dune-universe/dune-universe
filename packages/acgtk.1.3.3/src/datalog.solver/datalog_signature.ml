module Datalog_signature = 
  struct
    type predicate = int
    type signature = S of ((int*string) list * (int option) * (int option))
      (*list of predicate, if the kth element of the list is (n,name)
	then the kth predicate has arity n and is named 'name'.  
        Possibly a predicate is spcified to define a notion of equality.*)

    exception Undefined_predicate

    let empty = S ([],None,None)

    let make_pred (i:int) = (i:predicate)

    let get_predicate k = 
      function S (l,_,_) -> List.nth l k

    let get_name k s = let (_,name) = get_predicate k s in name
							  
    let get_arity k s = let (n,_) = get_predicate k s in n

    let find_predicates prop sign = 
      let rec find_pred l res n= 
	match l with 
	    [] -> res
	  | a::tl -> 
	      if prop(a)
	      then find_pred tl (n::res) (n+1)
	      else find_pred tl res (n+1)
      in
	match sign with
	    S (l,_,_)->
	      find_pred l [] 0
                
    let find_pred_of_name name sign = 
      let rec find_pred l id = 
        match l with 
            [] -> raise Not_found
          | (ar,name0)::l -> 
              if name=name0
              then (ar,id)
              else find_pred l (id+1)
      in
        match sign with 
            S (l,_,_) ->
              find_pred l 0

    let add_pred arity name =
      function S (l,eq,neq) -> S(l@[(arity,name)],eq,neq)

    let add_pred_get_id arity name = 
      function S (l,eq,neq) ->
        (List.length l, S(l@[(arity,name)],eq,neq))

    let get_identifier_of_name name =
      function S (l,eq,neq) ->
	let rec get_place_name l n = 
	  match l with
	      [] -> raise Undefined_predicate
	    | (_,name0)::l ->
		if (String.compare name name0)=0
		then n
		else get_place_name l (n+1)
	in
	  get_place_name l 0

    let add_pred_list arity name_list = 
      function S (l,eq,neq) ->
	S(
	  l@
	    (List.map
		(function name -> (arity,name))
		name_list
	    )
            ,
          eq
            ,
          neq
	)

    let fresh = 
      function S (l,_,_) -> List.length l

    let compare_predicate p1 p2 = compare p1 p2

    let add_eq_get_id =
      function S(l,eq,neq) as sign->
        match eq with
            Some(n) -> (n, sign)
          | None ->
              let n = List.length l in
                (n, S(l@[(2,"=")], Some n,neq))

    let add_eq =
      function S(l,eq,neq) as sign ->
        match eq with
            Some(n) -> sign
          | None ->
             let n =  List.length l in
               S(l@[(2,"=")], Some n,neq)

    let add_neq_get_id =
      function S(l,eq,neq) as sign->
        match neq with
            Some(n) -> (n, sign)
          | None ->
              let n = List.length l in
                (n, S(l@[(2,"~=")],eq,Some(n)))

    let add_neq =
      function S(l,eq,neq) as sign ->
        match neq with
            Some(n) -> sign
          | None ->
             let n =  List.length l in
               S(l@[(2,"~=")], eq, Some n)
  end
