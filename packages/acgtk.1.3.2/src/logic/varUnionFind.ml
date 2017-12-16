open UtilsLib.Utils


(** Modules with this module type should provide Union-Find algorithms
    and the indexed storage data structure. Note that we take the
    opportunity of implementing from scratch such algorithms to allow
    the [find] function returns not only the index of the
    representative and the values it indexes, but also the storage
    data structure, so that the [find] algorithm can modify it, in
    particular with path compression.
*)

module UF (Value :
  sig
    type t
    type value
    val unfold : value -> t -> (int*value list) option
(*    val fold : (int*value list) -> t -> value *)
  end) = 
struct
  module Store =
  struct
    type 'a t = 'a IntMap.t
    exception Store_Not_found
    let empty _ = IntMap.empty
    let get k m = 
      try
	IntMap.find k m
      with
      | Not_found -> raise Store_Not_found
    let set k v m = IntMap.add k v m
    let copy m=m
    let iter = IntMap.iter
  end
    
    
  (** The type of the values (content) that are indexed. It is either
      an actual value of type ['a] or a link to another indexed
      value. If a content at an index [i] points to [i], it is meant
      that to be a variable.*)
  type content =
  | Link_to of int
  | Value of Value.value
  | Constr of (int*int list)
    
  let rec content_to_string c =
    match c with
    | Link_to i -> Printf.sprintf "Linked to %d" i
    | Value v -> Printf.sprintf "Some Value"
    | Constr (i,lst) -> Printf.sprintf "Contructeur %d(%s)" i (string_of_list "," (fun j -> content_to_string(Link_to j)) lst)

  (** The actual type of the data structure. The rank is used to
      implement weighted union. See {{:
      http://www.risc.jku.at/education/courses/ss2012/unification/slides/02_Syntactic_Unification_Improved_Algorithms.pdf}
      Introduction to Unification Theory. Speeding Up (Temur
      Kutsia)} *)
  type t = {rank:int Store.t;parents:content Store.t;limit:int}

    
  let empty = {rank=Store.empty () ;parents=Store.empty ();limit=0}

  exception Union_Failure

  let to_string h =
    IntMap.fold
      (fun i v acc -> 
	match v with
	| Link_to next -> Printf.sprintf "%s%d\t\t--->\t%d\n" acc i next
	| Value _ -> failwith "Bug: should not encounter Value"
	| Constr (c,i_args) -> Printf.sprintf "%s%d\t\t--->\t(%s)\n" acc i (string_of_list " -> " string_of_int i_args))
      h.parents
      ""
      
  let to_string {rank=r;parents=p} =
    let buff=Buffer.create 2 in
    let to_string_aux i =
      Printf.sprintf "%d\t<--->\t%s\t\t(%d)\n" i (content_to_string (Store.get i p)) (Store.get i r) in
    let i=ref 1 in
    try
      let () =
	while true do
	  let () = Buffer.add_string buff (to_string_aux !i) in
	  i:=!i+1
	done in
      "Bug!"
    with
    | Store.Store_Not_found -> Buffer.contents buff



  let generate_new_var {rank;parents;limit} =
    let i=limit+1 in
    i,{rank=Store.set i 0 rank;parents=Store.set i (Link_to i) parents;limit=i}

  let generate_new_constr {rank;parents;limit} c =
    let i=limit+1 in
    i,{rank=Store.set i 0 rank;parents=Store.set i (Constr c) parents;limit=i}


  let rank_increment i h =
    {h with rank=
	Store.set i 
	  (1+(
	    try
	      Store.get i h.rank
	    with
	    | Store.Store_Not_found -> 0))
	  h.rank}


  (** [find_and_instantiate_aux i t f] returns a new indexed storage
      datastructure [f'] where the content at index [i] (and the ones
      it points to) has been set to [Value t]. If [i]'s representative
      indexes a variable or a value equal to [Value t] then the
      instantiation suceeds, otherwise it raises Union_failure. It
      also performs path compression.  *)
  let rec find_and_instantiate_aux i term table f =
    match Store.get i f.parents with
    | Value v when v=term -> f 
    | Value _ -> raise Union_Failure
    (* An actual value was reached at index [i] and we're in the case
       that it differs from [term]. So the union fails *)
    | Link_to next when next=i -> 
    (* The content indexed by [i] points to [i]. [i] is then the
       representative for the variable it denotes and can be unified
       with [term]. [f] is updated. *)
      (match Value.unfold term table with
      | None -> {f with parents=Store.set i (Value term) f.parents}
      | Some (c,args) ->
	let i_args,new_content =
	  List.fold_left
	    (fun (acc,cont) arg ->
	      let var,new_cont=generate_new_var cont in
	      var::acc,find_and_instantiate_aux var arg table (rank_increment var new_cont))
	    ([],f)
	    args in
	{new_content with parents=Store.set i (Constr (c,List.rev i_args)) new_content.parents}	)
    | Link_to next ->
      (* In the other cases, we follow the links to reach the
	 representative and the content it indexes *)
      let new_f = find_and_instantiate_aux next term table f in
      (* Then we update the storage data structure linking the context
	 indexed by [i] directly to the representative index. We know
	 it's safe to do it now since unification succeeded. *)
      let updated_parents = Store.set i (Value term) new_f.parents in
      {f with parents=updated_parents}
    | Constr (c,i_args) ->
      (match  Value.unfold term table with
      | None -> raise Union_Failure
      | Some (c',args) when c=c'->
	(try
	   List.fold_left2
	     (fun cont var arg ->
	       find_and_instantiate_aux var arg table cont)
	     f
	     i_args
	     args
	 with
	 | Invalid_argument _ -> raise Union_Failure)
      | Some (c',_) ->raise Union_Failure)
	
  (** [instantiate i t h] returns a new indexed storage data structure
      where the value indexed by [i] and [t] have been unified. It
      fails and raises the {! UnionFind.Union_Failure} exception if
      [i]'s representative indexes an actual values [Value a] such
      that [a] differs from [t]. *)
  let instantiate i t table h = find_and_instantiate_aux i t table h


    
  (** [find_aux i f] returns a pair [(i',v),f'] where [i'] is the
      index of the representative of the data indexed by [i]. [i=i']
      means that the [i]-th element is linked to itself: it is meant
      to be a variable, not an actual value. It also performs path
      compression *)
  let rec find_aux i f =
    match Store.get i f with
    | Value _ as v -> (i,v),f 
    (* An actual value was reached at index [i]. So [i] is returned
       together with [v] and [f] *)
    | Constr _ as v -> (i,v),f 
    (* An almost actual value was reached at index [i]. So [i] is returned
       together with [v] and [f] *)
    | Link_to next as v when next=i -> (i,v),f
    (* The content indexed by [i] points to [i]. [i] is then the
       representative for the variable it denotes. *)
    | Link_to next ->
      (* In the other cases, we follow the links to reach the
	 representative and the content it indexes *)
      let (representative_index,representative_value),new_f = find_aux next f in
      (* Then we update the storage data structure linking the context
	 indexed by [i] directly to the representative index *)
      let updated_f = Store.set i (Link_to representative_index) new_f in
      LOG "the \"UnionFinf.find\" function indeed returns a Link_to itself: %b" (let ()=match representative_value with
      | Link_to variable -> assert (representative_index=variable)
      | _ -> () in true) LEVEL FATAL;
      (representative_index,representative_value),updated_f
	
  (** [find i h] returns a pair [(i',v),f'] where [i'] is the index of
      the representative of the data indexed by [i]. [i=i'] means that
      the [i]-th element is linked to itself: it is meant to be a
      variable, not an actual value. It also performs path
      compression. The difference with [find_aux] is that it applyes
      to the whole storage data structure (that includes data for
      weighted union). *)
  let find i h =
    let rep_i,f = find_aux  i h.parents in
    rep_i,{h with parents=f}

  (** [extract ~start:s i t] returns a list of the [i] first elements
      of [t] starting from position [s] (default is 1, first
      position). It is ensured that the results only contain the
      values of representatives (i.e it follows the [Link_to] links
      until the value of the representative before returning it). *)
  let extract ?(start=1) i {parents=p} =
    LOG "Going to extract %d elements starting at %d..." i start LEVEL DEBUG;
    let rec extract_aux k res =
      match k-start with
      | j when j>0 -> 
	let (_,c),_= find_aux (start-1+j) p in
	extract_aux (start+j-1) (c :: res)
      | _ -> res in
    extract_aux (start+i) []
    


    
    
	
  (** [union i j h] returns a new storage data structure [h'] where
      [h'] has an equivalent content as [h] plus the unification
      between the elements indexed by [i] and [j] and plus, possibly,
      some path compression. *)
  let rec union i j h =
    let rep_i,h' = find i h in
    let rep_j,h'' = find j h' in
    match rep_i,rep_j with
    (* in case [rep_i] (rexp. [rep_j]) is a [(i,Link_to i')] we should
       have [i=i'], else there is a bug *)
    | (_,v_i),(_,v_j) when v_i=v_j -> h''
      
    | (rep_i_index,(Value _ as v_i)),(rep_j_index,Link_to _) ->
      {h'' with	parents=Store.set rep_j_index v_i h''.parents}

    | (rep_i_index,Link_to _),(rep_j_index,(Value _ as v_j)) ->
      {h'' with parents=Store.set rep_i_index v_j h''.parents}
	
    | (rep_i_index,Constr _ ),(rep_j_index,Link_to _) ->
      {h''  with parents=Store.set rep_j_index (Link_to rep_i_index) h''.parents}

    | (rep_i_index,Link_to _),(rep_j_index,Constr _) ->
      {h'' with parents=Store.set rep_i_index (Link_to rep_j_index) h''.parents}

    | (rep_i_index,Constr (c_i,args_i)),(rep_j_index,Constr (c_j,args_j)) when c_i=c_j ->
      let h''' = union_list args_i args_j h'' in 
      let rk_i = Store.get rep_i_index h'''.rank in
      let rk_j = Store.get rep_j_index h'''.rank in
      if rk_i > rk_j then
	{h''' with 
	  parents=Store.set rep_i_index (Constr (c_i,List.rev args_i)) (Store.set rep_j_index (Link_to rep_i_index) h'''.parents)}
      else
	if rk_i < rk_j then
	  {h''' with 
	    parents=Store.set rep_j_index (Constr (c_i,List.rev args_j)) (Store.set rep_i_index (Link_to rep_j_index) h'''.parents)}
	else
	  {h''' with
	    parents=Store.set rep_i_index (Constr (c_i,List.rev args_i)) (Store.set rep_j_index (Link_to rep_i_index) h'''.parents);
	    rank=Store.set rep_i_index (rk_i+1) h'''.rank}
	
    | (rep_i_index,Link_to i'),(rep_j_index,Link_to j') -> 
      let rk_i = Store.get rep_i_index h''.rank in
      let rk_j = Store.get rep_j_index h''.rank in
      if rk_i > rk_j then
	{h'' with 
	  parents=Store.set rep_j_index (Link_to rep_i_index) h''.parents}
      else
	if rk_i < rk_j then
	  {h'' with 
	    parents=Store.set rep_i_index (Link_to rep_j_index) h''.parents}
	else
	  {h'' with 
	    parents=Store.set rep_j_index (Link_to rep_i_index) h''.parents;
	    rank=Store.set rep_i_index (rk_i+1) h''.rank}
    | (_,Value v_i),(_,Value v_j) -> 
      (* v_i=v_j is caught by the first case *)
      raise Union_Failure
    | (_,Value _ ),(_,Constr _) -> raise Union_Failure
    | (_,Constr _ ),(_,Value _) -> raise Union_Failure
    | (_,Constr _),(_,Constr _) -> 
      (* Constr (c,_), Constr (c,_)  is caught by the 6th case *)
      raise Union_Failure
  and union_list args_i args_j h =
    match args_i,args_j with
    | [],[] -> h
    | i::tl_i,j::tl_j -> union_list tl_i tl_j (union i j h)
    | _,_-> raise Union_Failure
      
  (* cyclic_aux includes path compression *)
  let rec cyclic_aux i f acc =
    match Store.get i f with
    | Value v -> false,i,f
    | Link_to next when next=i -> false,i,f
    | Link_to next ->
      if IntSet.mem next acc then
	true,i,f
      else
	let cyclic,representative_index,new_f = cyclic_aux next f (IntSet.add next (IntSet.add i acc)) in
	let updated_f = Store.set i (Link_to representative_index) new_f in
	cyclic,representative_index,updated_f
    | Constr(c,args) -> 
      let new_acc=IntSet.add i acc in
      List.fold_left
	(fun (c,l_i,l_f) arg ->
	  LOG "Preparing to check cyclicity from %d" arg LEVEL TRACE;
	  if IntSet.mem arg new_acc then
	    true,l_i,l_f
	  else
	    let is_c,_,new_f= cyclic_aux arg l_f new_acc in
	    is_c || c,l_i,new_f)
	(false,i,f)
	args
	
	
  (* the cyclic function, calling cyclic_aux, compress paths
     (hence also returns the parents) *)
  let cyclic i h = 
    LOG "Checking cyclicity from %d of:" i LEVEL TRACE ;
    log_iteration
      (fun s -> LOG "%s" s LEVEL TRACE)
      (to_string h);
    let res,_,f = cyclic_aux i h.parents (IntSet.empty) in
    res,{h with parents=f}
      
  let copy {rank=r;parents=p;limit}={rank=Store.copy r;parents=Store.copy p;limit}
    
    
      
end



