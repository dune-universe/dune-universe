module SharedForest =
  struct

    let src = Logs.Src.create "ACGtkLib.sharedForest" ~doc:"logs ACGtkLib sharedForest events"
    module Log = (val Logs.src_log src : Logs.LOG) 


    
    (** This type is the type of addresses of forests. It is a list of
      (position in the forest,position as a child). *)
    type address=(int*int) list
                           
    (** This is the type of relative path from one forest to another
      one. The first argument is the number of steps to move up, then
      second argument is the address to reach from this point. *)
    type relative_path=int*address
                             
  let rec diff_aux add1 add2 back =
    match add1,add2 with
    | [],[] -> back,[]
    | _,[] -> back+List.length add1,[]
    | [],_ -> back,add2
    | (i,j)::tl1,(i',j')::tl2 when i=i' && j=j' -> diff_aux tl1 tl2 back
    | _::_,_::_ -> back+List.length add1,add2

  (** [diff add add'] returns the relative path to go from the
      forest (subtree) wich occurs at address [add] to the forest
      (subtree) wich occurs at address [add']. *)
  let diff add1 add2 = diff_aux add1 add2 0
    
  let address_to_string addr = 
    Printf.sprintf
      "[%s]"
      (Utils.string_of_list ";" (fun (i,j) -> Printf.sprintf "(%d,%d)" i j) addr)
      
  let path_to_string (i,add) =
    Printf.sprintf "(-%d,%s)" i (address_to_string add)
      
  type 'a stack='a list
  type 'a list_context ='a stack
    
  type 'a focused_list = 'a list_context * 'a list
    
    
  (** Recursive definition of a shared forest. *)
  type 'a forest = 'a tree focused_list
  and 'a tree = Node of 'a * 'a child list
  and 'a child = 
  | Forest of 'a forest
  | Link_to of relative_path

  (** Defintion of a "forest zipper" *)
  type 'a forest_zipper = 
  | Top of ('a tree) focused_list * int
  | Zip of
      'a * 
	(* The first element is the label of the node *)
	('a child) focused_list *
	(* the focused list of children of the tree. Just as for tree
	   zippers *)
	('a tree) focused_list *
	(* the focused list of the focuses child: a forest *)
	int *
	(* the position of the tree under focus in the current forest *)
	'a forest_zipper *
	(* the forest context *)
	'a forest_zipper option *
	(* a local context describing the way to reach the current
	   tree from top in case it was reached after a [Link_to] move,
	   so that if some other [Link_to] is met under thus subtree that
	   points higher, it goes to the right place. *)
	address
    (* The address of the current tree. Actually not used. *)

  (** Type definition for the focused forests *)
  type 'a focused_forest = 'a forest_zipper * 'a  tree
    
  (** Type definition for standard trees *)
  type 'a simple_tree = SimpleTree of 'a * 'a simple_tree list
    
  (** Type definition for standard tree zippers *)
  type 'a zipper =  ZTop | Zipper of ('a * 'a simple_tree focused_list * 'a zipper)
      
  (** Type definition for standard focused trees *)
  type 'a focused_tree = 'a zipper * 'a simple_tree
    

  let rec fold_depth_first ((transform,apply) as f) t =
    match t with
    | SimpleTree (v,[]) -> transform v
    | SimpleTree (v,children) -> 
      List.fold_left
	(fun acc child -> apply acc (fold_depth_first f child))
	(transform v)
	children

  type 'a simple_resumption = ('a focused_forest * 'a focused_tree * int) list
    
  type 'a delayed_resumption = ('a simple_resumption) Utils.IntMap.t
    
  type 'a resumption = 'a simple_resumption * 'a delayed_resumption

  let empty = [],Utils.IntMap.empty
    
  exception Infinite_loop

  let extend_simple_resume (f_f,f_t,i) resume = (f_f,f_t,i)::resume

  let extend_sized_indexed_resume (f_f,f_t,i) resume =
    try
      Utils.IntMap.add i ((f_f,f_t,i)::(Utils.IntMap.find i resume)) resume
    with
    | Not_found -> Utils.IntMap.add i [(f_f,f_t,i)] resume

  let extend_resume ?actual ?delayed ((resume1,resume2):'a resumption) =
    match actual,delayed with
    | None,None -> resume1,resume2
    | Some v,None -> extend_simple_resume v resume1,resume2
    | None,Some v -> resume1,extend_sized_indexed_resume v resume2
    | Some v1,Some v2->extend_simple_resume v1 resume1,extend_sized_indexed_resume v2 resume2

  type move =
  | Up
  | Down
  | Right
  | Forward
  | Backward
  | Cycle

  exception Move_failure of move
  exception Not_well_defined
  exception No_next_alt
  exception No_previous_alt
  exception Bad_argument
  exception Bad_address

  let swap = function
    | [],delayed ->
       (match Utils.IntMap.min_binding delayed with
	| _,[] -> failwith "Bug: such a binding should be removed"
	| i,[a] -> a,([], Utils.IntMap.remove i delayed)
	| i,a::res -> a,(res,Utils.IntMap.remove i delayed)
        | exception Not_found -> raise No_next_alt)
    | a::actual,delayed -> a,(actual,delayed)


  let rec unstack = function
    | [],l ->l
    | a::tl,l -> unstack (tl,(a::l))

  let rec f_list_up = function
    | [],l -> [],l
    | a::s,l-> f_list_up (s,a::l)

  let f_list_cycle = function
    | [],[] -> raise (Move_failure Cycle)
    | p,a::n -> a::p,n
    | f_lst -> f_list_up f_lst

  let focus_of = function
    | [],[] -> raise Not_well_defined
    | p,a::n -> (p,n),a,1+List.length p
    | p,[] -> 
      match List.rev p with
      | [] -> raise Not_well_defined
      | a::n ->  ([],n),a,1


  let rec f_list_fold ((p,n),i) f acc =
    match n with
    | [] -> (p,[]),acc
    | a::tl -> f_list_fold ((a::p,tl),i+1) f (f ((p,tl),i) a acc)

  let f_tree_up = function
    | ZTop,t -> raise (Move_failure Up)
    | Zipper (v,(l,r),z'),t -> z',SimpleTree (v,unstack (l,t::r))

  let rec zip_up_aux f_tree = 
    try
      zip_up_aux (f_tree_up f_tree)
    with
    | Move_failure Up -> f_tree

  let zip_up f_tree =
    let _,t = zip_up_aux f_tree in
    t

  let rec move_forward i (l,r) =
    match i,r with
    | 1,a::tl -> (l,tl),a
    | i,a::tl when i>1 -> move_forward (i-1) (a::l,tl)
    | _ -> raise Bad_address


  let forest_address = function
    | Top _ -> []
    | Zip (_,_,_,_,_,_,add) -> add

  let tree_address = function
    | Top ((_,_),i) -> i,[]
    | Zip (_,_,_,i,_,_,add) -> i,add





  (** [enter add (z,t)] returns the forest at address [add] starting
      from the current forest of [z] [t] is belonging to.
      
      Invariant: the result is [(z,t),forest] where [t] belongs to the
      forest [forest]. [t] is the focused element of the forest *)
  let rec enter addr (z,(Node (v,children) as t)) =
    Log.info (fun m -> m "Entering \"%s\" on a node with %d children%!" (address_to_string addr) (List.length children)) ;
    match addr with
    | [] -> 
      (match z with
      | Top (([],[]),_) -> (z,t),([],[t])
      | Top ((p,[]),_) ->
	(match unstack (p,[t]) with
	| [] -> raise Not_well_defined
	| a::n -> (Top (([],n),1),a),([],a::n))
      | Top ((p,a::n),i) -> (Top ((t::p,n),i+1),a),(t::p,a::n)
      | Zip (_,_,([],[]),_,_,_,_) -> (z,t),([],[t])
      | Zip (v,sibling,(p,[]),i,z',l_c,add) -> 
	(match unstack (p,[t]) with
	| [] -> raise Not_well_defined
	| a::n -> (Zip (v,sibling,([],n),1,z',l_c,add),a),([],a::n))
      | Zip (v,sibling,(p,a::n),i,z',l_c,add) -> 
	(Zip (v,sibling,(t::p,n),i+1,z',l_c,add),a),(t::p,a::n))
    | (j_alt,i_child)::tl ->
      let z,Node(v',children')=
	match z with
	| Top ((p,n),_) -> 
	  let (p,n),t'= move_forward j_alt (f_list_up (p,t::n)) in
	  Top ((p,n),j_alt),t'
	| Zip (v',(l,r),(p,n),_,z',l_c,add) ->
	  let (p,n),t'=move_forward j_alt (f_list_up (p,t::n)) in
	  Zip (v',(l,r),(p,n),j_alt,z',l_c,add),t' in
      let (l',r'),f_forest=move_forward i_child ([],children') in
      match f_forest with
      | Forest f ->
	let (p',n'), t'=move_forward 1 (f_list_up f) in
	enter tl (Zip(v',(l',r'),(p',n'),1,z,None,(j_alt,i_child)::(forest_address z)),t')
      | Link_to (back,addr) -> forest_at (back-1,addr) (z,Node(v',children'))
  and forest_at (back,addr) (z,(Node (_,children) as t)) = 
    Log.info (fun m -> m "Look for forest at path %s\n%!" (path_to_string (back,addr)));
    Log.info (fun m -> m "current focused tree has %d children\n%!" (List.length children));
    if back < 0 then
      failwith "Bug: looking for a forest with a negative back parameter"
    else
      match z,t with
      | Top _ ,_ when back>0 -> raise (Move_failure Up)
      | _,_ when back=0 -> enter addr (z,t)
      | Zip (v,(l,r),(p,n),_,z',None,_),t -> 
	let children=unstack (l,(Forest (t::p,n))::r) in
	forest_at (back-1,addr) (z',Node (v,children))      
      | Zip (_,_,(p,n),_,_,Some local_context,_),t -> 
	(match local_context with
	| Top ((p,n),i) -> failwith "want to move back on a top context"
	| Zip (v,(l,r),(p,n),_,z',_,_) ->
	  let children=unstack (l,(Forest (t::p,n))::r) in
	  forest_at (back-1,addr) (z',Node (v,children)))
      | _,_ -> raise Bad_address
	
  let set_local_context l_ctxt = function
    | Top _ -> raise Bad_argument
    | Zip (v,sibling,alt,i,z,_,add) -> Zip (v,sibling,alt,i,z,Some l_ctxt,add) 

  (** [next_alt f_forest] returns the next possible focused forest
      where the tree under focus in the forest has moved to the next
      one. It raises [No_next_alt] if [f_forest] focuses on the last
      one of the current forest. *)
  let next_alt (z,t) =
    match z with
    | Top ((_,[]),_) -> raise No_next_alt
    | Top ((p,a::tl),i) ->  (Top ((t::p,tl),i+1),a)
    | Zip (_,(_,_),(_,[]),_,_,_,_) -> raise No_next_alt
    | Zip (v,(l,r),(p,a::n),i,z',local_context,add) -> 
      (Zip (v,(l,r),(t::p,n),i+1,z',local_context,add),a)

  (** [previous_alt f_forest] returns the previous possible focused
      forest where the tree under focus in the forest has moved to the
      previous one. It raises [No_next_alt] if [f_forest] focuses on
      the first one of the current forest. *)
  let previous_alt (z,t) =
    match z with
    | Top (([],n),_) -> raise No_previous_alt
    | Top ((a::p,n),i) ->  (Top ((p,t::n),i-1),a)
    | Zip (_,(_,_),([],_),_,_,_,_) -> raise No_previous_alt
    | Zip (v,(l,r),(a::p,n),i,z',local_context,add) -> 
      (Zip (v,(l,r),(p,t::n),i-1,z',local_context,add),a)

	
  let rec get_all_next_alt_aux (z,t) acc =
    try
      let alt= next_alt (z,t) in
      get_all_next_alt_aux alt (alt::acc)
    with
    | No_next_alt -> acc

  let rec get_all_previous_alt_aux (z,t) acc =
    try
      let alt= previous_alt (z,t) in
      get_all_previous_alt_aux alt (alt::acc)
    with
    | No_previous_alt -> acc


  let get_all_alt (z,t) acc =
    let acc = get_all_next_alt_aux (z,t) acc in
    let acc = get_all_previous_alt_aux (z,t) acc in
    List.rev acc
      
  let simple_tree (Node (v,_)) = SimpleTree (v,[])
    
  let down (z,t) (zipper,b_t) depth resume=
    match t with
    | Node (_,[]) -> raise (Move_failure Down)
    | Node (v,forest::tl) ->
      (match forest with
      | Link_to (back,add) ->
	let (z'',_),f =  forest_at (back-1,add) (z,t) in
	let (p,n),a = 
	  match f with
	  | _,[] -> raise Bad_address
	  | p,a::n -> (p,n),a in
	let foc_forest = Zip (v,([],tl),(p,n),1+List.length p,z,Some z'',forest_address z''),a in
	let zipper=Zipper(v,([],[]),zipper) in
	let foc_tree=zipper,simple_tree a in
	(match add with
	| [] ->
	  let resume =
	    let all_alt = get_all_alt foc_forest [foc_forest] in
	    List.fold_left
	      (fun acc (z,t) ->
		extend_resume ~delayed:((z,t),(zipper,simple_tree t),depth+1) acc)
	      resume
	      all_alt in
	  let (foc_forest,foc_tree,depth'),resume = swap resume in
	  foc_forest,foc_tree,depth',resume
	| _ ->
	  let resume =
	    let all_alt = get_all_alt foc_forest [] in
	    List.fold_left
	      (fun acc (z,t) ->
		extend_resume ~actual:((z,t),(zipper,simple_tree t),depth+1) acc)
	      resume
	      all_alt in
	  foc_forest,foc_tree,depth+1,resume)
      | Forest ([],[]) -> raise Not_well_defined
      | Forest l_f ->
	let t_alt,add=tree_address z in
	let (p,n),a,pos=focus_of l_f in
	let foc_forest=Zip (v,([],tl),(p,n),pos,z,None,(t_alt,1)::add),a in
	let zipper=Zipper(v,([],[]),zipper) in
	let foc_tree=zipper,simple_tree a in
	let resume =
	  let all_alt = get_all_alt foc_forest [] in
	  List.fold_left
	    (fun acc (z,t) -> extend_resume ~actual:((z,t),(zipper,simple_tree t),depth+1) acc)
	    resume
	    all_alt in
	foc_forest,foc_tree,depth+1,resume)
	
  let right (z,t) (zipper,b_t) depth resume =
    match z,zipper with
    | _ ,ZTop -> raise (Move_failure Right)
    | Top _,_ ->  raise (Move_failure Right)
    | Zip (v,(_,[]),_,_,_,_,_), Zipper(v',_,_) when v=v'-> raise (Move_failure Right)
    | Zip (v,(l,a::r),(p,n),i,z',_,add), Zipper(v',(l',r'),z'') when v=v'->
      let l_c,f,loop =
	match a with
	| Forest f -> None,f_list_up f,false
	| Link_to (back,[]) -> 
	  let new_ctx = z', Node(v,unstack (l,(Forest(t::p,n))::a::r)) in
	  let (z,_),f=forest_at (back-1,[]) new_ctx in
	  Some z,f,true
	| Link_to (back,add) -> 
	  let new_ctx = z', Node(v,unstack (l,(Forest(t::p,n))::a::r)) in
	  let (z,_),f=forest_at (back-1,add) new_ctx in
	  Some z,f,false in
      let (p',n'),t' = 
	match f with
	| _,[] -> raise Bad_address
	| p,a::n -> (p,n),a in
      let foc_forest = Zip (v,((Forest(p,t::n))::l,r),(p',n'),1,z',l_c,add),t' in
      let zipper= Zipper(v',(b_t::l',r'),z'') in
      let foc_tree=zipper,simple_tree t' in
      (match loop with
      | true -> 
	let resume =
	  let all_alt = get_all_alt foc_forest [foc_forest] in
	  List.fold_left
	    (fun acc (z,t) ->
	      extend_resume ~delayed:((z,t),(zipper,simple_tree t),depth) acc)
	    resume
	    all_alt in
	let (foc_forest,foc_tree,depth'),resume = swap resume in
	foc_forest,foc_tree,depth',resume
      | false ->
	let resume =
	  let all_alt = get_all_alt foc_forest [] in
	  List.fold_left
	    (fun acc (z,t) ->
	      extend_resume ~actual:((z,t),(zipper,simple_tree t),depth) acc)
	    resume
	    all_alt in
	foc_forest,foc_tree,depth,resume)
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"
      

  let up (z,t) (zipper,b_t) depth =
    match z,zipper with
    | Top _,ZTop -> raise (Move_failure Up)
    | _,ZTop -> failwith "Bug: both forest and tree context should be top"
    | Top _,_ ->  failwith "Bug: both forest and tree context should be top"
    | Zip (v,(l,r),(p,n),_,z',_,_),Zipper(v',_,_) when v=v' -> 
      (z',Node (v,unstack (l,(Forest(p,t::n))::r))),
      f_tree_up (zipper,b_t),
      depth-1
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"


  let rec close_forest_context_up f_forest f_tree depth resume =
    try
      let f_forest,f_tree,depth = up f_forest f_tree depth in
      (try
	 right f_forest f_tree depth resume
       with 
       | Move_failure Right -> 
	  (try
	     close_forest_context_up f_forest f_tree depth resume
	   with
	   | Move_failure Up -> f_forest,f_tree,depth,resume))
    with
    | Move_failure Up -> f_forest,f_tree,depth,resume
	
	
  let rec build_tree_aux f_forest f_tree depth resume=
    try
      Log.info (fun m -> m "Trying to go down");
      let f_forest,f_tree,depth,resume = down f_forest f_tree depth resume in
      Log.info (fun m -> m "Succeeded") ;
      build_tree_aux f_forest f_tree depth resume
    with
    | Move_failure Down ->
      (try
         Log.info (fun m -> m "Trying to go right");
	 let f_forest,f_tree,depth,resume = right f_forest f_tree depth resume in
         Log.info (fun m -> m "Succeeded");
	 build_tree_aux f_forest f_tree depth resume
       with
       | Move_failure Right ->
         Log.info (fun m -> m "Trying to close up");
	 (match close_forest_context_up f_forest f_tree depth resume with
	 | ((Top _ ,_),(ZTop,_),_,_) as res -> 
         Log.info (fun m -> m "Succeeded");
	   res
	 | (Zip _,_) as l_f_forest,((Zipper _,_) as l_f_tree),depth',resume' -> 
         Log.info (fun m -> m "Succeeded");
         Log.info (fun m -> m "Trying to restart a building");
	   build_tree_aux l_f_forest l_f_tree depth' resume'
	 | _ -> failwith "Bug: not representing the same tree"))
	
  let build_tree f_forest f_tree depth resume = build_tree_aux f_forest f_tree depth resume

  let rec build_trees_aux f_forest f_tree depth resume acc =
    let _,(_,tree),_,resume = build_tree f_forest f_tree depth resume in
    try
      let (f_forest,f_tree,depth),resume = swap resume in
      build_trees_aux f_forest f_tree depth resume (tree::acc)
    with
    | No_next_alt -> tree::acc

      

  let init alt_trees =
    (snd (f_list_fold
	    (([],alt_trees),1)
	    (fun ((p,n),i) t acc -> ((Top ((p,n),i),t),(ZTop,simple_tree t),1)::acc)
	    [])),
    Utils.IntMap.empty

  let build_trees forest =
    match init forest with
    | [],_ -> failwith "Bug"
    | (f_forest,f_tree,depth)::res1,res2 -> 
      let res = build_trees_aux f_forest f_tree depth (res1,res2) [] in
      res
	
  let resumption (res) = 
    match res with
    | [],_ ->
      (try
	 let (f_forest,f_tree,depth),resume=swap res in
	 let _,(_,tree),_,res'=build_tree f_forest f_tree depth resume in
	 Some tree,(res')
       with
       | No_next_alt -> None,res)
    | (f_forest,f_tree,depth)::resume,delayed -> 
      let _,(_,tree),_,res'=build_tree f_forest f_tree depth (resume,delayed) in
      Some tree,(res')


  let is_empty = function
    | ([],_) as res ->
      (try
	 let _ = swap res in
	 false
       with
       | No_next_alt -> true)
    | _ -> false
end


