(** Time-stamp: <modified the 21/11/2016 (at 17:07) by Erwan Jahier> *)

(** Some experiments on heuristics used in schedule Actions.  Indeed,
    there are 2 dimensions: 

    - at worst, there exists n! possible topological sorts (total
    orderings of actions) of a DAG (a partial ordering induced by
    dependencies). Some orderings are better than others because they
    makes it possible to factorize test opennings. Indeed, compare 

    "if c then e1; if c then e2;" 

    with 
    
    "if c then (e1;e2)"

    - Beside, some more tests can be saved by duplicating code. for ex
    
    "if c then e1 ; e2 ; if c then  e3;"
    
    can be tranformed into
    
    "if c then (e1 ; e2 ; e3) else e2;"
    
    which saves 1 test. But of course the code size may blows up, and some trade-offs
    should be chosen.


    Idées d'heuristiques : 

    - Quand on choisit un successeur parmis dep(a), on pourrait prendre
    celui qui a le moins de dépendances.
*)

(* split_al [a1;a2;...;an] returns [a1;a2;...;ak],[ak+1;...;an] where k is
   the greater index such that {ai}_{i in [1,k]} is a set of independant
   actions. 
   nb : 
    - k=>1
    - the list in input is already sorted wrt deps (i.e., i<j => ai <= aj)
*)
let (split_al : ActionsDeps.t -> Action.t list -> Action.t list * Action.t list) =
  fun deps al ->
    (* al is (topologically) sorted, i.e.,  i<j => al_i <= al_j 
       (where a <= b means a depends on b or not)
    *)
    let rec aux acc = function
      | [] -> acc, []
      | a::al ->
        if List.for_all (fun acc1 -> not(ActionsDeps.depends_on deps a acc1)) acc then
          aux (a::acc) al
        else
          acc, (a::al)
    in 
    aux [] al

(* XXX switch cc and cv in the def of Lic.clock for the "On" variant 
   to avoid this function
*)
let compare_actions a1 a2 = 
  let (c1,r1,l1,op1,lxm1) = a1 in
  let (c2,r2,l2,op2,lxm2) = a2 in
  match c1,c2 with
    | Lic.On((cc1, cv1, t1),clk1),Lic.On((cc2, cv2, t2),clk2) -> 
      compare (cv1,cc1,t1,clk1,r1,l1,op1,lxm1) (cv2,cc2,t2,clk2,r2,l2,op2,lxm2)
    | _,_ -> compare a1 a2

(* In order to sort Soc.gao, I currently process in 3 stages ;
1. compute a total ordering of Actions.t according to the deps
2. transform Actions.t into Soc.gao (List.map)
3. factorize Soc.gao by looking at consecutive gao that have the same guard

   group is meant to work between steps 1 and 2, and reorder the list of
   actions so that group of independent actions are sorted according to
   their clock. The idea is of course to provide to step 3 more
   opportunities of factorization.
*)
let (group : Action.t list -> ActionsDeps.t -> Action.t list list) =
  fun al deps ->
    let rec aux al acc =
      let al1, al2 = split_al deps al in 
      let al1 = List.sort compare_actions al1 in 
    (* the clock is the left most element of the Action.t tuple *)
      match al2 with
        | [] -> List.rev (al1::acc)
        | _ -> aux al2 (al1::acc)
    in 
    let res = aux al [] in (*
    List.iter (fun al ->
      let strl = List.map Action.to_string_msg al in
      Printf.printf "\n*** actions that are grouped :\n   %s\n" (String.concat "\n   " strl) ;
      flush stdout
    ) res;*)
    res

(* duplicated! *)
let (optimize_test_openning: Soc.gao list list -> Soc.gao list) =
  fun gaoll ->
    let rec merge_consecutive acc gaol = match gaol with
      | []  -> List.rev acc
      | [a] -> List.rev (a::acc)
      | Soc.Call(o,op,i,lxm)::tail -> merge_consecutive (Soc.Call(o,op,i,lxm)::acc) tail
      | a1::Soc.Call(o,op,i,lxm)::tail ->
        merge_consecutive (Soc.Call(o,op,i,lxm)::a1::acc) tail 
      | Soc.Case(v1,l1,lxm1)::Soc.Case(v2,l2,lxm2)::tail -> 
        if v1 <> v2 then
          merge_consecutive (Soc.Case(v1,l1,lxm1)::acc) (Soc.Case(v2,l2,lxm2)::tail)
        else
          let l = merge_gaol l1 l2 [] in
          merge_consecutive acc (Soc.Case(v1,l,lxm1 (* xxx *))::tail) 
            
    and (merge_gaol : (string * Soc.gao list) list -> (string * Soc.gao list) list -> 
         (string * Soc.gao list) list -> (string * Soc.gao list) list) =
      fun l1 l2 acc -> 
        match l1 with
          | [] -> if l2 = [] then acc else List.rev_append acc l2
          | (x1,gaol1)::l1 -> 
            (match Lv6util.my_assoc x1 l2 with
              | None -> merge_gaol l1 l2 ((x1,gaol1)::acc)
              | Some(gaol2,l2) -> 
                let gaol = merge_consecutive [] (gaol1@gaol2) in
                merge_gaol l1 l2 ((x1,gaol)::acc)
            )
    in
    (* inner lists are sorted, hence merge_consecutive should hits some merge *)
    let gaoll = List.map (merge_consecutive []) gaoll in
    let gaoll = List.mapi (fun i gaol -> if i mod 2 = 1 then List.rev gaol else gaol) gaoll in 
    let gaol = List.flatten gaoll in
     merge_consecutive []  gaol
(* devrait pouvoir faire mieux !!*)      
