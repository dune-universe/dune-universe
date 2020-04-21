open Container
open Alba2_common

module Term = Term2



(* An inductive family consists of

   - a list of parameters

   - a list of inductive types

   - for each inductive type a list of constructors


   A simple definition defines only one inductive type with its list of
   constructors.

   Additional information:

   - a list of positive parameters

   - for each constructor a list of recursive arguments and a list of positive
   parameter arguments.


  *)

type param =
  string option * Term.typ * bool

type carg_class =
  | Normal          (* Normal argument, no recursion *)
  | Positive        (* Potentially recursive, contains a parameter positively *)
  | Recursive       (* Recursive argument *)

type constructor = {
    cname: Term.fname;
    typ:   Term.typ; (* valid in a context with all inductive types and the
                        parameters. *)
    argcls: carg_class list
  }


type t = {
    params:
      (string option  (* name *)
       * Term.typ     (* type *)
       * bool         (* parameter is positive? *)
      ) array;

    types: (Feature_name.t option
            * Term.typ      (* Aritiy of the inductive type, valid in a
                               context with parameters *)
            * Sorts.t       (* Sort of the arity *)
            * bool          (* elimination restricted? *)
           ) array;

    cs: (int                (* position of the first constructor of the type *)
         * constructor array(* constructors of the type *)
        ) array (* One set of constructors for each type *)
  }


let nparams ind = Array.length ind.params


let ntypes ind = Array.length ind.types


let nconstructors i ind =
  assert (i < ntypes ind);
  Array.length(snd ind.cs.(i))


let constructor_base_index i ind =
  (* The number of all constructors of the inductive types of the family
     before [i]. *)
  assert (i < ntypes ind);
  fst ind.cs.(i)


let params0 ind: Term.arguments =
  Array.map (fun (nme,tp,_) -> nme,tp) ind.params


let params ind: Term.arguments =
  let ni = ntypes ind
  and np = nparams ind in
  Array.map (fun (nme,tp,_) -> nme, Term.up_from np ni tp) ind.params

let positive_parameters ind =
  Array.map (fun (_,_,flag) -> flag) ind.params


let name i ind =
  assert (i < ntypes ind);
  let nme,_,_,_ = ind.types.(i) in
  nme


let itype i ind =
  (* The inductive type (and its name) in the base context. *)
  assert (i < ntypes ind);
  let nme,tp,_,_ = ind.types.(i) in
  nme, Term.push_product (params0 ind) tp


let types0 (ind:t): Term.gamma =
  (* The inductive types in a context with parameters. *)
  Array.map
    (fun (nme,tp,_,_) -> nme,tp)
    ind.types

let types (ind:t): Term.gamma =
  let pars = params0 ind in
  Array.map
    (fun (nme,tp,_,_) -> nme, Term.push_product pars tp)
    ind.types

let sort i ind =
  assert (i < ntypes ind);
  let _,_,s,_ = ind.types.(i) in
  s


let is_restricted i ind =
  assert (i < ntypes ind);
  let _,_,_,res = ind.types.(i) in
  res


let is_restriction_consistent i ind =
  assert (i < ntypes ind);
  let _,_,s,res = ind.types.(i)
  and _,cs = ind.cs.(i)
  in
  let ncs = Array.length cs
  in
  (not res (* Restriction => Prop and more than 0 constructors. *)
   || (s = Sorts.Proposition && ncs > 0))
  && (s <> Sorts.Proposition
      || not (2 <= ncs)
      || (* A propositon with at least 2 constructors must be restricted. *)
        res)

let all_args_propositions i ind =
  assert (i < ntypes ind);
  let _,_,s,res = ind.types.(i)
  and _,cs = ind.cs.(i)
  in
  not res
  && s = Sorts.Proposition
  && Array.length cs = 1



let cname i j ind =
  assert (i < ntypes ind);
  let _,cs = ind.cs.(i) in
  assert (j < Array.length cs);
  cs.(j).cname


let constructors i ind =
  assert (i < ntypes ind);
  let _,cs = ind.cs.(i) in
  Array.map (fun co -> co.cname, co.typ, co.argcls) cs


let constructor_arguments i j ind =
  assert (i < ntypes ind);
  let _,cs = ind.cs.(i) in
  assert (j < Array.length cs);
  cs.(j).argcls


let ctype i j ind =
  (* The type of the [j]th constructor of the type [i] of the family in an
     environment with all absolute inductive types and no parameters. *)
  assert (i < ntypes ind);
  let _,cs = ind.cs.(i) in
  assert (j < Array.length cs);
  let pars = params ind in
  cs.(j).cname, Term.push_product pars cs.(j).typ



let rec ndproduct (lst:Term.typ list): Term.typ =
  match lst with
  | [] ->
     assert false (* Illegal call: At least one type *)
  | [tp] ->
     tp
  | hd :: tl ->
     Term.All (None, hd, ndproduct tl)


let cmake cname argcls typ =
  {cname;typ;argcls}


let make params types cs =
  let ni = List.length types in
  assert (List.length cs = ni);
  let _,cs =
    List.fold_left
      (fun (i,lst) clst ->
        let carr = Array.of_list clst in
        let n = Array.length carr in
        i+n, (i,carr) :: lst)
      (0,[]) cs in
  {params = Array.of_list params;
   types = Array.of_list types;
   cs = Array.of_list cs}



let make_simple name params typ sort restr cs =
  make params [name,typ,sort,restr] [cs]









(* class false create end *)
let make_false: t =
  let open Term in
  make_simple
    some_feature_false
    []
    proposition
    Sorts.Proposition
    false
    []



(* class true create
       true_is_valid
    end *)
let make_true: t =
  let open Term in
  make_simple
    some_feature_true
    []
    proposition
    Sorts.Proposition
    false
    [cmake (some_feature_name "true_is_valid") [] variable0]




(* class
       (and) (a,b:Proposition): Proposition
   create
       conjunction (a,b): a and b
   end
 *)
let make_and: t =
  let open Term in
  make_simple
    (some_feature_operator Operator.andop)
    [ Some "a", proposition, true;
      Some "b", proposition, true ]
    proposition
    Sorts.Proposition
    false
    begin
      let vand = variable0
      and a = variable1
      and b = variable2
      in
      let a_and_b = apply2 vand a b in
      [ cmake
          (some_feature_name "conjunction")
          [Positive; Positive]
          (ndproduct [a; b; a_and_b] |> to_index 3)
      ]
    end





(* class
       (or) (a,b:Proposition): Proposition
   create
       left (a): a or b
       right(b): a or b
   end
 *)
let make_or: t =
  let open Term in
  make_simple
    (some_feature_operator Operator.orop)
    [ Some "a", proposition, true;
      Some "b", proposition, true ]
    proposition
    Sorts.Proposition
    true
    begin
      let vor = variable0
      and a   = variable1
      and b   = variable2
      and n   = 3 in
      let a_or_b = apply2 vor a b in
      [ cmake
          (some_feature_name "left")
          [Positive]
          (ndproduct [a; a_or_b] |> to_index n);
        cmake
         (some_feature_name "right")
          [Positive]
          (ndproduct [b; a_or_b] |> to_index n);
      ]
    end




(* class
      exist (A:Any, pred:A->Proposition): Proposition
   create
      exist_intro (a:A, p:pred(a)): exist(A,pred)
   end
 *)
let make_exist: t =
  let open Term in
  make_simple
    (some_feature_name "exist")
    [ Some "A", any, false;
      Some "pred", arrow variable0 proposition, false]
    proposition
    Sorts.Proposition
    true (* elimination restricted *)
    begin
      let exist = variable0
      and abig  = variable1
      and pred  = variable2
      and a     = variable3
      and n     = 3 in
      [cmake
         (some_feature_name "exist_intro")
         [Normal; Normal]
         (All (Some "a",
               abig,
               ndproduct [apply1 pred a; apply2 exist abig pred])
          |> to_index n)
      ]
    end




(* class
       accessible (A:Any, r:Relation(A,A), y:A): Proposition
   create
       access_intro
           (f:all(x) r(x,y) -> r.accessible(x))
           : r.accessible(y)
   end
 *)
let make_accessible: t =
  let open Term in
  make_simple
    (some_feature_name "accessible")
    [ Some "A", any, false;
      Some "r", arrow variable0 (arrow variable0 proposition), false;
      Some "y", variable1, false]
    proposition
    Sorts.Proposition
    false
    begin
      let acc = variable0
      and a =   variable1
      and r =   variable2
      and y =   variable3
      and x =   variable4
      and n =   4 in
      [cmake
         (some_feature_name "access_intro")
         [Recursive]
         (All (Some "f",
               All(Some "x",
                   a,
                   ndproduct [apply2 r x y; apply3 acc a r x]
                 ),
               apply3 acc a r y
            )
          |> to_index n)
      ]
    end





(* class
       (=) (A:Any, a:A): all(B:Any) B -> Proposition
   create
       reflexive: a = a
   end
 *)
let make_equal: t =
  let open Term in
  let abig = variable0
  and bbig = variable2
  and n = 4
  in
  make_simple
    (some_feature_operator Operator.eq)
    [
      Some "A", any,  true;
      Some "a", abig, false
    ]
    ( All(Some "B", any, ndproduct [bbig; proposition])
      |> to_index n)
    Sorts.Proposition
    false
    begin
      let eq = variable0
      and abig = variable1
      and a = variable2
      and n = 3 in
      [
        cmake
          (some_feature_name "reflexive")
          []
          (apply4 eq abig a abig a |> to_index n)
      ]
    end






(* class
       Natural
   create
       0
       successor(Natural)
   end *)
let make_natural: t =
  let open Term in
  make_simple
    (some_feature_name "Natural")
    []       (* no parameter *)
    any      (* of sort any *)
    Sorts.Any
    false    (* no elim restriction *)
    begin
      let nat = variable0 in
      [
        cmake (some_feature_number 0) [] variable0;
        cmake
          (some_feature_name "successor") [Recursive]
          (ndproduct [nat;nat] |> to_index 1)
      ]
    end






(* class
       List(A)
   create
       []
       (^)(A,List(A))
   end
 *)
let make_list: t =
  let open Term in
  make_simple
    (some_feature_name "List")
    [ Some "A", any, true]                  (* one parameter *)
    any                                     (* arity *)
    Sorts.Any
    false                                   (* no elim restriction *)
    begin
      let lst = variable0
      and a = variable1 in
      let lsta = apply1 lst a
      in
      [ cmake
          some_feature_bracket
          []
          (apply1 lst a |> to_index 2);

        cmake
          (some_feature_operator Operator.caret)
          [Positive; Recursive]
          (ndproduct [a; lsta; lsta] |> to_index 2)
      ]
    end



(* class
       Tree(A)
   create
       node (A, List(Tree(A)))
   end
 *)
let make_tree (i_lst:int): t =
  let open Term in
  make_simple
    (some_feature_name "Tree")
    [Some "A", any, true]                   (* one parameter *)
    any                                     (* arity *)
    Sorts.Any
    false                                   (* no elim restriction *)
    begin
      let cnt = i_lst + 1
      in
      let lst  = Variable 0
      and tree = Variable cnt
      and a    = Variable (cnt + 1)
      in
      let tree_a = apply1 tree a
      in
      let lst_tree_a =apply1 lst tree_a
      in
      [ cmake
          (some_feature_name "node")
          [Positive; Recursive]
          (ndproduct [a; lst_tree_a; tree_a] |> to_index (cnt+2))
      ]
    end
