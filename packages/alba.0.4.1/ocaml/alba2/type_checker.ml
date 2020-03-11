open Container
open Common
open Alba2_common
open Printf

module Term = Term2


module ListMOption = List.Monadic (Option)



let string_of_term (c:Gamma.t) (t:Term.t): string =
  let module TP = Term_printer.Make (Gamma) in
  let module PP = Pretty_printer2.Pretty_string in
  PP.string_of
    (PP.of_document (TP.term c TP.detailed t))
    200 0 70 70


let string_of_fixpoint (c:Gamma.t) (fp:Term.fixpoint): string =
  let module TP = Term_printer.Make (Gamma) in
  let module PP = Pretty_printer2.Pretty_string in
  PP.string_of
    (PP.of_document (TP.fixpoint c TP.detailed fp))
    200 0 70 70




(* =============================================

   Key Reduction a -> b

   =============================================


   1. beta reduction: (lam x:A. t) a args -> t[x:=a] args



   2. definition expansion: x a args -> t a args

      where t is the definition of x


   3. Pattern match: (Parameters???)

      insp(co(i) cargs,r,cases) a args -> cases(i) cargs a args


   4. Fixpoint expansion:

      fix(i,fp) args1 (co(i) cargs) args2 -> t(i) args1 (co(i) cargs) args2

      where t(i) is the term of the fixpoint i with all fixpoint variables j
      substituted by fix(j,fp).


   5. insp(e,r,cases) args -> insp(f,r,cases) args

      where e -> f


   6. fix(i,fp) args1 a args2 -> fix(i,fp) args1 b args2

      where a -> b and a is decreasing argument for the fixpoint i

 *)


let rec key_normal0 (f:Term.t) (args: Term.t list) (c:Gamma.t)
        : Term.t * Term.t list =
  (* Compute the key normal form of [f args] i.e. [key_normal0 f args1 c = key
     args2] where [f args1 ~>* key args2] and [key] cannot be key reduced
     anymore. *)
  let key = ref f
  and args = ref args
  and go_on = ref true
  in
  let open Term in
  while !go_on do
    match !key, !args with

    | Sort _, [] ->
       go_on := false

    | Sort s, _ :: _ ->
       assert false (* sorts cannot be applied *)

    | Variable i, hd :: tl when Gamma.has_definition i c ->
       key := Gamma.definition i c

    | Variable _, _ ->
       go_on := false

    | Application (f, a, _), _ ->
       key := f;
       args := a :: !args

    | Lambda _, [] ->
       go_on := false

    | Lambda (_, tp, t), hd :: tl ->
       key := substitute t hd;
       args := tl

    | All _, [] ->
       go_on := false

    | All _, _ :: _ ->
       assert false (* product cannot be applied *)

    | Inspect _, [] ->
       (* ????? *)
       go_on := false

    | Inspect (e, res, cases), _ :: _ ->

       let e, eargs = key_normal0 e [] c
       in
       begin
         match e with

         | Variable i when Gamma.is_constructor i c ->
            key := Term.case_definition cases.(Gamma.constructor_offset i c);
            args := eargs @ !args (* Parameters ???? *)

         | _ ->
            key := Inspect (apply_args e eargs, res, cases);
            go_on := false
       end

    | Fix _, [] ->
       go_on := false

    | Fix (i, fp), _ :: _ ->

       let _,_,decr,_ = fp.(i) in
       let args1_rev,args2 =
         Mylist.split_condition (fun i _ -> i = decr) !args
       in

       begin
         match args2 with

         | [] ->
            go_on := false

         | hd :: tl ->
            let co,coargs = key_normal0 hd [] c in

            match co with

            | Variable j when Gamma.is_constructor j c ->
               (* reduce fixpoint *)
               key := Term.reduce_fixpoint i fp;
               args := List.rev args1_rev @ (apply_args co coargs) :: args2

            | _ ->
               (* no reduction, but reinsert key normal form of decreasing
                  argument. *)
               args := List.rev args1_rev @ (apply_args co coargs) :: args2;
               go_on := false
       end
  done;
  !key, !args




let key_normal (t:Term.t) (c:Gamma.t): Term.t * Term.t list =
  (* Compute the key normal form of [t] i.e. [key_normal t c = key args] where
     [t ~>* key args] and [key] cannot be key reduced anymore. *)
  key_normal0 t [] c


let key_normal_term (t:Term.t) (c:Gamma.t): Term.t =
  let key,args = key_normal0 t [] c in
  Term.apply_args key args







(* ==========================================================

   Equivalence of Terms

   ==========================================================

   Two terms are equivalent if they have the same normal form.
 *)


let rec equivalent (a:Term.t) (b:Term.t) (c:Gamma.t): bool =
  (* Are [a] and [b] equivalent (i.e. convertible) in the context [c]?
     Assume that both terms are wellformed. *)
  let ka,argsa = key_normal a c in
  let kb,argsb = key_normal b c in
  equivalent_key ka kb c && equivalent_arguments argsa argsb c


and equivalent_key (a:Term.t) (b:Term.t) (c:Gamma.t): bool =
  (* Are [a] and [b] equivalent as keys of an application in key normal form
     in the context [c]? Assume that both terms are wellformed. *)
  let open Term in
  match a, b with

  | Sort sa, Sort sb ->
     Sorts.equal sa sb

  | Variable i,  Variable j ->
     i = j

  | Lambda (nme,tpa,ta), Lambda (_,tpb,tb) when equivalent tpa tpb c ->
     equivalent ta tb (Gamma.push_simple nme tpa c)

  | All (nme,tpa,ta), All (_,tpb,tb) when equivalent tpa tpb c ->
     equivalent ta tb (Gamma.push_simple nme tpa c)

  | Inspect (ea,pa,casesa), Inspect (eb,pb,casesb)
       when equivalent ea eb c && equivalent pa pa c ->
     let ncases = Array.length casesa in
     assert(ncases <> Array.length casesb);
     interval_for_all
       (fun i ->
         let fa = case_definition casesa.(i)
         and fb = case_definition casesb.(i)
         in
         equivalent fa fb c)
       0 ncases

  | Fix (i1, fp1), Fix (i2, fp2)
       when i1 = i2 && Array.length fp1 = Array.length fp2 ->
     let len = Array.length fp1 in
     (* Corresponding types must be equivalent and corresponding terms must
        decrease on the same argument. *)
     interval_for_all
       (fun i ->
         let _,tp1,decr1,_ = fp1.(i)
         and _,tp2,decr2,_ = fp2.(i)
         in
         decr1 = decr2
         && equivalent tp1 tp2 c
       )
       0 len
     && (* Corresponding terms must be equivalent *)
       let c_inner = Gamma.push_fixpoint fp1 c in
       interval_for_all
         (fun i ->
           let _,_,_,t1 = fp1.(i)
           and _,_,_,t2 = fp2.(i)
           in
           equivalent t1 t2 c_inner
         )
         0 len

  (* default case: different or incompatible constructors *)
  | _ ->
     false


and equivalent_arguments (argsa: Term.t list) (argsb:Term.t list) (c:Gamma.t)
    : bool =
  match argsa, argsb with
  | [], [] ->
     true
  | a :: argsa, b :: argsb ->
     equivalent a b c && equivalent_arguments argsa argsb c
  | _ ->
     false









(* ==========================================================

   Subtyping

   ==========================================================
 *)


let rec is_subtype (a:Term.typ) (b:Term.typ) (c:Gamma.t): bool =
  (* Is [a] a subtype of [b] in the context [c]. Assume that both are
     wellformed.  *)
  let ka,argsa = key_normal a c in
  let kb,argsb = key_normal b c in
  let open Term in
  match ka, kb with
  | Sort sa, Sort sb ->
     Sorts.sub sa sb
  | All (nme,tpa,ta), All(_,tpb,tb) ->
     equivalent tpa tpb c
     && is_subtype ta tb (Gamma.push_simple nme tpa c)
  | _ ->
     equivalent_key ka kb c && equivalent_arguments argsa argsb c







(* ===================================================================

   Decreasing Fixpoints

   ===================================================================


   A recursive call must be protected by one ore more pattern matches of the
   decreasing argument.

   The decreasing argument of the fixpoint term must be of an inductive type
   of a family. A constructor argument is a recursive argument if its type
   belongs to the same family as the constructor.

   In a pattern match [inspect(x,r,[f1,f2,...])] each recursive argument of
   any fi is structurally smaller than x. We expect the inspected expression
   to be a variable.

   The inspect expressions surrounding a recursive call gives the set of all
   variables structurally smaller than the decreasing argument of the fixpoint
   expression.

   For any recursive call [y args] we get the argument expression
   corresponding to the decreasing argument of [y]. This argument expression
   must be structurally smaller than the decreasing formal argument of the
   current fixpoint expression.

   In Coq it is possible to compute a structurally smaller elements of the
   original argument. A smaller accessibility proof term can be computed from
   an acessibility proof term by giving a predecessor in the corresponding
   relation. We do not use this possibility for the time being.

   Having the argument expression of the decreasing argument of the recursive
   call, the key normal form of this expression must have one of the
   structurally smaller variables in the key (i.e. function) position.

   The algorithm holds a set of structurally smaller variables (use De Bruijn
   levels to describe them in order to keep them invariant for the different
   contexts) during the term iteration. Whenever it encounters a recursive
   call it reduces the argument expression to key normal form and searches for
   the key variable in the set of structurally smaller variables.


   Nesting Case:

   An inductive family might use another inductive type (not mutually defined)
   within the family. This happens if the other type has one positive
   parameter which is instantiated with a type containing one type of the
   family. E.g. [List(Tree(A))].

  *)

let check_fixpoint_decreasing
      (t:Term.t) (fp:Term.fixpoint)
      (nargs:int) (decr:int) (c:Gamma.t)
    : unit option =
  (* Check that the component term [t] of the fixpoint array [fp] with [nargs]
     arguments is decreasing on the [decr] argument. The context [c] contains
     all types of the fixpoints and all arguments of the current fixpoint.

     - At least one branch calls another function j of the fixpoint array.

     - A recursive call has the form [(Var j) args b ...] where [b] is the
     decreasing argument of fixpoint j and b is a part of the argument [decr]
     of the current fixpoint.

     Remark:

     - [t] is not an abstraction, all abstractions are already pushed into the
     context.  *)
  assert (decr < nargs);

  let decr_level = (* level of decreasing argument *)
    Gamma.count c - nargs + decr

  and fixpoint_start = (* level of first fixpoint *)
    Gamma.count c - nargs - Array.length fp

  and fixpoint_beyond = (* level beyond last fixpoint *)
    Gamma.count c - nargs
  in

  let decrementing_argument i c =
    (* What is the decrementing argument of a recursive call with variable [i]
       in the function position? Return [None] if variable [i] does not
       represent a component of the fixpoint [fp]. *)
    let level = Gamma.to_level i c in
    if fixpoint_start <= level && level < fixpoint_beyond then
      let _,_,decr,_ = fp.(fixpoint_beyond - level - 1) in
      Some decr
    else
      None

  and can_generate_smaller i smaller c =
    let level = Gamma.to_level i c in
    level = decr_level || IntSet.mem level smaller

  and add_recursive_arguments i j smaller c =
    (* add recursive arguments of the constructor [j] of the inductive type of
       variable [i] to the set of structurally smaller variables. The
       constructor arguments are not yet pushed into the context. Therefore
       the level starts at [Gamma.count c]. *)
    let tp = Gamma.entry_type i c
    and cnt = Gamma.count c
    in
    let key,args = key_normal tp c
    in
    match key with
    | Term.Variable ivar ->
       let _,smaller =
         List.fold_left
           (fun (k,smaller) cls ->
             let open Inductive in
             match cls with
             | Normal ->
                k+1, smaller
             | Recursive ->
                k+1, IntSet.add (cnt + k) smaller
             | Positive ->
                (* Argument [k] of the constructor has a positive parameter as
                   its final type. It is potentially recursive. *)
                k+1,smaller (* nyi: Missing check of nested inductive types *)
           )
           (0,smaller)
           (Gamma.constructor_arguments ivar j c)
       in
       smaller
    | _ ->
       assert false (* Illegal call: [Variable i] is either the decreasing
                       argument or it is in the list of structurally smaller
                       variables. Therefore its type must be inductive. *)
  in

  let rec check_recursive_calls
            (t:Term.t) (smaller:IntSet.t) (c:Gamma.t) (n:int)
          : int option =
    (* Check all recursive calls in the term [t]. Return the number of valid
       recursive calls or None if there are illegal calls. *)

    let check_with_lambda t smaller c n =
      let t1, c1 = Gamma.push_lambda t c in
      check_recursive_calls t1 smaller c1 n
    in

    let check_list lst n =
      ListMOption.fold_left
        (fun t n -> check_recursive_calls t smaller c n)
        lst n
    in

    let key,args = key_normal t c
    in
    let open Term in
    match key with
    | Sort s ->
       assert (args = []); (* sorts cannot be applied *)
       Some n

    | Variable i ->
       begin
         match decrementing_argument i c with

         | Some decr_j ->
            (* [key args] is a recursive call *)
            let args1_rev,args2 =
              Mylist.split_condition (fun k _ -> k = decr_j) args in
            begin
              match args2 with
              | [] ->
                 (* too few arguments in the recursive call *)
                 None
              | hd :: tl ->
                 let hd_key,_ = key_normal hd c
                 in
                 match hd_key with
                 | Variable i_hd ->
                    if IntSet.mem (Gamma.to_level i_hd c) smaller then
                      (* The argument [hd] is structurally smaller. Now it
                         remains to check the other arguments beside [hd]. The
                         arguments of [hd_key] need not be checked, because
                         the positivity condition guarantees that they do not
                         contain any inductive objects of the same inductive
                         family. *)
                      Option.(check_list args1_rev (n+1)
                              >>= check_list tl)
                    else
                      (* [Variable i_hd] is not structurally smaller *)
                      None
                 | _ ->
                    (* [hd] cannot be structurally smaller *)
                    None
            end

         | None ->
            (* We are not in a recursive call. Therefore check arguments
               only. *)
            check_list args n
       end

    | Application _ ->
       assert false (* key cannot be an application *)

    | Lambda (nme, tp, t) ->
       assert (args = []); (* otherwise [key args] would have a key redex *)
       check_recursive_calls t smaller (Gamma.push_simple nme tp c) n

    | All (nme, tp, t) ->
       assert (args = []); (* product cannot be applied *)
       check_recursive_calls t smaller (Gamma.push_simple nme tp c) n

    | Inspect (Variable i, res, cases)
         when can_generate_smaller i smaller c ->
       Option.(
        fold_array
          (fun n case j ->
            let smaller = add_recursive_arguments i j smaller c in
            check_with_lambda (case_definition case) smaller c n
          )
          n cases
       )

    | Inspect (e, res, cases) ->
       Option.(
        check_recursive_calls e smaller c n >>= fun n ->
        fold_array
          (fun n case j ->
            check_with_lambda (case_definition case) smaller c n
          )
          n cases
       )

    | Fix (i_inner, fp_inner) ->
       (* The fixpoint array [fp2] is not interesting per se, but some of its
          components might have recursive calls to some component of the outer
          fixpoint. *)
       let c1 = Gamma.push_fixpoint fp_inner c
       in
       Option.fold_array
         (fun n comp _ ->
           let _,_,_,t = comp in
           check_recursive_calls t smaller c1 n)
         n fp_inner
  in

  Option.(
    check_recursive_calls t IntSet.empty c 0 >>= fun n ->
    if n = 0 then
      printf "fixpoint element does not contain a recursive call\n%s\n"
        (string_of_term c t);
    of_bool (0 < n)
  )












(* ===================================================================

   Type Checking

   ===================================================================

  A collection of mutually recursive functions.

*)

let rec type_of (t:Term.t) (c:Gamma.t): Term.typ option =
  (* Return the type of [t] in the context [c] if it is wellformed. *)
  let open Term in
  match t with
  | Sort s ->
     Option.(Sorts.type_of s >>= fun s -> Some (Sort s))

  | Variable i ->
     if  i < Gamma.count c then
       Some (Gamma.entry_type i c)
     else
       begin
         printf "variable (%d/%d) out of bounds\n" i (Gamma.count c);
         None
       end

  | Application (f,a,_) ->
     (* Does the type of [a] fit the argument type of [f]? *)
     Option.(
      type_of f c >>= fun ftp ->
      type_of a c >>= fun atp ->
      (* Now f,ftp and a,atp are wellformed *)
      let hn = key_normal_term ftp c
      in
      match hn with

      | All (_, tp, res) (* tp, res are wellformed *)
           when is_subtype atp tp c  ->
         Some (Term.substitute res a)

      | All (_,tp,res) ->
         printf "application failed 'f:%s', 'a:%s'\n  %s is no subtype of %s\n"
           (string_of_term c ftp)
           (string_of_term c atp)
           (string_of_term c atp)
           (string_of_term c tp);
         None

      | _ ->
         None
     )
  | Lambda (nme,tp,t) ->
     (* check:
        - Is [tp] a wellformed type?
        - Is [t] a wellformed term in the context [c,tp]?
        - Is the corresponding product wellformed? *)
     Option.(
      type_of tp c >>= fun s ->
      Term.get_sort s >>= fun _ ->
      type_of t (Gamma.push_simple nme tp c) >>= fun ttp ->
      let lam_tp = All (nme,tp,ttp) in
      type_of lam_tp c >>= fun _ ->
      Some lam_tp
     )
  | All (nme,arg_tp,res_tp) ->
     (* check:
        - Is [arg_tp] must be a wellformed type?
        - Is [res_tp] must be a wellformed type in the context with [c,arg_tp]?

        The sorts of [arg_tp] and [res_tp] determine the sort of the quantified
        expression. *)
     Option.(
      let open Term in
      type_of arg_tp c >>= fun arg_s ->
      type_of res_tp (Gamma.push_simple nme arg_tp c) >>= fun res_s ->
      product arg_s res_s
     )
  | Inspect (e,res,cases) ->
     type_of_inspect e res cases c
  | Fix (idx, arr) ->
     if idx < 0 || Array.length arr <= idx then
       None
     else
       Option.(
       check_fixpoint arr c >>= fun _ ->
       let _,typ,_,_ = arr.(idx) in
       Some typ)


and type_of_inspect
(e:Term.t) (res:Term.t) (cases: Term.case array) (c:Gamma.t)
    : Term.typ option =
  (* inspect(e,res,cases)

     1. Compute head normal form of the type of e: I p a
        I must be an inductive type with nparams parameter. Therefore
        parameters and arguments can be extracted.

     2. Compute the type of res(a,e). inspect(e,res,cases): res(a,e).
        If I has an elimination restriction, then the sort of res(a,e) must
        be Proposition.

     3. |cases| = number of constructors of I.

     4. For all 0 <= j < |cases|:

        Compute type of cj(p) where cj is the j-th constructor of I and p are
        the parameters.
            cj(p): all(t:T) I p aj.

        Compute the type of fj = snd cases.(j).
            fj: all(t:T) res(aj,cj(p,t))

   *)
  Option.(
    type_of e c >>= fun e_tp ->
    check_inductive e_tp c >>= fun (ivar,params,args,ind,ind_idx) ->

    (* The term [res(args,e)] is the result type of the expression, its sort
       must satisfy the potential elimination restrictions of [I]. *)
    let res_tp = Term.apply1 (Term.apply_args res args) e in
    type_of res_tp c >>= fun res_s ->
    if Inductive.is_restricted ind_idx ind && res_s <> Term.proposition then
      None
    else if Inductive.nconstructors ind_idx ind <> Array.length cases then
      None
    else if
      interval_for_all
        (fun j ->
          let cj, fj = Term.case_pair cases.(j) in
          let cj0 =
            Term.apply_args
              (Term.Variable (Gamma.constructor_of_inductive_variable j ivar c))
              params in
          None <>
            (type_of cj0 c >>= fun _ ->
             type_of cj  c >>= fun cj_tp ->
             check_case cj cj_tp fj res c
            )
        )
        0 (Array.length cases)
    then
      Some res_tp
    else
      None
  )


and check_inductive (tp:Term.typ) (c:Gamma.t)
    : (Term.t           (* I *)
       * Term.t list    (* params *)
       * Term.t list    (* args *)
       * Inductive.t    (* inductive structure *)
       * int            (* position in the family *)
      ) option =
  (* Check if [tp] is an inductive type. In case of yes, return the variable
     representing the inductive type, the parameter and arguments such that [I
     params args] is the key normal form of [tp], the inductive structure and
     the position of the inductive type in its family. *)
  Option.(
    let ivar,args = key_normal tp c in
    Gamma.inductive_index ivar c >>= fun (ind_idx,ind) ->
    let nparams = Inductive.nparams ind in
    let params,args = Mylist.split_at nparams args in
    Some (ivar,params,args,ind,ind_idx)
  )


and check_case
(cj:Term.t) (* fst cases.(j) *)
(cj_tp:Term.typ) (* type of cj *)
(fj:Term.t) (* snd cases.(j) *)
(res:Term.t)
(c:Gamma.t)
    : unit option =
  (* - cp and cj must have an equivalent type
     - cj: all(t:T) I p aj, fj: all(t:T) res(aj,cj(t))
   *)
  let rec split_product c_tp f_tp i c =
    match c_tp, f_tp with
    | Term.All(nm1,tp1,c_tp), Term.All(_,tp2,f_tp) when equivalent tp1 tp2 c ->
       split_product c_tp f_tp (i+1) (Gamma.push_simple nm1 tp1 c)
    | _ ->
       Some (c_tp,f_tp,i,c)
  in
  Option.(
    type_of fj c >>= fun fj_tp ->
    split_product cj_tp fj_tp 0 c >>= fun (i_tp,res_tp,n,c1) ->
    check_inductive i_tp c1 >>= fun (ivar,params,args,ind,ind_idx) ->
    let res_tp_req =
      Term.apply1
        (Term.apply_args (Term.up n res) args)
        (Term.apply_standard n 0 (Term.up n cj))
    in
    type_of res_tp_req c1 >>= fun _ ->
    if equivalent res_tp_req res_tp c1 then
      Some ()
    else
      None
  )

and check_fixpoint (fp:Term.fixpoint) (c:Gamma.t): unit option =
  (* - All types must be valid in the current environment
     - All terms must have the corresponding type
     - All recursive calls must be with a structurally decreasing argument *)
  Option.(
    Option.of_bool
      (Array.for_all (fun (_,tp,_,_) -> type_of tp c <> None) fp) >>= fun _ ->
    let c1 = Gamma.push_fixpoint fp c in
    let len = Array.length fp in
    (* c1 contains all types of the [len] fixpoints. *)
    (* Check all fixpoints that they have the correct type and are
          structurally decreasing. *)
    interval_fold
      (fun _ i ->
        let nme,typ,decr,t = fp.(i) in
        type_of t c1 >>= fun tp ->
        Option.of_bool
          (equivalent tp (Term.up len typ) c1) >>= fun _ ->
        (* term of [fp.(i)] has the correct type. *)
        let t2,c2 = Gamma.push_lambda t c1 in
        let nargs = Gamma.count c2 - Gamma.count c1 in
        Option.of_bool (decr < nargs) (* are there sufficient arguments?
                                       *)
        >>= fun _ ->
        check_inductive
          (Gamma.entry_type (nargs - decr - 1) c2) c2
        >>= fun (_,_,_,ind,_) ->
        check_fixpoint_decreasing t2 fp nargs decr c2
      )
      (Some ()) 0 len
  )



let type_and_sort_of (t:Term.t) (c:Gamma.t): (Term.typ*Term.typ) option =
  Option.(
    type_of t c >>= fun tp ->
    type_of tp c >>= fun s ->
    Some (tp,s)
  )


let sort_of (tp:Term.t) (c:Gamma.t): Sorts.t option =
  Option.(
    type_of tp c >>= fun tptp ->
    let key,_ = key_normal tptp c in
    match key with
    | Term.Sort s ->
       Some s
    | _ ->
       None
  )

let is_wellformed (t:Term.t) (c:Gamma.t): bool =
  type_of t c <> None


let is_wellformed_type (tp:Term.t) (c:Gamma.t): bool =
  Option.(
    type_of tp c >>= fun s ->
    Term.get_sort s
  ) <> None








(* ===================================================================

   Checking of the Definition of Inductive Types

   ===================================================================

   The following functions check if an inductive definition [ind] is valid and
   can be added to the context.

*)


(* ==== Positivity check of constructor types ==== *)
let check_constructor_type
      (tp:Term.typ)  (* constructor type *)
      (argcls:Inductive.carg_class list) (* classification of arguments:
                                            Normal, Positive or Recursive *)
      (i_start:int)  (* level of the first inductive type *)
      (i_current:int)(* level of the current inductive type *)
      (i_beyond:int) (* level beyond the last inductive type *)
      (parpos:bool array) (* parpos.(i) = parameter i is positive *)
      (allprop:bool) (* all constructor arguments must be propositions *)
      (no_any:bool)  (* no constructor args can be [Any]. *)
      (c:Gamma.t)
    : unit option =
  printf "  check constructor type %s (nargs %d)\n"
    (string_of_term c tp)
    (List.length argcls);
  (* Check if [tp] is a valid constructor type for the inductive type
     [i_current] in the context [c] which contains all absolute inductive types
     and the parameters.

     Assume that [tp] is a wellformed type.

     A valid constructor type constructs an object of the inductive type
     [i_current] which is a type of the family and it uses in its arguments
     other objects of the inductive family only positively i.e. only already
     constructed objects.  *)
  assert (i_start <= i_current);
  assert (i_current < i_beyond);
  let par_beyond = i_beyond + Array.length parpos
  in
  let is_of_family c i =
    let li = Gamma.to_level i c in
    i_start <= li && li < i_beyond
  and is_positive_parameter c i =
    let li = Gamma.to_level i c in
    i_beyond <= li
    && li < par_beyond
    && parpos.(li - i_beyond)
  in
  let is_positive c i =
    is_of_family c i || is_positive_parameter c i
  in
  let has_of_positive c t =
    Term.has_variables (is_positive c) t
  and has_of_family c t =
    Term.has_variables (is_of_family c) t
  in
  let rec check_constructor_type tp argcls c =
    (* Check all argument types and the final type of [tp]. *)

    let key,args = key_normal tp c in

    let open Term in
    match key, args, argcls with

    | Sort s, [], []  ->
       None (* A sort cannot be a constructor type *)

    | Variable i, _, [] ->
       if i_current = Gamma.to_level i c then
         (* [key args] has the form [I args] where [I] represents the current
            inductive type of the family. Since [key args] is wellformed it
            has the form [I params iargs]. *)
         Some ()
       else
         (* An object of the current inductive type is not constructed. *)
         None

    | All (nme, arg, tp), [], cls :: argcls ->
       Option.(
        if allprop && sort_of arg c <> Some Sorts.Proposition then
          None
        else if no_any
                && match sort_of arg c with
                   | Some Sorts.Box ->
                      true
                   | _ ->
                      false
        then
          (printf "  failed predicativity\n";None)
        else
          check_constructor_argument arg cls c >>= fun _ ->
          check_constructor_type tp argcls (Gamma.push_simple nme arg c)
       )
    | _, _, _ ->
       assert false (* Illegal call: The type [tp] is not wellformed. *)

  and check_constructor_argument tp cls c =
    (* [tp] is the type of an argument of the constructor. If it is a function
       type, the argument types of the function must not contain any inductive
       type of the family nor any positive parameter. For [cls = Normal] the
       final type must not contain a positive parameter nor an inductive type
       of the family. For [cls = Recursive] the final type must contain an
       inductive type of the family. For [cls = Positive] the final type must
       contain a positive parameter. *)
    printf "    check constructor argument (%s) %s\n"
      (let open Inductive in
       match cls with
       | Normal -> "Normal"
       | Recursive -> "Recursive"
       | Positive  -> "Positive")
      (string_of_term c tp);

    let key,args = key_normal tp c in

    let open Term in
    match key, args with

    | Sort s, [] ->
       (* No inductive type of the family contained. *)
       Some ()

    | Variable i, _ ->
       (* [key args] is a final type of the constructor type. *)
       begin
         printf "    check argument final type %s\n"
           (string_of_term c (Term.apply_args key args));
         match cls with
         | Inductive.Normal
              when not (is_positive c i)
                   && not (List.exists (has_of_positive c) args) ->
            (* No inductive type of the family and no positive parameter is
               involved, neither in the key nor in the arguments. *)
            Some ()

         | Inductive.Recursive when is_of_family c i ->
            Some ()

         | Inductive.Recursive ->
            (* Nesting case *)
            printf "    check nesting %s\n"
              (string_of_term c (Term.apply_args key args));
            Option.(
              Gamma.inductive_family i c >>= fun (ith,ind) ->
              of_bool (Inductive.ntypes ind = 1) >>= fun _ ->
              let pos_params = Inductive.positive_parameters ind
              and np = Inductive.nparams ind
              in
              ListMOption.foldi_left
                (fun pos arg n ->
                  if has_of_family c arg then
                    (* Check that the argument at position [pos] is a positive
                       parameter of the nested inductive type. *)
                    if pos < np && pos_params.(pos) then
                      Some (n+1)
                    else
                      None
                  else
                    Some n
                )
                args 0
              >>= fun n ->
              of_bool (n > 0)
            )

         | Inductive.Positive
              when is_positive_parameter c i  ->
            Some ()

         | _ ->
            None
       end

    | All (nme, arg, tp), [] ->
       (* The argument type is a function type. None of the argument types of
          the function is allowed to contain an inductive type of the
          family nor a positive parameter. *)
       printf "    function arg %s\n" (string_of_term c arg);
       if has_of_positive c arg then
         (printf "  strict positivity failed for %s\n"
           (string_of_term c arg);
         None)
       else
         check_constructor_argument tp cls (Gamma.push_simple nme arg c)

    | _, _ ->
       assert false (* Illegal call: The argument type [tp] is not
                       wellformed. *)

  in
  check_constructor_type tp argcls c




let check_arity (tp:Term.typ) (c:Gamma.t): Sorts.t option =
  (* Check if [tp] is an arity, i.e. normalized it looks like [all(....) s]
     for some sort [s]. Return the sort or [None] if the term [tp] is not
     wellformed or not an arity. *)
  if is_wellformed tp c then
    let rec check t c =
      let key,args = key_normal t c in
      let open Term in
      match key,args with
      | Sort s, [] ->
         Some s
      | All(nme,tp,t), [] ->
         check t (Gamma.push_simple nme tp c)
      | _ ->
         None
    in
    check tp c
  else
    None




(* Check if [ind] is a valid inductive definition. *)
let check_inductive_definition (ind:Inductive.t) (c:Gamma.t)
    : Inductive.t option =
  printf "check inductive definition\n";

  (* Are all parameter types are valid in the outer context? *)
  let check_parameter (c:Gamma.t): Gamma.t option =
    printf "check parameter\n";
    Option.fold_array
      (fun c (nme,tp) i ->
        if is_wellformed tp c then
          Some (Gamma.push_simple nme tp c)
        else
          None
      )
      c (Inductive.params0 ind)
  in


  (* Are all inductive types arities of some sort?

     - All types must be valid and must be types (i.e. their type is some
     sort)

     - The normalized types must look like [all(a:A,b:B,...) s] for some sort
     [s]. The sort [s] is not the sort of the complete type. Even if the
     complete type is a type, [s] is not necessarily a sort. It could be a
     type as well. *)
  let check_types (c:Gamma.t): unit option =
    (* [c] is a context with the parameters. *)
    printf "check types\n";
    Option.(
      fold_array
        (fun _ (_,tp) ith ->
          printf "  check arity %s\n" (string_of_term c tp);
          check_arity tp c >>= fun s ->
          of_bool ( Inductive.sort ith ind = s
                    && Inductive.is_restriction_consistent ith ind)
        )
        () (Inductive.types0 ind)
    )
  in
  let check_constructors (c:Gamma.t): unit option =
    let ni = Inductive.ntypes ind in
    let i_start = Gamma.count c in
    let i_beyond = i_start + ni in
    let parpos = Inductive.positive_parameters ind in
    let cc = Gamma.push_ind_types_params ind c in
    Option.fold_interval
      ((fun _ ith ->
        printf "check constructors of %s\n"
          (string_of_term
             cc
             (Term.Variable (Gamma.to_index (i_start + ith) cc)));
        let allprop = Inductive.all_args_propositions ith ind
        and no_any  = Inductive.sort ith ind = Sorts.Any
        in
        Option.fold_array
          (fun _ (nme,c_type,argcls) _ ->
            Option.(
             printf "check constructor type\n";
             type_of c_type cc (* [c_type] is wellformed.*)
             >>= fun ctptp ->
             printf "  ok %s : %s\n"
               (string_of_term cc c_type) (string_of_term cc ctptp);
             check_constructor_type
               c_type argcls i_start (i_start + ith) i_beyond
               parpos allprop no_any cc
            )
          )
          ()
          (Inductive.constructors ith ind)
      ): unit -> int -> unit option)
      () 0 ni
  in
  Option.(
    check_parameter c >>= fun cp ->
    check_types cp
    >>= fun _ ->
    check_constructors c
    >>= fun _ ->
    Some ind
  )










(* ============================================================

   Some Builtin Types and Function

   ============================================================
 *)


(* Predicate (A:Any): Any := A -> Proposition *)
let predicate: Gamma.Definition.t =
  let open Term in
  let t = Lambda (Some "A", any, arrow variable0 proposition)
  and typ = All (Some "A", any, any) in
  Gamma.Definition.make_simple (Some "Predicate") typ t


(* Relation (A,B:Any): Any := A -> Proposition *)
let binary_relation: Gamma.Definition.t =
  let open Term in
  let t = Lambda
            (Some "A",any,
             Lambda
               (Some "B",any,
                arrow variable1 (arrow variable0 proposition)))
  and typ = All
              (Some "A",
               any,
               All
                 (Some "B",
                  any,
                  any))
  in
  Gamma.Definition.make_simple (Some "Relation") typ t


(* Endo_relation (A:Any): Any := Relation(A,A) *)
let endorelation (rel_idx:int): Gamma.Definition.t =
  let open Term in
  let t =
    Lambda
      (Some "A",
       any,
       apply_args (Variable (rel_idx+1)) [variable0;variable0])
  and typ =
    All
      (Some "A", any, any)
  in
  Gamma.Definition.make_simple (Some "Endorelation") typ t

(* (->) (A,B:Any): Any :=
     all(_:A) B

   (=>) (A,B:Proposition): Proposition :=
     all(_:A) B
 *)

let generic_arrow_definition (sa:string) (sb:string) (op:Operator.t) (s:Term.t)
    : Gamma.Definition.t =
  let open Term in
  let a = variable0
  and b = variable1
  in
  let args = [| Some sa, s; Some sb, s |] in
  let typ =
    push_product args s
  and term =
    push_lambda
      args
      (All (None, a, b))
  in
  Gamma.Definition.make
    (some_feature_operator op)
    (typ  |> to_index 0)
    (term |> to_index 0)


let fat_arrow_definition: Gamma.Definition.t =
  generic_arrow_definition
    "a" "b"
    Operator.fat_arrow
    Term.proposition

let arrow_definition: Gamma.Definition.t =
  generic_arrow_definition
    "A" "B"
    Operator.arrow
    Term.any



(* pred0 (a:Natural): Natural :=
        inspect
            a
        case
            0 := 0
            n.successor := n
        end
 *)
let nat_pred0 (nat_idx:int): Gamma.Definition.t =
  assert (2 <= nat_idx);
  let open Term in
  let nat = 0
  and base = nat_idx + 1
  in
  let nat_tp = Variable nat
  and zero = Variable (nat + 1)
  and succ = Variable (nat + 2)
  and a = Variable base
  and n = Variable (base + 1)
  in
  let nme = some_feature_name "predecessor"
  and typ = push_product [|Some "a", nat_tp|] nat_tp
  in
  let t =
    push_lambda
      [|Some "a", nat_tp|]
      (Inspect (a,
                Lambda (None, nat_tp, nat_tp),
                [|
                  make_case [||] zero zero;
                  make_case
                    [|Some "n",nat_tp|]
                    (apply_target succ n)
                    (n)
                |]
         )
      )
  in
  Gamma.Definition.make nme (typ |> to_index base) (t |> to_index base)





(* (+)(a,b:Natural): Natural :=
       inspect a case
           0 := b
           n.successor := n + b.successor
       end
 *)
let nat_add_fp (nat_idx:int): Term.fixpoint =
  assert (2 <= nat_idx);
  let open Term in
  let nat = 0
  and base = nat_idx + 1
  in
  let nat_tp = Variable nat
  and zero = Variable (nat + 1)
  and succ = Variable (nat + 2)
  and plus = Variable base
  and a = Variable (base + 1)
  and b = Variable (base + 2)
  and n = Variable (base + 3)
  in
  let nme = some_feature_operator Operator.plus
  and typ =
    push_product
      [|Some "a",nat_tp; Some "b", nat_tp|]
      nat_tp
  in
  let t =
    push_lambda
      [|Some "a",nat_tp; Some "b", nat_tp|]
      (
        Inspect (
            a,
            Lambda (None, nat_tp, nat_tp),
            [|make_case [||] zero b;

              make_case
                [|Some "n", nat_tp|]
                (apply_target succ n)
                (binary plus n (apply_target succ b))
            |]
          )
      )
  in
  [|nme, typ |> to_index base, 0, t |> to_index (base + 1)|]









(* ============================================================

   Unit Tests

   ============================================================
 *)

let print_type_of (t:Term.t) (c:Gamma.t): unit =
  match type_of t c with
  | None ->
     printf "not wellformed %s\n" (string_of_term c t);
  | Some tp ->
     printf "%s : %s\n" (string_of_term c t) (string_of_term c tp)

let test (): unit =
  let equal_opt a b =
    match a with
    | None ->
       false
    | Some a ->
       Term.equal a b
  in
  let equivalent_opt a b c =
    match a with
    | None ->
       false
    | Some a ->
       equivalent a b c
  in
  Printf.printf "test type checker\n";
  let open Term in

  let c = Gamma.push_unnamed any Gamma.empty
  in

  (* Proposition -> Proposition *)
  assert ( type_of (arrow proposition proposition) Gamma.empty = Some any);

  (* Natural -> Proposition *)
  assert ( type_of (arrow variable0 proposition) c = Some any);

  (* (Natural -> Proposition) -> Proposition *)
  assert ( type_of (arrow
                    (arrow variable0 proposition)
                    proposition) c = Some any);

  (* all(A:Prop) A -> A : Proposition *)
  assert ( product proposition proposition = Some proposition );
  assert ( product any proposition         = Some proposition );
  assert ( type_of (All (None,
                       proposition,
                       arrow variable0 variable0)) c
           = Some proposition);

  (* all(n:Natural) n -> n is illformed, n is not a type! *)
  assert ( type_of (All (None,
                         Variable 0,
                         arrow variable0 variable0)) c
           = None);

  (* Natural -> Natural : Any *)
  assert ( product any any = Some any );
  assert ( type_of (arrow variable0 variable0) c
           = Some any);

  (* ((a:Prop, p:a) := p): all(a:Prop) a => a *)
  assert ( type_of
             (Lambda (None,
                      proposition,
                      Lambda (None,
                              variable0,
                              variable0)
                ))
             c
           = Some (All (None,
                        proposition,
                        arrow variable0 variable0) )
    );

  (* All prover type:
         all(p:Proposition) p: Proposition
     is equivalent to false *)
  assert( type_of (All (Some "p", proposition, variable0)) Gamma.empty
          = Some proposition);

  (* All inhabitor type:
         all(A:Any) A  : Box
     is equivalent to false *)
  assert( type_of (All (Some "A", any, variable0)) c
          = Some box);

  assert (check_inductive_definition Inductive.make_natural Gamma.empty <> None);
  assert (check_inductive_definition Inductive.make_false Gamma.empty <> None);
  assert (check_inductive_definition Inductive.make_true Gamma.empty <> None);
  assert (check_inductive_definition Inductive.make_and Gamma.empty <> None);
  assert (check_inductive_definition Inductive.make_or Gamma.empty <> None);
  assert (check_inductive_definition Inductive.make_equal c <> None);
  assert (check_inductive_definition Inductive.make_list c <> None);
  assert (check_inductive_definition Inductive.make_accessible c <> None);
  assert (check_inductive_definition Inductive.make_exist c <> None);

  (* class Natural create 0; successor(Natural) end *)
  ignore(
      let ind = Inductive.make_natural
      in
      assert (check_inductive_definition ind Gamma.empty <> None);
      let c = Gamma.push_inductive ind Gamma.empty in
      assert (type_of variable2 c = Some any);
      assert (type_of variable1 c = Some variable2);
      assert (type_of variable0 c = Some (arrow variable2 variable2))
    );




  (* Failed positivity:
     class C create
         _ (f:C->C): C
     end
   *)
  assert
    begin
      let open Inductive in
      let ind =
        make_simple
          (some_feature_name "C")
          []
          any
          Sorts.Any
          false
          [cmake
             None
             [Recursive]
             (arrow (arrow variable0 variable0) variable0)]
      in
      check_inductive_definition ind Gamma.empty = None
    end;


  (* Check Tree *)
  ignore(
      let c =
        Gamma.push_inductive Inductive.make_list Gamma.empty
      in
      assert (Gamma.count c = 3);
      assert (
          check_inductive_definition
            (Inductive.make_tree 2)
            c
          <> None
        )
    );

  let check_definition (c:Gamma.t) (def:Gamma.Definition.t): unit =
    assert (equal_opt
              (type_of (Gamma.Definition.term def) c)
              (Gamma.Definition.typ def));
  in

  (* Arrow, Predicate, Relation *)
  check_definition Gamma.empty arrow_definition;
  check_definition Gamma.empty fat_arrow_definition;
  check_definition Gamma.empty predicate;
  check_definition Gamma.empty binary_relation;


  (* Endorelation *)
  begin
    let endo = endorelation 0 in
    let c =
      Gamma.push_definition binary_relation Gamma.empty
    in
    check_definition c endo;
    let c =
      Gamma.push_definition endo c
      |> Gamma.push_simple (Some "Natural") any in
    (* Endorelation(Natural) *)
    let endo_nat = apply1 variable1 variable0 in
    (*print_type_of endo_nat c;*)
    assert (is_wellformed endo_nat c)
  end;

  (* Predecessor and addition of natural numbers *)
  begin
    let ind = Inductive.make_natural
    in
    let c = Gamma.push_inductive ind Gamma.empty in
    let pred = nat_pred0 2 in
    printf "%s\n" (string_of_term c (Gamma.Definition.term pred));
    assert (is_wellformed (Gamma.Definition.term pred) c);
    assert (equivalent_opt
              (type_of (Gamma.Definition.term pred) c)
              (Gamma.Definition.typ pred)
              c);
    let plus_fp = nat_add_fp 2 in
    printf "%s\n" (string_of_fixpoint c plus_fp);
    assert (check_fixpoint plus_fp c <> None)
  end;
  ()
