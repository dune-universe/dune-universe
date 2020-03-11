open Alba2_common
open Container
open Printf

module Term = Term2

module type CONTEXT =
  sig
    type t
    val empty: t
    val push: Feature_name.t option -> Term.typ -> t -> t
    val push_simple: string option -> Term.typ -> t -> t
    val push_arguments: Term.arguments -> t -> t
    val push_fixpoint: Term.fixpoint -> t -> t
    val is_valid: int -> t -> bool
    val name: int -> t -> Feature_name.t option
    (*val shadow_level: int -> t -> int
    val type_: int -> t -> Term.*)
  end

module Raw_context =
  struct
    type t = unit
    let empty: t = ()
    let push (_:Feature_name.t option) (_:Term.typ) (c:t): t = c
    let push_simple (_:string option) (_:Term.typ) (c:t): t = c
    let push_arguments (args:Term.arguments) (c:t): t = c
    let push_fixpoint (fp:Term.fixpoint) (c:t): t = c
    let is_valid (i:int) (c:t): bool =
      true
    let name (i:int) (c:t): Feature_name.t option =
      some_feature_name (string_of_int i)
  end




(*
  Basics
  ------

  ((((f a) b) c) ... z)    ~>  f(a,b,c,...,z)

  all(a:A) all(b:B) ... t  ~>  all(a:A,b:B,...) t

  (a:A) := (b:B) := ... e  ~>  (a:A,b:B,...) := e


  Function term f can be a variable, lambda term, ...

  ((x,y) := exp)(a,b):
        exp                           let
          where                         x := a
             x := a                     y := b
             y := b                   then
          end                           exp

   derivable type arguments are supressed

   Fix(idx, (nm,tp,decr,term), ....)

      let mutual
        f1(args):tp := t
        f2(....) ...
        ...
      end then
        fi


  Parentheses:

  1) outer operator has higher precedence than inner operator

  2) same precedence, same left assoc and inner is right operand
     e.g. a + (b + c)

  3) same precedence, some right assoc and inner is left operand
     e.g. (a ^ b) ^ c

  4) same precedence, different assoc

  5) inspect, lambda and all have lower precedence than all operators (must be
     parenthesized if they occur as operand) but higher precedece than comma
     and colon.

  6) oo-dot binds stronger than application, application binds stronger than
     usual operators. 'tgt.f(args)' does not need '(tgt.f)(args)',
     'tgt.f(args) + ...' does not need parentheses. But '(+r)(a,b)' needs
     parentheses around '+r'.

  7) assignment binds stronger than comma i.e. f(...) := (a,b)  needs parentheses
     around 'a,b'.
 *)


(* ------------------------------------------------------------------

   Design

   ------------------------------------------------------------------

   Each subterm is converted to a triple (doc,prec,assoc option). Precedence
   is needed for all substerms, associativity only if the expression is a
   binary operator.

   The context is read only. A reader monad can be used to access the context.



   Application:

   a + b;  not b;  x.f(args);  f(args);   f

   binary, unary, oo_style, normal, only implicit arguments

   binary and unary: 2 or 1 essential arguments and (nice or no implicits)

   oo_style: Application_type = Target

   normal: First or f = Variable i



 *)

module Document = Pretty_printer2.Document

module type S =
  functor (C:CONTEXT) ->
  sig
    type context = C.t

    type level
    val compact: level
    val all_types: level
    val detailed: level

    val term: context -> level -> Term.t -> Document.t
    val fixpoint: context -> level -> Term.fixpoint -> Document.t
  end


module Make: S =
  functor (C:CONTEXT) ->
  struct
    type context = C.t

    type level = int

    let compact: level     = 0
    let outer_types: level = 1
    let all_types: level   = 2
    let detailed: level    = 3

    let with_implicits (l:level): bool =
      detailed = l

    let no_implicits (l:level): bool =
      l < detailed

    let with_types (l:level): bool =
      all_types <= l

    include Monad.Reader (struct type t = C.t*level end)

    type doc = Document.t

    type doc_plus = doc * Precedence.t

    type print_tp = Term.t -> doc_plus t


    let push_simple nme tp (c,l) =
      C.push_simple nme tp c, l

    let push_arguments args (c,l) =
      C.push_arguments args c, l

    let push_fixpoint fp (c,l) =
      C.push_fixpoint fp c, l

    let of_sort (s:Sorts.t): doc_plus t =
      let open Term in
      let open Document in
      let doc =
        match s with
        | Sorts.Proposition ->
           text "Proposition"
        | Sorts.Any ->
           text "Any"
        | Sorts.Box ->
           text "Box"
      in
      make (doc, Precedence.highest)

    let doc_of_simple_name (nme:string option) : doc =
      let open Document in
      match nme with
      | None ->
         text "_"
      | Some s ->
         text s


    let doc_of_name (nme:Feature_name.t option): doc =
      let open Feature_name in
      let open Document in
      match nme with
      | None ->
         text "_"
      | Some nme ->
         begin
           match nme with
           | Name s ->
              text s
           | Operator op ->
              text "("  ^  text (Operator.string op)  ^  text ")"
           | Bracket ->
              text "[]"
           | True ->
              text "true"
           | False ->
              text "false"
           | Number i ->
              text (string_of_int i)
         end


    let of_variable (i:int) (pr:print_tp): doc_plus t =
      ask >>= fun (c,_) ->
      let open Feature_name in
      let open Document in
      let vhash = "v#" in
      let doc =
        if C.is_valid i c then
          match C.name i c with
          | None ->
             text vhash ^ text (string_of_int i)
          | Some nme ->
             doc_of_name (Some nme)
        else
          text "(" ^ text vhash ^ text (string_of_int i) ^ text "?)"
      in
      make (doc, Precedence.highest)




    let parens (d:doc): doc =
      Document.bracket 2 "(" d ")"

    let left_parens
          (upper:Precedence.t)
          ((d,prec):doc_plus)
        : doc =
      if Precedence.left_needs_parens upper prec then
        parens d
      else
        d

    let right_parens
          (upper:Precedence.t)
          ((d,prec):doc_plus)
        : doc =
      if Precedence.right_needs_parens upper prec then
        parens d
      else
        d

    let lower_parens
          (upper:Precedence.t)
          ((d,prec):doc_plus)
        : doc =
      if Precedence.lower_needs_parens upper prec then
        parens d
      else
        d


    let actual_arguments (args:Term.t list) (pr:print_tp): doc_plus t =
      let prec = Precedence.argument_list
      in
      let pr_arg a =
        map
          (fun d ->
            lower_parens prec d, Precedence.highest)
          (pr a)
      in
      let rec arglist args =
        match args with
        | [] ->
           assert false (* Illegal call *)
        | [z] ->
           pr_arg z
        | a :: args ->
           pr_arg a >>= fun (a,_) ->
           arglist args >>= fun (args,_) ->
           make Document.(a ^ text "," ^ cut ^ args, prec)
      in
      arglist args >>= fun (d,_) ->
      make Document.(group d,prec)


    let normal_application
          (f:Term.t) (args:Term.t list) (n:int) (pr:print_tp)
        : doc_plus t =
      let prec = Precedence.application
      in
      if n = 0 then
        (* all arguments are implicit *)
        pr f
      else
        pr f >>= fun fdoc ->
        actual_arguments args pr >>= fun (argsdoc,_) ->
        let doc =
          Document.(lower_parens prec fdoc ^ parens argsdoc)
        in
        make (doc,prec)



    let oo_application
          (tgt:Term.t) (f:Term.t) (args:Term.t list) (n:int) (pr:print_tp)
        : doc_plus t =
      let prec = Precedence.dot in
      pr tgt >>= fun tgt ->
      pr f >>= fun f ->
      let open Document in
      let doc =
        lower_parens prec tgt
        ^ text "."
        ^ lower_parens prec f
      in
      if n = 0 then
        make (doc,prec)
      else
        actual_arguments args pr >>= fun (args,_) ->
        make (doc ^ parens args, prec)


    let operator_application
          (i:int) (args:Term.t list) (pr:print_tp): doc_plus t =
      ask >>= fun (c,_) ->
      let open Term in
      let open Feature_name in
      if not (C.is_valid i c) then
        normal_application (Variable i) args 2 pr
      else
        match C.name i c with
        | Some (Operator op) ->
           let op_string = Operator.string op
           and prec = Operator.precedence op
           in
           begin
             match args with
             | [a] ->
                pr a >>= fun adoc ->
                let open Document in
                let doc =
                  text op_string ^ text " "
                  ^ lower_parens prec adoc
                  |> group
                in
                make (doc,prec)
             | [a;b] ->
                pr a >>= fun adoc ->
                pr b >>= fun bdoc ->
                let open Document in
                let doc =
                  left_parens prec adoc
                  ^ space
                  ^ text op_string
                  ^ text " "
                  ^ right_parens prec bdoc
                  |> group in
                make (doc,prec)

             | _ ->
                assert false (* illegal call, either one or two arguments. *)
           end
        | _ ->
           normal_application (Variable i) args 2 pr


    let of_application
          (f:Term.t) (z:Term.t) (app:Application_type.t)
          (pr:print_tp)
        : doc_plus t =
      ask >>= fun (_,level) ->
      let rec of_appl f z app args n =
        let open Term in
        let open Application_type in
        match app, f with
        | First, _ ->
           normal_application f (z :: args) (n+1) pr

        | First_implicit, _ ->
           normal_application f args n pr

        | Target, _ ->
           oo_application z f args n pr

        | Operator, Variable i when n = 0 ||  n = 1 ->
           operator_application i (z :: args) pr

        | Implicit, Application (f0, a, app0) when no_implicits level ->
           of_appl f0 a app0 args n

        | Implicit, _  when no_implicits level ->
           normal_application f args n pr

        | _, Application (f0, a, app0)  ->
           of_appl f0 a app0 (z :: args) (n+1)

        | _, _ ->
           normal_application f (z :: args) (n+1) pr

      in
      of_appl f z app [] 0




    let formal_argument
          (nme: string option) (tp: Term.typ)
          (pr:print_tp)
        : doc_plus t =
      let prec = Precedence.argument_list
      in
      pr tp >>= fun tpdoc ->
      make (
          Document.(doc_of_simple_name nme
                      ^ text ":"
                      ^ lower_parens prec tpdoc),
          prec)


    let formal_arguments
          (args:Term.argument_list) (m:'a t) (pr:print_tp)
        : (doc * 'a) t =
      let rec formals args =
        match args with
        | [] ->
           assert false (* Illegal call *)

        | [nme,typ] ->
           formal_argument nme typ pr >>= fun (arg,_) ->
           local (push_simple nme typ) m >>= fun a ->
           make (arg, a)

        | (nme,typ) :: tl ->
           formal_argument nme typ pr >>= fun (arg,_) ->
           local (push_simple nme typ) (formals tl) >>= fun (argsdoc,a) ->
           make (Document.(arg ^ text "," ^ cut ^ argsdoc),a)
      in
      formals args




    let of_lambda
          (nme:string option) (tp:Term.typ) (t:Term.t) (pr:print_tp)
        : doc_plus t =
      let prec = Precedence.quantifier in
      let t,args_rev = Term.split_lambda0 (-1) t 1 [nme,tp]
      in
      formal_arguments (List.rev args_rev) (pr t) pr >>= fun (args,t) ->
      let doc =
        Document.(parens args
                  ^ text " :="
                  ^ group (nest 2 (space ^ lower_parens prec t))
                  |> group)
      in
      make (doc,Precedence.quantifier)



    let of_product
          (nme:string option) (tp:Term.typ) (t:Term.t) (pr:print_tp)
        : doc_plus t =
      let prec = Precedence.quantifier in
      let tp,args_rev = Term.split_product0 (-1) t 0 [nme,tp]
      in
      formal_arguments (List.rev args_rev) (pr tp) pr >>= fun (args,tp) ->
      let doc =
        Document.(text "all"
                  ^ parens args
                  ^ space
                  ^ lower_parens prec tp
                  |> group)
        in
        make (doc,Precedence.quantifier)


    let of_case (co:Term.t) (def:Term.t) (pr:print_tp): doc t =
      let args,co = Term.split_lambda co in
      let def,_ = Term.split_lambda0 (Array.length args) def 0 [] in
      local
        (push_arguments args)
        (pr co >>= fun (co,_) ->
         pr def >>= fun (def,_) ->
         let open Document in
         group (co ^ text " :="
                ^ nest 2 (space ^ def))
         |> make)

    let of_cases
          (cases: Term.case array)
          (pr:print_tp)
        : doc t =
      let ncases = Array.length cases
      in
      assert (ncases > 0);
      let rec cases_ i =
        let open Document in
        let co,def = Term.case_pair cases.(i) in
        if i + 1 = ncases then
          of_case co def pr
        else
          of_case co def pr >>= fun c ->
          cases_ (i+1) >>= fun cs ->
          c ^ line "; " ^ cs
          |> make
      in
      cases_ 0


    let of_inspect
          (e:Term.t)
          (r:Term.t)
          (cases: Term.case array)
          (pr:print_tp)
        : doc_plus t =
      pr e >>= fun (e,_) ->
      pr r >>= fun (r,_) ->
      of_cases cases pr >>= fun cases ->
      let open Document in
      let doc =
        (text "inspect"
         ^ nest 2 (space ^ e ^ line "; " ^ r)
         ^ space ^ text "case"
         |> group)
        ^ nest 2 (space ^ cases)
        ^ space ^ text "end"
        |> group
      in
      make (doc,Precedence.quantifier)


    (* Fixpoint *)
    (* mutual
         count (t:Tree(A)): Natural :=
            inspect t case
              node (a, children) := 1 + count(children)
            end
         count (f:Forest(A)): Natural :=
            inspect f case
               [] := 0
               x ^  xs := count(x) + count(xs)
            end
       end
     *)
    let of_one_fixpoint (i:int) (fp:Term.fixpoint) (pr:print_tp): doc t =
      let nme,typ,decr,t = fp.(i)
      and n = Array.length fp in
      let e,args_rev = Term.split_lambda0 (-1) t 0 []
      in
      let nargs = List.length args_rev
      in
      let typ,_ = Term.split_product0 nargs (Term.up n typ) 0 []
      in
      let m =
        pr e >>= fun e ->
        pr typ >>= fun typ ->
        make (e,typ)
      in
      formal_arguments (List.rev args_rev) m pr >>= fun (args,(e,(typ,_))) ->
      let open Document in
      let doc =
        let prec = Precedence.quantifier
        in
        doc_of_name nme
        ^ parens args
        ^ text ": " ^ typ
        ^ text " := "
        ^ group (nest 2 (space ^ lower_parens prec e))
        |> group
      in
      make doc

    let of_fixpoint (fp:Term.fixpoint) (pr:print_tp): doc_plus t =
      let n = Array.length fp in
      assert (n > 0);
      let open Document in
      let rec fixpoints i fp =
        if i + 1 = n then
          of_one_fixpoint i fp pr
        else
          of_one_fixpoint i fp pr >>= fun fpi ->
          fixpoints (i+1) fp >>= fun fps ->
          fpi ^ cut ^ fps |> make
      in
      local
        (push_fixpoint fp)
        (fixpoints 0 fp >>= fun fps ->
         let doc =
           if n = 1 then
             fps
           else
             text "mutual" ^ nest 2 (cut ^ fps) ^ cut ^ text "end"
         in
         make (doc,Precedence.quantifier))


    let rec of_term (t:Term.t): doc_plus t =
      let open Term in
      match t with
      | Sort s ->
         of_sort s
      | Variable i ->
         of_variable i of_term
      | Application (f,z,app) ->
         of_application f z app of_term
      | Lambda (nme,tp,t) ->
         of_lambda nme tp t of_term
      | All(nme,tp,t) ->
         of_product nme tp t of_term
      | Inspect (e,m,f) ->
         of_inspect e m f of_term
      | Fix (i,arr) ->
         assert false (* nyi *)


    let term (c:context) (level:level) (t:Term.t): Document.t =
      run (c,level) (map fst (of_term t))

    let fixpoint (c:context) (level:level) (fp:Term.fixpoint): Document.t =
      run (c,level) (of_fixpoint fp of_term |> map fst)
  end
