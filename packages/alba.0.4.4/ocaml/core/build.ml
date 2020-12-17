(* Status: Very Draft

   A first attempt to do term building with the sexp_parser.

   Used in 'Welltyped'.
*)











(** A core internal module to support term building. Do not use outside of core
    directly.



    Basic Idea
    ==========


    There is a stack of to be built terms. When the construction of term starts,
    the term can get some type requirement.

    Invariant
    =========

    - Only terms which are welltyped in the context are constructed.

    - All terms on the stack are welltyped and satisfy their requirements or
      have still a chance to satisfy their requirement. The satisfaction of the
      requirements is checked by putting a term to the stack.


    Used Typing Rules
    =================

    Product
    -------

        Gamma      |- A: sA
        Gamma, x:A |- B: sB
        Gamma      |- pi_sort(sA,sB) <= s
        -----------------------------------
        Gamma      |- (all (x: A). B) : s


        sA              sR              pi_sort(sA,sR)
        ----------------------------------------------
        *               Proposition     Proposition
        Proposition     Any(i)          Any(i)
        Any(i)          Any(j)          Any(max(i,j))


        At the start there is on top of the stack an empty term with an optional
        requirement of its return type. The required type, if present, must be a
        sort.

        Depending of the required type we push a new item onto the stack. If the
        required type is a proposition, then the required type of the argument
        type is Any. If the required type of the whole product is [Any(i)], then
        the required type of the argument type is [Any(i)].

        We build the argument type and pop it and push is as a bound variable.

        Then we push a new item for the result type with the same required type
        as the requirement of the product term onto the stack and build the
        result type.

        Finally we pop the result type and the binder and put the product term
        onto the top of the stack.


    Application
    -----------

        Gamma |- f: all (x: A). B
        Gamma |- a: A'
        Gamma |- A' <= A             -- subtype
        ----------------------------------------
        Gamma |- f a: B[x:=a]

        We start to build [f a] with a possible requirement which has to be
        satisfied after being applied to [nargs] arguments.

        [start_application] increments nargs by one. Then it builds [f]. The
        function term can only be built if its type is a product type.

        Then we start a new term with the required type [A] from the type of
        [f].



    Lambda
    ------

        Gamma      |- (all (x: A): B) : s
        Gamma, x:A |- e: B
        --------------------------------------------
        Gamma      |- (\ (x: A) := e): all (x: A): B

*)






open Fmlib
open Module_types
open Common



module Make (Info: ANY) =
struct
    module Algo = Gamma_algo.Make (Gamma_holes)
    module Uni = Unifier.Make (Gamma_holes)

    type name =
        Info.t * string


    type requirement =
        | No_requirement
        | Some_type
        | Result_type of Term.typ * int




    type item = {
        requirement: requirement;
        satisfied: bool;
        nargs: int;
        term: (Info.t
               * Term.t
               * Term.typ
               * int  (* size of the context *)
               ) option;
    }

    type binder = {
        info: Info.t;
        sort: Sort.t;
        binder_level: int;
        name_map_previous: Name_map.t;
    }


    type t = {
        name_map: Name_map.t;
        gh: Gamma_holes.t;
        stack: item list;           (* terms under construction *)
        binders: binder list;       (* open binders *)
    }

    type problem = Info.t * Type_error.t

    type 'a res = ('a, problem) result


    let make (context: Context.t): t =
        let gamma = Context.gamma context
        and name_map = Context.name_map context
        in
        {
            name_map;
            gh = Gamma_holes.make gamma;
            stack = [];
            binders = [];
        }


    let empty: t =
        make Context.empty


    let count (bc: t): int =
        Gamma_holes.count bc.gh

    let count_base (bc: t): int =
        Gamma_holes.count_base bc.gh


    let string_of_term (term: Term.t) (bc: t): string =
        Term_printer.string_of_term
            term
            (Gamma_holes.context bc.gh)


    let term_to_context (term: Term.t) (n: int) (bc: t): Term.t =
        assert (n <= count bc);
        Term.up (count bc - n) term


    let into_new_binder (idx: int) (term: Term.t): Term.t
        (* Put [term] which is welltyped in the current context into a new
           binder where [idx] is the index of the current binder to which is
           bound.

                gamma ... idx ... |- term
                                  ^
                                  cnt

           [idx] is no longer refereced in the new term. All referenced to [idx]
           are replace by 0 i.e. the new bound variable. All other variables
           have to be increased by one, because they have to escape the new
           binder.
        *)
        =
        Term.map
            (fun i ->
                 if i = idx then
                     0
                 else
                     i + 1)
            term



    let sort_of_term (term: Term.typ): Sort.t =
        match term with
        | Sort sort ->
            sort
        | _ ->
            assert false (* term is not a sort *)



    let make_pi
            (arg_name: string)
            (arg_idx: int)
            (arg_typ: Term.typ)
            (arg_sort: Sort.t)
            (res_typ: Term.typ)
            (res_sort: Term.typ)
            (bc: t)
        : Term.typ * Term.typ
        =
        Term.make_product
            arg_name
            true (* is_typed *)
            (Algo.is_kind arg_typ bc.gh)
            arg_typ
            (into_new_binder arg_idx res_typ)
        ,
        (Term.Sort
             (Sort.pi_sort
                  arg_sort
                  (sort_of_term res_sort)))



    let push_item
            (requirement: requirement)
            (nargs: int)
            (bc: t)
        : t
        =
        {bc with
         stack =
             {requirement; nargs; satisfied = false; term = None}
             ::
             bc.stack}



    let split_stack (bc: t): item * item list =
        List.split_head_tail bc.stack


    let split_binders (bc: t): binder * binder list =
        List.split_head_tail bc.binders



    let map_top (f: item -> item res): t -> t res =
        fun bc ->
        let top, stack = split_stack bc in
        Result.map
            (fun item -> {bc with stack = item :: stack})
            (f top)




    let map_top0 (f: item -> item): t -> t =
        fun bc ->
        match map_top (fun item -> Ok (f item)) bc with
        | Ok bc ->
            bc
        | Error _ ->
            assert false (* Illegal call. *)



    let get_top (bc: t): Term.t * Term.typ * item list =
        let item, stack = split_stack bc
        in
        match item.term with
        | None ->
            assert false (* Cannot happen. Term construction not yet completed.
                         *)
        | Some (_, term, typ, n) ->
            term_to_context term n bc,
            term_to_context typ n bc,
            stack



    let pop_top (bc: t): Term.t * Term.typ * t =
        let term, typ, stack = get_top bc
        in
        term, typ, {bc with stack}



    let pop_binder (bc: t): string * int * Term.typ * Sort.t * t =
        let binder, binders = split_binders bc
        in
        Gamma_holes.name_at_level binder.binder_level bc.gh,
        Gamma_holes.index_of_level binder.binder_level bc.gh,
        Gamma_holes.type_at_level binder.binder_level bc.gh,
        binder.sort,
        {bc with
         gh = Gamma_holes.pop_bound bc.gh;
         binders;
         name_map = binder.name_map_previous}



    let get_final (bc: t): Term.t * Term.typ =
        (* Get the term in the original context. *)
        let term, typ, stack = get_top bc
        in
        assert (stack = []);
        let cnt = count bc
        and cnt0 = count_base bc
        in
        match
            Term.down (cnt - cnt0) term,
            Term.down (cnt - cnt0) typ
        with
        | Some term, Some typ ->
            term, typ
        | _, _ ->
            assert false (* Term or type still contain some holes. *)




    let set_term
            (info: Info.t) (term: Term.t) (typ: Term.typ)
        : t -> t
        =
        fun bc ->
        let item, stack = split_stack  bc in
        {bc with
         stack =
             {item with
              term =
                  Some (info, term, typ, count bc)}
             ::
             stack}



    let put_term
            (info: Info.t) (term: Term.t) (typ: Term.typ)
        : t -> t res
        =
        (*  Two cases:

            1. nargs = 0:
                Check that welltyped [term] of [typ] satisfies the requirements.
                If yes, put it as the new top term. Otherwise report an error.

            2. nargs > 0:
                Check that the term is a function term. If yes, put it as the
                new top term and push a new item for the expected argument onto
                the stack. Otherwise report an error.
        *)
        fun bc ->
        let item, _ = split_stack bc
        in
        let put bc =
            Ok (set_term info term typ bc)
        in
        if item.nargs = 0 then
            match item.requirement with
            | No_requirement ->
                put bc

            | Some_type ->
                if Term.is_sort typ then
                    put bc
                else
                    Error (info, Type_error.Not_a_type)

            | Result_type (req_typ, n) ->
                (
                    let req_typ = term_to_context req_typ n bc
                    in
                    match Uni.unify typ req_typ true bc.gh with
                    | None ->
                        Error (
                            info,
                            Type_error.Wrong_type (
                                req_typ,
                                typ,
                                Gamma_holes.context bc.gh
                            ))
                    | Some gh ->
                        put {bc with gh}
                )
        else
            let typ_n = Algo.key_normal typ bc.gh
            and cnt = count bc in
            match typ_n with
            | Term.Pi (arg_tp, _, _) ->
                Ok (
                    set_term info term typ_n bc
                    |> push_item (Result_type (arg_tp, cnt)) 0
                )
            | _ ->
                Error (info,
                       Type_error.Not_a_function (
                           typ, Gamma_holes.context bc.gh)
                )




    let check_naming_convention
            (info: Info.t) (name: string) (typ: Term.typ) (bc: t)
        : t res
        =
        let is_lower, is_upper =
            if String.length name > 0 then
                let c = name.[0] in
                Char.is_lower c, Char.is_upper c
            else
                false, false
        and tv fail =
            if not fail then
                Ok bc
            else
                Error (info, Type_error.Naming_type_variable)
        and no_tv fail =
            if not fail then
                Ok bc
            else
                Error (info, Type_error.Naming_no_type_variable)
        in
        match Algo.sort_of_kind typ bc.gh with
        | None ->
            (* proof or object, must be lower case or operator *)
            no_tv is_upper
        | Some Sort.Proposition ->
            (* must be lower case or operator *)
            no_tv is_upper
        | Some Sort.Any _ ->
            (* mus be upper case or operator *)
            tv is_lower




    let make_sort (info: Info.t) (s: Sort.t) (bc: t): t res =
        let add =
            put_term
                info
                (Term.Sort s)
                (Term.Sort (Sort.type_of s))
        in
        match s with
        | Sort.Proposition ->
            add bc
        | Sort.Any i ->
            if i > 0 then
                Error (info, Type_error.Higher_universe i)
            else
                add bc




    let make_identifier (info: Info.t) (name: string): t -> t res =
        fun bc ->
        if name = "Proposition" then
            make_sort info Sort.Proposition bc
        else if name = "Any" then
            make_sort info (Sort.Any 0) bc
        else
            match Name_map.find name bc.name_map with
            | [] ->
                Error (info, Type_error.Name_not_found name)
            | [level] ->
                let level = Gamma_holes.adapted_level level bc.gh in
                let typ = Gamma_holes.type_at_level level bc.gh
                and idx = Gamma_holes.index_of_level level bc.gh
                in
                put_term info (Term.Variable idx) typ bc
            | _ ->
                Error (info,
                       Type_error.Not_yet_implemented
                           "<name overloading>")




    let start_term: t -> t =
        (* Start a term without any requirements. *)
        push_item No_requirement 0



    let start_typed_term (bc: t): t =
        let typ, _, _ = get_top bc in
        push_item (Result_type (typ, count bc)) 0 bc




    let start_type: t -> t =
        (* Start the analysis of a type. *)
        push_item Some_type 0




    (* Build Function Application
       --------------------------

       initial state:
            An empty item is on top of the stack which represents the
            requirement for the whole application.

       start_application:
            Increase the number of expected arguments by one.

       build the function term:
            The top of the stack is replaced by the built function term. A new
            item with the requirement of the function argument is pushed onto
            the stack.

       build the argument term:
            The built argument term is on top of the stack.

       end_application:
            The function term and the argument term are on top of the stack.
            Pop the argument term and replace the function term with the
            application and decrease the number of expected arguments by one.
    *)


    let start_application: t -> t =
        (* Start an application [f a]. The top term contains the requirement for
         * [f a]. In order to convert this into a requirement for [f], the
         * number of expected arguments has to be increased by one. *)
        map_top0
            (fun item ->
                 assert (item.term = None);
                 {item with nargs = item.nargs + 1})




    let end_application (info: Info.t) (bc: t): t res =
        let arg, _, bc = pop_top bc in
        let f, typ, _  = get_top bc in
        let bc =
            map_top0
                (fun item ->
                     assert (item.nargs > 0);
                     {item with nargs = item.nargs - 1})
                bc
        in
        match typ with
        | Pi (_, res_tp, _) ->
            put_term
                info
                (Term.application f arg)
                (Term.apply res_tp arg)
                bc
        | _ ->
            assert false (* Cannot happen. [f] has already been checked to have
                            a function type. *)




    let start_binder ((info, name): name): t -> t res =
        fun bc ->
        let open Result in
        let typ, sort, stack = get_top bc in
        let sort = sort_of_term sort in
        check_naming_convention info name typ bc
        >>= fun _ ->
        Ok
            {name_map =
                 Name_map.add_local name bc.name_map;
             stack;
             gh =
                 Gamma_holes.push_bound name true typ bc.gh;
             binders =
                 {info;
                  sort;
                  name_map_previous = bc.name_map;
                  binder_level = count bc
                 }
                 ::
                 bc.binders
            }


    let start_pi (info: Info.t): t -> t res =
        fun bc ->
        let item, _ = split_stack bc
        and cnt = count bc in
        assert (item.term = None);
        match item.requirement with
        | No_requirement | Some_type ->
            Ok (push_item Some_type 0 bc)

        | Result_type (Term.(Sort sort), _) ->
            (
                match sort with
                | Sort.Proposition ->
                    Ok (push_item
                            (Result_type (Term.Sort (Sort.Any 1), cnt))
                            0
                            bc)
                | _ ->
                    Ok (push_item
                            (Result_type (Term.Sort sort, cnt))
                            0
                            bc)
            )
        | Result_type (req_typ,n) ->
            Error (info,
                   Type_error.No_type_allowed (
                       term_to_context req_typ n bc,
                       Gamma_holes.context bc.gh))



    let start_pi_result: t -> t res =
        fun bc ->
        let item, _ = split_stack bc in
        assert (item.term = None);
        match item.requirement with
        | No_requirement | Some_type ->
            Ok (push_item Some_type 0 bc)

        | Result_type (req_typ, n) ->
            Ok (push_item
                    (Result_type (req_typ, n))
                    0
                    bc)




    let end_pi (info: Info.t): t -> t res =
        fun bc ->
        let res_typ, res_sort, bc = pop_top bc
        in
        let arg_name, arg_idx, arg_typ, arg_sort, bc = pop_binder bc
        in
        let pi_term, pi_type =
            make_pi arg_name arg_idx arg_typ arg_sort res_typ res_sort bc
        in
        put_term info pi_term pi_type bc
end
