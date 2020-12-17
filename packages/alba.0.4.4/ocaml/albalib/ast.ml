open Fmlib
open Alba_core

module Located = Character_parser.Located


type range = Position.t * Position.t


module Expression = struct
    type operator = string * Operator.t

    type argument_type =
      | Normal
      | Operand


    type t =
        t0 Located.t

    and t0 =
        | Proposition
        | Any
        | Identifier of string
        | Number of string
        | Char of int
        | String of string
        | Operator of operator
        | Typed of t * t                      (* exp, type *)
        | Application of t * (t * argument_type) list
        | Function of
            formal_argument list
            * t option                        (* result type *)
            * t                               (* defining expression *)
        | Product of formal_argument list * t
        | Where of t * definition list
        | List of t list

    and formal_argument =
        string Located.t * t option

    and signature =
        formal_argument list * t option

    and named_signature =
        string Located.t * signature

    and definition =
        (string Located.t * formal_argument list * t option * t) Located.t


    type operand = operator Located.t list * t




    let to_list (e: t): t0 =
        let rec to_list e =
            match Located.value e with
            | Application (f, [(a, _) ; (b, _)]) ->
                (
                    match Located.value f with
                    | Identifier "," ->
                        a :: to_list b
                    | _ ->
                        [e]
                )

            | _ ->
                [e]
        in
        List (to_list e)







    let rec occurs (name: string Located.t) (e: t0): bool =
        let name_occurs name exp =
            occurs name (Located.value exp)
        in
        let occurs_opt name term_opt =
            match term_opt with
            | None ->
                false
            | Some term ->
                name_occurs name term
        in
        let rec occurs_in_fargs fargs opt1 opt2=
            match fargs with
            | [] ->
                occurs_opt name opt1 || occurs_opt name opt2
            | (arg_name, arg_tp) :: fargs ->
                Located.value arg_name <> Located.value name
                &&
                (occurs_opt name arg_tp || occurs_in_fargs fargs opt1 opt2)
        in
        match e with
        | Proposition | Any | Number _ | Char _ | String _ | Operator _ ->
            false

        | Identifier str ->
            str = Located.value name

        | Typed (exp, tp) ->
            name_occurs name exp || name_occurs name tp

        | Application (f, args) ->
            name_occurs name f
            ||
            List.find (fun (arg, _) -> name_occurs name arg) args
            <>
            None

        | Function (fargs, res, exp) ->
            occurs_in_fargs fargs res (Some exp)

        | Product (fargs, res) ->
            occurs_in_fargs fargs (Some res) None

        | Where (exp, defs) ->
            (
                match defs with
                | [] ->
                    name_occurs name exp

                | def :: defs ->
                    let name2, fargs, res_tp, def_exp =
                        Located.value def
                    in
                    Located.value name <> Located.value name2
                    &&
                    (
                        occurs_in_fargs fargs (Some def_exp) None
                        ||
                        occurs_opt name res_tp
                        ||
                        occurs name (Where (exp, defs))
                    )
            )

        | List lst ->
            List.find (name_occurs name) lst <> None



    let rec find_unused_local
        (exp: t)
        (defs: definition list)
        : string Located.t option
        =
        match defs with
        | [] ->
            None

        | def :: defs ->
            let name, _, _, _ = Located.value def in
            if occurs name (Where (exp, defs)) then
                find_unused_local exp defs
            else
                Some name
end (* Expression *)







module Operator_expression =
struct
    open Expression

    type rest = (operator Located.t * operand) list

    let (>>=) = Result.(>>=)


    let is_left_leaning
        (op1: operator Located.t)
        (op2: operator Located.t)
        : bool
    =
        let _, op1 = Located.value op1
        and _, op2 = Located.value op2
        in
        Operator.is_left_leaning op1 op2


    let is_right_leaning
        (op1: operator Located.t)
        (op2: operator Located.t)
        : bool
    =
        let _, op1 = Located.value op1
        and _, op2 = Located.value op2
        in
        Operator.is_right_leaning op1 op2


    let apply_unary (op: operator Located.t) (e: t): t =
        let pos1 = Located.start op
        and pos2 = Located.end_ e
        in
        let inner =
            Application (
                Located.map
                    (fun (op_str, _) -> Identifier op_str)
                    op,
                [e, Operand]
            )
        in
        Located.make pos1 inner pos2


    let apply_binary (e1: t) (op: operator Located.t) (e2: t): t =
        let pos_start = Located.start e1
        and pos_end   = Located.end_ e2
        and op_str,_    = Located.value op
        in
        Located.make
            pos_start
            (
                if op_str = ":" then
                    Typed (e1, e2)
                else if op_str = "->" then
                    (* e1 -> e2 *)
                    let name = Located.map (fun _ -> "_") e1
                    in
                    match Located.value e2 with
                    | Product (formal_arguments, result_type) ->
                        Product (
                            (name, Some e1) :: formal_arguments,
                            result_type
                        )
                    | _ ->
                         Product ([name, Some e1], e2)
                else
                    Application (
                        Located.map
                            (fun (op_str,_) -> Identifier op_str)
                            op,
                        [ e1, Operand;
                          e2, Operand]
                    )
            )
            pos_end


    let split_higher (op: operator Located.t) (rest: rest): rest * rest =
        (* Split the rest in two parts. The first part contains only operators
        with higher precedence than [op]. The second part starts with an
        operator with the same precedence or lower. *)
        let precedence op =
            Operator.precedence
                (snd (Located.value op))
        in
        let prec = precedence op
        in
        List.split_at
            (fun (op2, _) ->
                precedence op2 <= prec)
            rest


    let split_right (op: operator Located.t) (rest: rest): rest * rest =
        (* Split the rest in two parts. The first part contains only operators
        which are right leaning with respect to [op] i.e. [e0 op e1 op2 e2] must
        be parsed as [e0 op (e1 op2 e2)].
        *)
        let _, op = Located.value op in
        List.split_at
            (fun (op2, _) ->
                not (
                    Operator.is_right_leaning
                        op
                        (snd (Located.value op2))
                )
            )
            rest


    let rec make
        ((unops, e0): operand)
        (rest: rest)
        : (t, range * string * string) result
    =
        match unops with
        | [] ->
            without_unary e0 rest

        | u :: unops ->
            let higher, lower_equal =
                (* All operators in [rest1] have higher precedence than [u]. *)
                split_higher u rest
            in
            make (unops, e0) higher
            >>=
            fun e ->
            without_unary
                (apply_unary u e)
                lower_equal


    and without_unary
        (e0: t)
        (rest: rest)
        : (t, range * string * string) result
    =
        match rest with
        | [] ->
            Ok e0

        | [op1, e1] ->
            make e1 []
            >>= fun e1 ->
            Ok (apply_binary e0 op1 e1)

        | (op1, e1) :: (op2, e2) :: rest ->
            if is_left_leaning op1 op2 then
                (* (e0 op1 e1) op2 e2 rest *)
                without_unary
                    e0 [op1, e1]
                >>=
                fun e ->
                without_unary
                    e
                    ((op2, e2) :: rest)
            else if is_right_leaning op1 op2 then
                (* e0 op1 (e1 op2 higher) lower_equal *)
                let higher, lower_equal =
                    split_right op1 rest
                in
                make
                    e1 ((op2, e2) :: higher)
                >>= fun e ->
                without_unary
                    e0 ((op1, ([], e)) :: lower_equal)
            else
                let op1_str, _ = Located.value op1
                and op2_str, _ = Located.value op2
                in
                Error (
                    (Located.start e0, Located.end_ (snd e2)),
                    op1_str,
                    op2_str
                )
end (* Operator_exression *)





module Source_entry =
struct
    type named_signature =
        Expression.named_signature

    type inductive =
        named_signature * named_signature array

    type t =
        | Normal of Expression.definition
        | Inductive of inductive array
end
