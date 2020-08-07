open Fmlib


module type GAMMA =
sig
    type t
    val is_valid_index: int -> t -> bool
    val name_of_index: int -> t -> string
    val push_local: string -> Term.typ -> t -> t
end



module Make (Gamma: GAMMA) (P: Pretty_printer.SIG) =
struct
    module TP = Term_printer.Pretty (Gamma) (P)

    open P

    let print_name (name: string): P.t =
        let len = String.length name in
        if len = 0 then
            string "<empty name>"
        else if
            Common.Char.is_letter name.[0]
            && not (Operator.is_keyword_operator name)
        then
            string name
        else if name.[0] = '[' then
            string name
        else
            char '(' <+> string name <+> char ')'



    let push_types_params
        (ind: Inductive.t) (c0: Gamma.t)
        : int * Gamma.t
    =
        let ntypes = Inductive.count_types ind
        and params = Inductive.parameters ind in
        let nparams = Array.length params
        in
        let rec push_types i c =
            if i = ntypes then
                c
            else
                let name, typ = Inductive.ith_type i ind in
                push_types
                    (i + 1)
                    (Gamma.push_local name (Term.up i typ) c)
        in
        let rec push_params i c =
            if i = nparams then
                c
            else
                let name,typ = params.(i) in
                push_params
                    (i + 1)
                    (Gamma.push_local name typ c)
        in
        ntypes,
        push_types 0 c0 |> push_params 0



    let print_kind (nparams: int) (k: Term.typ) (c0: Gamma.t): P.t =
        let rec print ipar lst k c =
            if ipar = nparams then
                List.rev lst, k, c
            else
                let open Term in
                let open P in
                match k with
                | Pi (arg, res, info) ->
                    let name = Pi_info.name info in
                    print
                        (ipar + 1)
                        (   (char '('
                            <+> string name
                            <+> string ": "
                            <+> TP.print arg c
                            <+> char ')') :: lst)
                        res
                        (Gamma.push_local name arg c)

                | _ ->
                    assert false (* [k] must have [nparams] parameters. *)
        in
        let lst, k_inner, c_inner = print 0 [] k c0
        in
        group (
            list_separated
                space
                lst
            <+> cut
            <+> string ": "
            <+> TP.print k_inner c_inner
        )


    let print_header (i: int) (ind: Inductive.t) (c0: Gamma.t): P.t =
        let name, typ = Inductive.ith_type i ind
        and nparams   = Inductive.count_params ind
        in
        print_name name
        <+> space
        <+> print_kind nparams typ c0


    let print_constructors (i: int) (ind: Inductive.t) (c1: Gamma.t): P.t =
        let n = Inductive.count_constructors i ind in
        let lst =
            let rec print j lst =
                if j = 0 then
                    lst
                else
                    let j = j - 1 in
                    let name, typ = Inductive.raw_constructor i j ind in
                    let co =
                        print_name name
                        <+> string ": "
                        <+> TP.print typ c1
                    in
                    print j (co :: lst)
            in
            print n []
        in
        list_separated
            (line "; ")
            lst


    let print_types
        (ntypes:int) (ind: Inductive.t) (c0: Gamma.t) (c1: Gamma.t)
        : P.t
    =
        let lst =
            let rec print i lst =
                if i = 0 then
                    lst
                else
                    let i = i - 1 in
                    let cls =
                        let header = print_header i ind c0
                        and cons   = print_constructors i ind c1
                        in
                        group (
                            string "class"
                            <+> nest 4 (group space <+> group header)
                            <+> group space
                            <+> string ":="
                            <+> nest 4 (space <+> group cons)
                        )
                    in
                    print i (cls :: lst)
            in
            print ntypes []
        in
        list_separated
            (line "; ")
            lst



    let print (ind: Inductive.t) (c0: Gamma.t): P.t =
        let ntypes, c1 = push_types_params ind c0 in
        assert (0 < ntypes);
        if ntypes = 1 then
            print_types ntypes ind c0 c1
        else
            string "mutual"
            <+> space
            <+> group (
                    nest 4 (
                        print_types ntypes ind c0 c1
                    )
                )
end




module String_print (Gamma: GAMMA) =
struct
    let string_of_inductive (ind: Inductive.t) (c: Gamma.t): string =
        let module PP = Pretty_printer.Pretty (String_printer) in
        let module P  = Make (Gamma) (PP) in
        String_printer.run (
            (PP.run 0 70 45 (P.print ind c))
        )
end



let string_of_inductive (ind: Inductive.t) (c: Gamma.t): string =
    let module SP = String_print (Gamma) in
    SP.string_of_inductive ind c
