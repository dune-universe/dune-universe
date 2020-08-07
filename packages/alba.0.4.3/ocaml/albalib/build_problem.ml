open Fmlib
open Alba_core


type pos = Character_parser.Position.t
type range = pos * pos


type type_in_context = Build_context.type_in_context

type description =
    | Overflow
    | No_name
    | Incomplete_type of type_in_context
    | Cannot_infer_bound
    | Not_a_function of type_in_context list
    | Wrong_type of (type_in_context * type_in_context) list
    | Wrong_base of type_in_context list * type_in_context list
    | Ambiguous of type_in_context list
    | Name_violation of string * string (* case, kind *)
    | Ambiguous_definition
    | Wrong_parameter_count of int
    | Wrong_parameter_name of string
    | Wrong_parameter_type of Term.typ * Gamma.t
    | Missing_inductive_type
    | No_inductive_type
    | Duplicate_inductive
    | Duplicate_constructor
    | Wrong_type_constructed of Term.typ * Gamma.t
    | Negative
    | Nested_negative of Inductive.t * int * Gamma.t
    | Not_positive of Term.typ * Gamma.t
    | Not_yet_implemented of string


type t = range * description


module Print (P: Pretty_printer.SIG) =
struct
    module PP = Term_printer.Pretty (Gamma) (P)
    module PPInd = Print_inductive.Make (Gamma) (P)


    let type_or_types (l: 'a list): P.t =
        match l with
        | [_] ->
            P.wrap_words "the type"
        | _ :: _ :: _ ->
            P.wrap_words "one of the types"
        | _ ->
            assert false (* Illegal call! *)

    let typ (holes: int list) (tp: Term.typ) (gamma: Gamma.t): P.t =
        let tp = PP.print tp gamma in
        let open P in
        match holes with
        | [] ->
            tp
        | _ ->
            let holes =
                char '['
                <+>
                list_separated
                    (char ',' <+> group space)
                    (List.map
                        (fun level ->
                            let v = Gamma.variable_at_level level gamma
                            and vtp = Gamma.type_at_level level gamma in
                            PP.print v gamma <+> char ':' <+> char ' '
                            <+> PP.print vtp gamma)
                        holes)
                <+> char ']'
            in
            tp
            <+>
            nest 4 (
                cut
                <+> string "unknown: "
                <+> holes)

    let type_list (lst: type_in_context list): P.t =
        let open P in
        nest 4
            (list_separated
                cut
                (List.map
                    (fun (holes, tp, gamma) ->
                        (typ holes tp gamma))
                    lst))

    let wrong_type
        (reqs: type_in_context list)
        (acts: type_in_context list)
        : P.t
        =
        let open P in
        wrap_words "I was expecting a term which has"
        <+> group space
        <+> type_or_types reqs
        <+> cut <+> cut
        <+> type_list reqs
        <+> cut <+> cut
        <+> wrap_words "and the highlighted term has"
        <+> group space
        <+> type_or_types acts
        <+> cut <+> cut
        <+> type_list acts
        <+> cut <+> cut



    let description (descr: description): P.t =
        let open P in
        match descr with
        | Overflow ->
            wrap_words "The number does not fit into a machine word" <+> cut
        | No_name ->
            string "I cannot find this name or operator" <+> cut
        | Cannot_infer_bound ->
            wrap_words "I cannot infer a type for this variable" <+> cut
        | Incomplete_type tp  ->
            wrap_words "I cannot infer a complete type of the expression. \
                        Only the incomplete type"
            <+> cut <+> cut
            <+> type_list [tp]
            <+> cut <+> cut
            <+> wrap_words "This usually happens if I cannot infer the types \
                            of some bound variables."
            <+> cut

        | Not_a_function lst ->
            assert (lst <> []);
            wrap_words "I was expecting a function which can be applied to \
                        arguments. But the expression has"
            <+> group space
            <+> type_or_types lst
            <+> cut <+> cut
            <+> type_list lst
            <+> cut <+> cut
            <+> wrap_words "which is not a function type." <+> cut

        | Wrong_type lst ->
            assert (lst <> []);
            let reqs, acts = List.split lst in
            wrong_type reqs acts

        | Wrong_base (reqs, acts) ->
            wrong_type reqs acts

        | Ambiguous types ->
            wrap_words
                "This term is ambiguous. It can have the following types."
            <+> cut <+> cut
            <+> type_list types
            <+> cut <+> cut
            <+> wrap_words
                "Please give me more type information to infer a unique type."
            <+> cut

        | Name_violation (case, kind) ->
            if case = "Upper" then
                wrap_words
                    "This identifier must not start with an upper case letter. \
                    Identifiers starting with upper case letters are allowed \
                    only for types and type constructors. \
                    The highlighted identifier is a"
                <+> group space
                <+> string kind
                <+> cut
            else
                wrap_words
                    "This identifier must not start with a lower case letter. \
                    Identifiers starting with lower case letters are allowed \
                    only for object variables, proofs and propositions. \
                    But the highlighted identifier is a"
                <+> group space
                <+> string kind
                <+> cut

        | Ambiguous_definition ->
            wrap_words
                "There is already a definition with the same name and \
                the same signature. Remember that there can be multiple \
                definitions with the same name only if they have \
                different signatures."
                <+> cut

        | Wrong_parameter_count required ->
            wrap_words
                "This inductive type is part of an inductive family. \
                All members of the family must have"
            <+> group space
            <+> string (string_of_int required)
            <+> group space
            <+> wrap_words
                "parameter(s)."
            <+> cut

        | Wrong_parameter_name required ->
            wrap_words
                "All corresponding parameters of an inductive family must \
                have the same name. This parameter should have the name"
            <+> group space
            <+> string ("\"" ^ required ^ "\"")
            <+> cut

        | Wrong_parameter_type (required, gamma) ->
            wrap_words
                "All corresponding parameters of an inductive family must \
                have the same type. This parameter should have the type"
            <+> cut
            <+> nest 4
                (
                    cut <+> PP.print required gamma
                )
            <+> cut

        | Missing_inductive_type ->
            wrap_words
                "The inductive type has indices. Therefore the constructor \
                has to indicate explicitly the type of the object it \
                constructs with all parameters and indices. Please add a \
                type annotation."
            <+> cut

        | No_inductive_type ->
            wrap_words
                "This is not an allowed type of an inductive type. A legal \
                type of an inductive type must have a form like"
            <+> cut <+> cut
            <+> nest 4
                (
                    list_separated
                        cut
                        [ string "Any"
                        ; string "Proposition"
                        ; string "Int -> Int -> Proposition"
                        ]
                )
            <+> cut <+> cut
            <+> wrap_words
                "or any type which reduces to one of these forms. \
                The final type must be either \"Any\" or \
                \"Proposition\""
            <+> cut

        | Duplicate_inductive ->
            wrap_words
                "All types of an inductive family must have different \
                names."
            <+> cut

        | Duplicate_constructor ->
            wrap_words
                "All constructors of an inductive type must have different \
                names."
            <+> cut

        | Wrong_type_constructed (res, gamma) ->
            wrap_words
                "All constructors of an inductive type must construct an \
                object of the inductive type. The constructed type must have \
                the form"
            <+> cut <+> cut
            <+> nest 4 (string "I p1 p2 ... i1 i2 ...")
            <+> cut <+> cut
            <+> wrap_words
                "where 'I' is the name of the inductive type, 'p1 p2 ...' \
                are the parameters and 'i1 i2 ...' are the indices. However \
                the constructed type has the form"
            <+> cut <+> cut
            <+> nest 4 (PP.print res gamma)
            <+> cut <+> cut

        | Negative ->
            wrap_words
                "The constructor does not satisfy the positivity condition. \
                One of its argument types is a function type which uses \
                an object of some of the inductive types as an argument i.e. \
                one of the inductive types appears in a negative position."
            <+> cut

        | Nested_negative (ind, iparam, gamma) ->
            wrap_words
                "The constructor does not satisfy the nested positivity \
                condition. One of its arguments uses an inductive type of \
                this definition nested within the inductive type"
            <+> cut
            <+> nest 4 (
                cut
                <+>
                PPInd.print ind gamma
            )
            <+> cut <+> cut
            <+> wrap_words "as the parameter <"
            <+> string (Inductive.parameter_name iparam ind)
            <+> wrap_words
                "> which does not satisfy the \
                 positivity condition."
            <+> cut

        | Not_positive (typ, gamma) ->
            wrap_words
                "The constructor does not satisfy the positivity condition. \
                One of its argument types uses an inductive type of the family \
                in the positive position:"
            <+> cut <+> cut
            <+> nest 4 (PP.print typ gamma)
            <+> cut <+> cut
            <+> wrap_words "However it is not used in the correct format."
            <+> cut

        | Not_yet_implemented str ->
            char '<' <+> string str <+> char '>'
            <+> group space
            <+> wrap_words "is not yet implemented"


    let print_with_source
        (src: string) ((range, desc): t)
        : P.t
        =
        let module P0 = Printer.Make (P) in
        let open P in
        P0.print_error_header "TYPE"
        <+>
        P0.print_source src range []
        <+>
        description desc
        <+> cut


    let print_with_source_lines
        (lines: string Sequence.t)
        ((range, desc): t)
        : P.t
        =
        let module P0 = Printer.Make (P) in
        let open P in
        P0.print_error_header "TYPE"
        <+>
        P0.print_source_lines lines range []
        <+>
        description desc
        <+> cut
end



let string_of_problem (src: string) (problem: t): string =
    let module Pretty_printer = Pretty_printer.Pretty (String_printer) in
    let module Error_print = Print (Pretty_printer) in
    String_printer.run (
        Pretty_printer.run
            0 70 70
            (Error_print.print_with_source src problem)
    )
