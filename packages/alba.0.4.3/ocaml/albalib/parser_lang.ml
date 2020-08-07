open Fmlib
open Module_types
open Common
open Alba_core
open Ast

module Located = Character_parser.Located
type 'a located = 'a Located.t

module Position = Character_parser.Position

module Indent   = Character_parser.Indent

type indent   = Character_parser.Indent.t
type position = Character_parser.Position.t
type range    = Position.t * Position.t








module Command =
struct
    type t =
        | Evaluate of Expression.t
        | Type_check of Expression.t
        | Define of Expression.definition
        | Clear
        | Load of string Located.t
        | Reload
        | Exit
        | Do_nothing
end



module Source_file =
struct
    type entry =
        | Expression of (bool * Expression.t)
        | Entry of Source_entry.t


    type t = {
        entries: entry list;
        n: int;
    }

    let count (src: t): int =
        src.n

    let empty: t =
        {
            n = 0;
            entries = [];
        }

    let top (src: t): entry =
        assert (0 < count src);
        fst (List.split_head_tail src.entries)


    let push_entry
        (entry: Source_entry.t) (src: t): t =
        {
            entries = Entry entry :: src.entries;

            n = src.n + 1;
        }


    let push_expression
        (evaluate_flag: bool) (exp: Expression.t) (src: t): t
        =
        {
            entries =
                Expression (evaluate_flag, exp) :: src.entries;

            n = src.n + 1;
        }
end






module Problem =
struct
    type t =
      | Operator_precedence of
          string * string (* the 2 operator strings *)

      | Illegal_name of string (* expectation *)

      | Illegal_command of string list

      | Ambiguous_command of string list

      | Duplicate_argument

      | Unused_definition of string

      | No_result_type

      | No_argument_type
end




module type ERROR =
            Generic_parser.ERROR
                with type expect   = string * Character_parser.Indent.t
                and  type semantic = range * Problem.t






module Print (Error: ERROR) (P: Pretty_printer.SIG) =
struct
    let problem (problem: Problem.t): P.t =
        let open Problem in
        let open P
        in
        match problem with
        | Operator_precedence (op1, op2) ->
            let source_text op1 op2 =
                string "_ "
                <+> string op1
                <+> string " _ "
                <+> string op2
                <+> string " _"
            and left op1 op2 =
                string "( _ "
                <+> string op1
                <+> string " _ ) "
                <+> string op2
                <+> string " _"
            and right op1 op2 =
                string "_ "
                <+> string op1
                <+> string " ( _ "
                <+> string op2
                <+> string " _ )"
            in
            wrap_words "I am no able to group your operator expression"
            <+> cut <+> cut
            <+> nest 4 (source_text op1 op2) <+> cut <+> cut
            <+> wrap_words "I can either group the first two"
            <+> cut <+> cut
            <+> nest 4 (left op1 op2) <+> cut <+> cut
            <+> wrap_words "or group the second two"
            <+> cut <+> cut
            <+> nest 4 (right op1 op2) <+> cut <+> cut
            <+> wrap_words
               "However the precedence and associativity of these operators \
                don't give me enough information. Please put parentheses to \
                indicate your intention."
            <+> cut <+> cut

        | Illegal_name expect ->
            wrap_words "I was expecting"
            <+> group space
            <+> string expect
            <+> cut

        | Illegal_command _ ->
            string "Illegal commmand" <+> cut

        | Ambiguous_command _ ->
            string "Ambiguous commmand" <+> cut

        | Duplicate_argument ->
            wrap_words "I found a duplicate argument name. All names \
                        of formal arguments must be different."
            <+> cut <+> cut

        | Unused_definition _ ->
            wrap_words "This local definition is not used. \
                        Sorry, this is not allowed."
            <+> cut <+> cut

        | No_result_type ->
            wrap_words
                "Top level definitions must have an explicit result type."
            <+> cut <+> cut

        | No_argument_type ->
            wrap_words
                "In top level definitions all formal arguments \
                must be explicitly typed."
            <+> cut <+> cut



    let expectations
        (col: int)
        (exps: (string * indent) list)
        (tab_positions: int list)
        : P.t
        =
        let open P in
        let find_tab_number ind =
            let rec find number tabs =
                match tabs with
                | [] ->
                    assert false (* Illegal call! *)
                | pos :: tabs ->
                    if pos = Indent.lower_bound ind then
                        number
                    else
                        find (number + 1) tabs
            in
            find 0 tab_positions
        in
        let expectation e ind =
            if Indent.is_offside col ind then
                string e
                <+>
                (if Indent.has_only_one_position ind then
                    string " at tab marker "
                 else
                    string " starting at tab marker ")
                <+>
                string (string_of_int (find_tab_number ind))
            else
                string e
        in
        match exps with
        | [] ->
            assert false (* Cannot happen, at least one expectation *)

        | [e, ind] ->
            string "I was expecting the following"
            <+> cut <+> cut
            <+> nest 4 (expectation e ind)
            <+> cut <+> cut

        | lst ->
            string "I was expecting one of the following"
            <+> cut <+> cut
            <+> nest
                    4
                    (list_separated
                        cut
                        (List.map
                            (fun (e,ind) ->
                                string "- "
                                <+>
                                expectation e ind)
                            lst))
            <+> cut <+> cut
end





module type SIG =
    sig
        type parser
        type state
        type final
        type _ t

        module Error: ERROR

        val needs_more: parser -> bool
        val has_ended:  parser -> bool
        val has_succeeded: parser -> bool
        val has_failed: parser -> bool
        val state: parser -> Source_file.t

        val put_character: parser -> char -> parser
        val put_end:  parser -> parser

        val result: parser -> final option
        val error:  parser -> Error.t
        val line: parser -> int
        val column: parser -> int
        val position: parser -> position
        val error_tabs: parser -> int list

        val expression: unit -> Expression.t t
        val command: Command.t t
        val global_definition:  _ -> Expression.definition t
        val global_definitions: _ -> Expression.definition array t
        val inductive_type:     _ -> Source_entry.inductive t
        val inductive_family:   _ -> Source_entry.inductive array t
        val source_file: bool -> unit t
        val make: final t -> parser
        val run: final t -> string -> parser

        module Error_printer (PP: Pretty_printer.SIG):
        sig
            val print_with_source: string -> parser -> PP.t
            val print_with_source_lines:
                string Sequence.t -> parser -> PP.t
        end
    end



let keywords: String_set.t =
  let open String_set in
  empty
  |> add "all"
  |> add "case"
  |> add "class"
  |> add "inspect"
  |> add "mutual"
  |> add "where"






module Make (Final: ANY) =
struct
    module P =
        Character_parser.Normal
            (Source_file)
            (Final)
            (struct type t = range * Problem.t end)
            (String)


    include P


    let make_where
        (e: Expression.t)
        (defs: Expression.definition list)
        (end_pos: position)
        : Expression.t t
        =
        match Expression.find_unused_local e defs with
        | None ->
            return (
                Located.make
                    (Located.start e)
                    (Expression.Where (e, defs))
                    end_pos)
        | Some name ->
            fail (
                Located.range name,
                Problem.Unused_definition (Located.value name)
            )




    let line_comment: unit t =
        backtrackable (string "--") "\"--\""
        >>= fun _ ->
        skip_zero_or_more
          (expect
             (fun c -> c <> '\n')
             "any char except newline")
        >>= fun _ ->
        return ()


    let multiline_comment: unit t =
        let rec to_end (): unit t =
          (char '-' >>= fun _ ->
           (char '}'
           <|> to_end ()))
          <|> (expect (fun _ -> true) "any char"
               >>= fun _ ->
               to_end ())
        in
        backtrackable
          (string "{-")
          "\"{-\""
        >>= fun _ ->
        to_end ()


    let whitespace_char: char t =
        expect
            (fun c -> c = ' ' || c = '\n' || c = '\t')
            "space, newline or tab"


    let whitespace: int t =
        detached
            (skip_zero_or_more
                (
                    (map (fun _ -> ()) whitespace_char)
                    <|> line_comment
                    <|> multiline_comment
                    <?> "whitespace"
                )
            >>= succeed (* To avoid error messages like "expecting whitespace"
            *)
            )

    let command_argument: string Located.t t =
        located (
            map
                String.of_list
                (
                    one_or_more
                    (expect
                        (fun c -> c <> ' ' && c <> '\n' && c <> '\r')
                        "normal character")
                )
        )

    let raw_name: string t =
        word
            Char.is_letter
            (fun c -> Char.is_letter c || Char.is_digit c || c = '_')
            "identifier"


    let name: string located t =
        located raw_name


    let name_ws: string located t =
        name |. whitespace


    let identifier (with_any_prop: bool): string located t =
        backtrackable
            (
                name
                >>= fun s ->
                let str = Located.value s
                in
                if
                    String_set.mem str keywords
                    || Operator.is_keyword_operator str
                    || (
                        not with_any_prop
                        && (str = "Proposition" || str = "Any"))
                then
                    unexpected "identifier"
                else
                    return s
            )
            "identifier"
        |. whitespace


    let number: string located t =
        located
            (word Char.is_digit Char.is_digit "digit")
        |. not_followed_by letter "not a letter"
        |. whitespace


    let identifier_expression: Expression.t t =
      map
        (Located.map
           (fun s ->
             if s = "Proposition" then
               Expression.Proposition
             else if s = "Any" then
               Expression.Any
             else
               Expression.Identifier s
        ))
        (identifier true)


    let number_expression: Expression.t t =
      map
        (Located.map (fun s -> Expression.Number s))
        number



    let literal_string: Expression.t t =
      located (
          return
            (fun chars ->
              let chars = Array.of_list chars in
              let len   = Array.length chars in
              Expression.String (String.init len (fun i -> chars.(i))))
          |. char '"'
          |= zero_or_more
               (expect
                  (fun c ->
                    let i = Char.code c in
                    Char.code ' ' <= i && i < 128 && c <> '"')
                  "string character")
          |. char '"')
      |. whitespace


    let literal_char: Expression.t t =
      located (
          return
            (fun c -> Expression.Char (Char.code c))
          |. char '\''
          |= expect
               (fun c -> c <> '\'' && c <> '\n')
               "character"
          |. char '\'')
      |. whitespace


    let left_bracket: unit t =
        backtrackable
            (
                char '['
                |. not_followed_by (char ']') "not ']'"
            )
            "'['"
        |. whitespace


    let right_bracket: unit t =
        char ']'
        |. whitespace


    let empty_list_string: string t =
        map
            (fun _ -> "[]")
            (backtrackable (string "[]") "[]")


    let empty_list_expression: Expression.t t =
        map
            (Located.map (fun _ -> Expression.Identifier "[]"))
            (
                located (
                    backtrackable
                        (string "[]")
                        "[]"
                )
                |. whitespace
            )


    let colon: unit t =
      backtrackable
        (char ':'
         |. not_followed_by (char '=') "not '='")
        "':'"


    let assign: unit t =
      backtrackable
        (string ":=")
        "':='"


    let operator_character: char t =
        one_of_chars "+-^*|/=~<>" "operator character"


    let operator_string
        (with_comma: bool)
        : string Located.t t
    =
        located (
            (
                map
                    String.of_list
                    (one_or_more operator_character)
                >>= fun str ->
                optional (char ':')
                >>= fun oc ->
                match oc with
                | None ->
                    return str
                | Some _ ->
                    return (str ^ ":")

            )
            <|>
            (
                let colon = map (fun _ -> ":") colon
                and comma = map (fun _ -> ",") (char ',')
                in
                if with_comma then
                    colon <|> comma
                else
                    colon
            )
            <|>
            backtrackable
                (
                    raw_name
                    >>=
                    fun str ->
                    if Operator.is_keyword_operator str then
                        return str
                    else
                        unexpected "keyword operator"
                )
                "'and' or 'or'"
        )
        <?>
        "operator"
        |. whitespace



    let operator
        (with_comma: bool)
        : Expression.operator Located.t t
    =
        map
            (Located.map
                (fun op_str -> op_str, Operator.of_string op_str))
            (operator_string with_comma)


    let unary_operator: Expression.operator Located.t t =
        backtrackable
            (
                operator false
                >>= fun op ->
                let op_str, _ = Located.value op in
                if Operator.is_unary op_str then
                    return op
                else
                    unexpected "unary operator"
            )
            "unary operator"


    let lonely_operator: Expression.t t =
        map
            (fun op_located ->
                Located.map (fun op -> Expression.Operator op) op_located)
            (operator true)


    let char_ws (c:char): unit t =
        char c |. whitespace



    let zero_or_more_reversed (p: 'a t): 'a list t =
        let rec many l =
            (p >>= fun a -> many (a :: l))
            <|>
            return l
        in
        many []


    let parenthesized
        (p: unit -> 'a Located.t t)
        : 'a Located.t t
    =
        located (char '(') |. whitespace
        >>= fun loc1 ->
        p ()
        >>= fun a ->
        located (char ')') |. whitespace
        >>= fun loc2 ->
        return
            (Located.make
                (Located.start loc1)
                (Located.value a)
                (Located.end_ loc2))


    let name_for_definition (with_operators: bool): string Located.t t =
        if with_operators then
            identifier false
            <|>
            located empty_list_string
            <|>
            parenthesized
                (fun _ -> operator_string true)
        else
            identifier false


    let rec find_duplicate_argument
        (arg_lst: (string located * Expression.t option) list)
        : string located option
        =
        match arg_lst with
        | [] ->
            None
        | arg :: args ->
            let arg_name (nme,_) =
                Located.value nme
            in
            let name = arg_name arg
            in
            match List.find (fun arg2 -> name = arg_name arg2) args with
            | None ->
                find_duplicate_argument args
            | Some (duplicate, _) ->
                Some duplicate



    let rec expression0 (with_comma: bool) (): Expression.t t =

        let primary (what: string): Expression.t t =
            backtrackable identifier_expression "identifier"
            <|>
            number_expression
            <|>
            literal_char
            <|>
            literal_string
            <|>
            empty_list_expression
            <|>
            (*  "( exp )" *)
            parenthesized (
                fun _ ->
                indented (
                    expression0 true ()
                    <|>
                    lonely_operator
                )
            )
            <|>
            (*  "[ e1, e2, ... ]" *)
            located (
                map
                    Expression.to_list
                    (
                        left_bracket
                        >>= fun _ ->
                        (indented (expression0 true ()))
                        |. right_bracket
                    )
            )
            <|>
            (* \ (x: A) (y: B) ... : RT := exp *)
            (
                return
                    (fun pos1 args rt exp ->
                        Located.make
                            pos1
                            (Expression.Function (args, rt, exp))
                            (Located.end_ exp))
                |= get_position
                |. char_ws '\\'
                |= formal_arguments false false
                |= optional_result_type ()
                |= assign_defining_expression ()
            )
            <|>
            (* all (a: A) (b: B) ... : RT *)
            (
                return
                    (fun pos1 args rt ->
                        Located.make
                            pos1
                            (
                                match Located.value rt with
                                | Expression.Product (args_inner, rt) ->
                                    Expression.Product (args @ args_inner, rt)
                                | _ ->
                                    Expression.Product (args, rt)
                            )
                            (Located.end_ rt)
                    )
                |= get_position
                |. backtrackable (string "all") "all"
                |. whitespace
                |= formal_arguments false false
                |= result_type ()
            )
            <?>
            what
        in

        let application: Expression.t t =
            primary "expression" >>= fun f ->
            (
                match Located.value f with
                | Proposition | Any
                | Number _ | Char _ | String _
                | Product _ ->
                    return []
                | _ ->
                    indented (
                        zero_or_more_reversed
                            (primary "function argument"))

            )
            >>= fun args_rev ->
            match args_rev with
            | [] ->
                return f
            | last :: _ ->
                let arg_lst =
                    List.rev_map
                        (fun arg -> arg, Expression.Normal)
                        args_rev
                in
                let f0, arg_lst =
                    match Located.value f with
                    | Expression.Application (f0, arg_lst0) ->
                        f0, arg_lst0 @ arg_lst
                    | _ ->
                        f, arg_lst
                in
                let pos1 = Located.start f
                and pos2 = Located.end_ last
                in
                return (
                    Located.make
                        pos1
                        (Expression.Application (f0, arg_lst))
                        pos2
                )
        in


        let where_block: Expression.definition list t =
            (
                backtrackable
                    (string "where")
                    "where <local definitions>"
                |. whitespace
            )
            >>= fun _ ->
            indented (one_or_more_aligned (definition false))
        in


        let operand: Expression.operand t =
            map
                (fun exp -> [], exp)
                application
            <|>
            (
                one_or_more unary_operator
                >>= fun op_lst ->
                application
                >>= fun exp ->
                return (op_lst, exp)
            )
        in



        let operator_and_operand (with_comma: bool) =
          return (fun op exp -> (op,exp))
          |= operator with_comma
          |= operand
        in


        let operator_expression (with_comma: bool): Expression.t t =
            operand
            >>= fun e1 ->
            zero_or_more (operator_and_operand with_comma)
            >>= fun lst ->
            (
                match Operator_expression.make e1 lst with
                | Ok e ->
                   return e

                | Error (range, op1, op2) ->
                   fail (range, Problem.Operator_precedence (op1, op2))
            )
        in

        (* expression parsing *)
        absolute (
            operator_expression with_comma
            >>= fun e ->
            located (optional where_block)
            >>= fun def ->
            match Located.value def with
            | None ->
                return e
            | Some definitions ->
                assert (definitions <> []);
                let definitions = List.rev definitions in
                let pos_end = Located.end_ (List.head_strict definitions) in
                make_where
                    e
                    definitions
                    pos_end
        )

    and indented_expression (kind: string) () =
        indented (expression0 false () <?> kind)

    and subexpression (kind: string) () =
        maybe_indented (expression0 false () <?> kind)

    and result_type _ : Expression.t t =
      (colon |. whitespace >>= subexpression "type")
      <?> ": <result type>"


    and optional_result_type _ : Expression.t option t =
      optional (result_type ())


    and typed_formal_argument
        _
        : Expression.formal_argument t
    =
        char_ws '(' >>= fun _ ->
        identifier false
        >>= fun name ->
        colon |. whitespace
        >>= subexpression "type"
        >>= fun typ ->
        char_ws ')'
        >>= fun _ ->
        return (name, Some typ)


    and formal_argument
        (typed: bool)
        : Expression.formal_argument t
    =
        (
            if typed then
                typed_formal_argument ()
            else
                typed_formal_argument ()
                <|>
                map
                    (fun name -> name, None)
                    (identifier false)
        )
        <?>
        "formal argument"


    and formal_arguments
        (zero: bool)
        (typed: bool)
        : Expression.formal_argument list t
    =
        (
            if zero then
                zero_or_more (formal_argument typed)
            else
                one_or_more (formal_argument typed)
        )
        >>= fun lst ->
        match find_duplicate_argument lst with
        | None ->
            return lst
        | Some name ->
            fail (Located.range name, Problem.Duplicate_argument)

    and signature
        (typed: bool)
        : Expression.signature t
    =
        return
            (fun fargs res -> fargs, res)
        |= formal_arguments true typed
        |= optional_result_type ()


    and assign_defining_expression _: Expression.t t =
        assign
        |. whitespace
        >>= indented_expression "defining expression"
        <?>
        ":= <defining expression>"


    and definition (with_operators: bool): Expression.definition t =
        return
            (fun name args res_tp e ->
                let p1 = Located.start name
                and p2 = Located.end_ e
                in
                Located.make p1 (name, args, res_tp, e) p2)
        |= name_for_definition with_operators
        |= formal_arguments true false
        |= optional_result_type ()
        |= assign_defining_expression ()
        <?>
        "definition"



    let expression (): Expression.t t =
        expression0 true ()



    let global_definition _ : Expression.definition t =
        definition true >>= fun def ->
        let name, fargs, res, _ =
            Located.value def
        in
        match
            List.find
                (fun (_, tp) -> tp = None)
                fargs
        with
        | Some (name, _) ->
            fail (Located.range name, Problem.No_argument_type)

        | None ->
            if res = None then
                fail (Located.range name, Problem.No_result_type)
            else
                return def


    let global_definitions _: Expression.definition array t =
        map
            Array.of_list
            (one_or_more_aligned (global_definition ()))




    let named_signature
        (with_operators: bool)
        (typed: bool)
        : Expression.named_signature t
    =
        name_for_definition with_operators
        >>= fun name ->
        signature typed
        >>= fun sign ->
        return (name, sign)


    let inductive_type _: Source_entry.inductive t =
        return (
            fun header constructors ->
                header,
                Array.of_list
                    (List.join constructors)
        )
        |. backtrackable (string "class") "class"
        |. whitespace
        |= indented (
            named_signature true false
        )
        |. assign |. whitespace
        |= indented (
            zero_or_more_aligned
                (
                    one_or_more_separated
                        (named_signature true true)
                        (char ';' |. whitespace)
                )
        )


    let inductive_family _: Source_entry.inductive array t =
        map
            Array.of_list
            (one_or_more_aligned (inductive_type ()))




    let source_entry _: unit t =
        (
        map
            (fun ind -> Source_entry.Inductive [|ind|])
            (inductive_type ())
        <|>
        map
            (fun def -> Source_entry.Normal def)
            (global_definition ())
        )
        <|>
        (
            string "mutual" |. whitespace
            >>= fun _ ->
            indented (
                map
                    (fun inds -> Source_entry.Inductive inds)
                    (inductive_family ())

            )
        )
        >>= fun entry ->
        update
            (Source_file.push_entry entry)


    let commands: (string * Command.t t) list =
        (* repl commands *)
        ["evaluate",
         map (fun e -> Command.Evaluate e) (expression ());

         "typecheck",
         map (fun e -> Command.Type_check e) (expression ());

         "clear", return Command.Clear;

         "load",
         map (fun file_name -> Command.Load file_name) command_argument;

         "define",
         map (fun def -> Command.Define def) (global_definition ());

         "exit", return Command.Exit;
        ]


    let find_command (cmd: string): (string * Command.t t) list =
        List.filter
            (fun (str, _) ->
              String.is_prefix cmd str)
        commands


    let command_names (cs: (string * Command.t t) list): string list =
        List.map fst cs


    let command: Command.t t =
        (char ':' >>= fun _ ->
         (name_ws <?> "command")
         >>= fun cmd ->
         match find_command (Located.value cmd) with
         | [] ->
              fail
                  (Located.range cmd,
                   Problem.Illegal_command (command_names commands))

         | [_, arg_parser] ->
            indented arg_parser

         | lst ->
              fail
                  (Located.range cmd,
                   Problem.Ambiguous_command (command_names lst))
        )
        <|> (return
               (fun exp ->
                 match exp with
                 | None ->
                    Command.Do_nothing
                 | Some exp ->
                    Command.Evaluate exp)
             |. whitespace
             |= optional (expression ()))




    let source_file_command: unit t =
        backtrackable
            (
                char ':'
                >>= fun _ ->
                name_ws
                >>= fun str ->
                let str = Located.value str in
                if str = "evaluate" then
                    return true
                else if str = "typecheck" then
                    return false
                else
                    unexpected "command"
            )
            "':evaluate <expression>' or ':typecheck <expression>'"
        >>= fun evaluate_flag ->
        indented (expression ())
        >>= fun exp ->
        update (Source_file.push_expression evaluate_flag exp)



    let declaration (with_expressions: bool): unit t =
        if with_expressions then
            source_file_command
            <|>
            source_entry ()
        else
            source_entry ()


    let declarations (with_expressions: bool): unit t =
        map (fun _ -> ())
            (
                skip_zero_or_more
                    (absolute (declaration with_expressions))
            )


    let source_file (with_expressions: bool) : unit t =
        whitespace
        >>= fun _ ->
        absolute_at 0 (declarations with_expressions)





    let make (p: final t): parser =
        P.make (p |. expect_end) (Source_file.empty)


    let run (p: final t) (input: string): parser =
        run (p |. expect_end) (Source_file.empty) input



    module Error_printer (PP: Pretty_printer.SIG) =
    struct
        module PP0 = Printer.Make (PP)
        module PPr = Print (Error) (PP)

        open PP

        let print0 (lines: range -> int list -> PP.t) (p: parser): PP.t =
            assert (has_ended p);
            assert (not (has_succeeded p));
            if Error.is_semantic (error p) then
                let range, error = Error.semantic (error p) in
                PP0.print_error_header "SYNTAX"
                <+>
                lines range []
                <+>
                PPr.problem error
            else
                let pos = position p
                and tabs = error_tabs p
                in
                PP0.print_error_header "SYNTAX"
                <+>
                lines (pos,pos) tabs
                <+>
                PPr.expectations
                    (column p)
                    (Error.expectations (error p))
                    tabs


        let print_with_source
            (source: string)
            (p: parser)
            : PP.t
            =
            assert (has_ended p);
            assert (not (has_succeeded p));
            print0
                (PP0.print_source source)
                p


        let print_with_source_lines
            (lines: string Sequence.t)
            (p: parser)
            : PP.t
            =
            print0
                (PP0.print_source_lines lines)
                p
    end
end (* Make *)
