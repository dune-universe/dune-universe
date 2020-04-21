open Fmlib
open Module_types
open Common
open Ast
open Alba_core

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
      | Exit
      | Do_nothing
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
            string "I was expecting a"
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
        type final
        type _ t

        module Error: ERROR

        val needs_more: parser -> bool
        val has_ended:  parser -> bool
        val has_succeeded: parser -> bool

        val put_char: parser -> char -> parser
        val put_end:  parser -> parser

        val result: parser -> final option
        val error:  parser -> Error.t
        val line: parser -> int
        val column: parser -> int
        val position: parser -> position
        val error_tabs: parser -> int list

        val expression: unit -> Expression.t t
        val command: Command.t t
        val make: final t -> parser
        val run: final t -> string -> parser

        module Error_printer (PP: Pretty_printer.SIG):
        sig
            val print_with_source: string -> parser -> PP.t
        end
    end



let keywords: String_set.t =
  let open String_set in
  empty
  |> add "all"
  |> add "create"
  |> add "inspect"
  |> add "where"





module Make (Final: ANY) =
struct
    module P =
        Character_parser.Normal
            (Unit)
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



    let word_ws
          (start: char->bool)
          (inner: char->bool)
          (msg:   string)
        : string located t
      =
      located @@ word start inner msg
      |. whitespace
      >>= succeed


    let name: string located t =
        located
            (word
                Char.is_letter
                (fun c -> Char.is_letter c || Char.is_digit c || c = '_')
                "identifier")


    let name_ws: string located t =
        name |. whitespace


    let identifier: string located t =
        backtrackable
            (located
                (word
                    Char.is_letter
                    (fun c -> Char.is_letter c || Char.is_digit c || c = '_')
                    "identifier")
             >>= fun s ->
             if String_set.mem (Located.value s) keywords then
               fail (Located.range s, Problem.Illegal_name "<identifier>")
             else
                return s)
            "identifier"


    let formal_argument_name: string located t =
        identifier |. whitespace>>= fun name_located ->
        let name = Located.value name_located in
        if String_set.mem name keywords
           || name = "Proposition"
           || name = "Any"
        then
            fail
                (Located.range name_located,
                 Problem.Illegal_name "<argument name>")
        else
            return name_located


    let number: string located t =
      word_ws
        Char.is_digit
        Char.is_digit
        "number"


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
        (identifier |. whitespace)


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



    let colon: unit t =
      backtrackable
        (char ':'
         |. not_followed_by (char '=') "not '='")
        "':'"


    let assign: unit t =
      backtrackable
        (string ":=")
        "':='"


    let operator: Expression.operator Located.t t =
      let op_chars = "+-^*|/=~<>" in
      let len = String.length op_chars in
      let is_op_char c =
        String.find (fun op_char -> c = op_char) 0 op_chars < len
      in
      located
      @@ map
           (fun lst ->
             let op_str = String.of_list lst
             in
             op_str, Operator.of_string op_str)
           (one_or_more
              (expect
                 is_op_char
                 "operator character")
            <|> map (fun _ -> [':']) colon
            <?> "operator or ':'"
           )
      |. whitespace
      >>= succeed


    let lonely_operator: Expression.t t =
      map
        (fun op_located ->
          Located.map (fun op -> Expression.Operator op) op_located)
        operator


    let char_ws (c:char): unit t =
      char c |. whitespace


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



    let rec expression (): Expression.t t =

        let indented_expression () =
            indented (expression ())
        in

        let subexpression (kind: string) () =
            maybe_indented (expression () <?> kind)
        in

        let result_type: Expression.t t =
          colon |. whitespace >>= subexpression "type"
        in

        let optional_result_type: Expression.t option t =
          optional result_type
        in

        let formal_argument: (string located * Expression.t option) t =
          (
            char_ws '(' >>= fun _ ->
            formal_argument_name
            >>= fun name ->
            colon |. whitespace
            >>= subexpression "type"
            >>= fun typ ->
            char_ws ')'
            >>= fun _ ->
            return (name, Some typ)
         )
          <|> map (fun name -> name, None) formal_argument_name
        in

        let formal_arguments zero =
            (if zero then
                zero_or_more formal_argument
            else
                one_or_more formal_argument)
            >>= fun lst ->
            match find_duplicate_argument lst with
            | None ->
                return lst
            | Some name ->
                fail (Located.range name, Problem.Duplicate_argument)
        in


        let primary (): Expression.t t =
            backtrackable identifier_expression "identifier"
            <|>
            number_expression
            <|>
            literal_char
            <|>
            literal_string
            <|>
            (*  ( exp ) *)
            ( ( char_ws '('
                >>= fun _ ->
                (* op_expression has to be encapsulated in a function,
                   otherwise infinite recursion!! *)
                indented_expression ()
                <|>
                indented lonely_operator)
              |. char_ws ')'
            )
            <|>
            (* \ (x: A) (y: B) ... : RT := exp *)
            located
                (return (fun args rt exp -> Expression.Function (args, rt, exp))
                 |. char_ws '\\'
                 |= formal_arguments false
                 |= optional_result_type
                 |= (assign |. whitespace >>= indented_expression))
            <|>
            (* all (a: A) (b: B) ... : RT *)
            located
                (return
                  (fun args rt ->
                      match Located.value rt with
                      | Expression.Product (args_inner, rt) ->
                          Expression.Product (args @ args_inner, rt)
                      | _ ->
                          Expression.Product (args, rt))
                |. backtrackable (string "all") "all"
                |. whitespace
                |= formal_arguments false
                |= result_type)
            <?>
            "expression"
        in

        let application =
          primary () >>= fun f ->
          located (zero_or_more (primary ())) >>= fun args ->
          match Located.value args with
          | [] ->
             return f
          | arg_lst ->
              let arg_lst =
                  List.map (fun arg -> arg, Expression.Normal) arg_lst
              in
              let pos1 = Located.start f
              and pos2 = Located.end_ args
              and f, arg_lst =
               match Located.value f with
               | Expression.Application (f0, arg_lst0) ->
                   f0, arg_lst0 @ arg_lst
               | _ ->
                   f, arg_lst
              in
              return
                (Located.make
                   pos1
                   (Expression.Application (f, arg_lst))
                   pos2)
        in

        let operator_and_operand =
          return (fun op exp -> (op,exp))
          |= operator
          |= application
        in


        let definition: Expression.definition t =
            return
                (fun name args res_tp e -> name, args, res_tp, e)
            |= formal_argument_name
            |= formal_arguments true
            |= optional_result_type
            |= (assign |. whitespace >>= indented_expression)
        in


        let where_block: Expression.definition list t =
            (backtrackable (string "where") "where" |. whitespace)
            >>= fun _ ->
            indented (one_or_more_aligned definition)
        in


        let operator_exression: Expression.t t =
            application >>= fun e1 ->

            zero_or_more operator_and_operand >>= fun lst ->

            (
                match Expression.binary e1 lst with
                | Ok e ->
                   return e

                | Error (range, op1, op2) ->
                   fail (range, Problem.Operator_precedence (op1, op2))
            )
        in

        (* expression parsing *)
        absolute (
            operator_exression >>= fun e ->
            located (optional where_block) >>= fun def ->

            match Located.value def with
            | None ->
                return e
            | Some definitions ->
                assert (definitions <> []);
                make_where e (List.rev definitions) (Located.end_ def)
        )




    let commands: (string * Command.t t) list =
      ["evaluate",
       map (fun e -> Command.Evaluate e) (expression ());

       "exit", return Command.Exit;

       "typecheck",
       map (fun e -> Command.Type_check e) (expression ());
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

    let make (p: final t): parser =
        P.make (p |. expect_end) ()

    let run (p: final t) (input: string): parser =
        run (p |. expect_end) () input



    module Error_printer (PP: Pretty_printer.SIG) =
    struct
        module PP0 = Printer.Make (PP)
        module PPr = Print (Error) (PP)

        open PP

        let print_with_source
            (source: string)
            (p: parser)
            : PP.t
            =
            assert (has_ended p);
            assert (not (has_succeeded p));
            if Error.is_semantic (error p) then
                let range, error = Error.semantic (error p) in
                PP0.print_error_header "SYNTAX"
                <+>
                PP0.print_source source range []
                <+>
                PPr.problem error
            else
                let pos = position p
                and tabs = error_tabs p
                in
                PP0.print_error_header "SYNTAX"
                <+>
                PP0.print_source source (pos,pos) tabs
                <+>
                PPr.expectations
                    (column p)
                    (Error.expectations (error p))
                    tabs
    end
end (* Make *)
