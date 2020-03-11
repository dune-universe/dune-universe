open Fmlib
open Module_types
open Common
open Ast

module Located = Character_parser.Located
type 'a located = 'a Located.t

module Position = Character_parser.Position

type position = Character_parser.Position.t
type range = Position.t * Position.t








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
          range
          * string * string (* the 2 operator strings *)

      | Illegal_name of range * string (* expectation *)

      | Illegal_command of range * string list

      | Ambiguous_command of range * string list

      | Duplicate_argument of range
  end







module type SIG =
    sig
        type parser
        type final
        type _ t

        module Error: Generic_parser.ERROR with type expect = string
                                            and type semantic = Problem.t


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

        val expression: unit -> Expression.t t
        val command: Command.t t
        val make: final t -> parser
        val run: final t -> string -> parser
    end



let keywords: String_set.t =
  let open String_set in
  empty
  |> add "all"
  |> add "create"
  |> add "inspect"





module Make (Final: ANY) =
    struct
        module P =
          Character_parser.Advanced
            (Unit) (Final) (String) (Problem) (String)
        include P

        let string (str: string): unit t =
          P.string str (fun i -> "'" ^ String.one str.[i] ^ "'")


        let char (c:char): unit t =
          P.char c ("'" ^ String.one c ^ "'")


        let whitespace_char: char t =
          P.whitespace_char "whitespace"


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
          skip_zero_or_more
            ((map (fun _ -> ()) whitespace_char)
             <|> line_comment
             <|> multiline_comment)



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
                   fail (Problem.Illegal_name (Located.range s, "<identifier>"))
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
              (Problem.Illegal_name
                (Located.range name_located, "<argument name>"))
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

          let result_type: Expression.t t =
            colon |. whitespace >>= expression
          in

          let optional_result_type: Expression.t option t =
            optional result_type
          in

          let formal_argument: (string located * Expression.t option) t =
            (char_ws '(' >>= fun _ ->
             formal_argument_name >>= fun name ->
             colon |. whitespace >>= expression >>= fun typ ->
             char_ws ')' >>= fun _ ->
             return (name, Some typ)
            )
            <|> map (fun name -> name, None) formal_argument_name
          in

          let formal_arguments =
            one_or_more formal_argument >>= fun lst ->
            match find_duplicate_argument lst with
            | None ->
                return lst
            | Some name ->
                fail (Duplicate_argument (Located.range name))
          in


          let primary (): Expression.t t =
            backtrackable identifier_expression "identifier"
            <|> number_expression
            <|> literal_char
            <|> literal_string
            <|> ( ( char_ws '('
                    >>= fun _ ->
                    (* op_expression has to be encapsulated in a function,
                       otherwise infinite recursion!! *)
                    expression () <|> lonely_operator)
                  |. char_ws ')'
                )
            <|> located
                  (return (fun args rt exp -> Expression.Function (args, rt, exp))
                   |. char_ws '\\'
                   |= formal_arguments
                   |= optional_result_type
                   |= (assign |. whitespace >>= expression))
            <|> located
                  (return
                    (fun args rt ->
                        match Located.value rt with
                        | Expression.Product (args_inner, rt) ->
                            Expression.Product (args @ args_inner, rt)
                        | _ ->
                            Expression.Product (args, rt))
                  |. backtrackable (string "all") "all"
                  |. whitespace
                  |= formal_arguments
                  |= result_type)
            <?> "expression"
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

          (* expression parsing *)
          application >>= fun e1 ->

          zero_or_more operator_and_operand >>= fun lst ->

          match Expression.binary e1 lst with
          | Ok e ->
             return e

          | Error (range, op1, op2) ->
             fail (Problem.Operator_precedence (range, op1, op2))



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
                (Problem.Illegal_command
                    (Located.range cmd, command_names commands))

           | [_, arg_parser] ->
              arg_parser

           | lst ->
              fail
                (Problem.Ambiguous_command
                   (Located.range cmd, command_names lst))
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
            make (p |. expect_end "end of input") ()

        let run (p: final t) (input: string): parser =
            run p () input
    end (* Make *)
