open Bark

(* Helpers *)

let parenthesize : string list -> string =
  fun xs ->
    "(" ^ String.concat " " xs ^ ")"

(* Specialized parser *)

type problem =
  | ExpectingEnd

  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingComma
  | ExpectingFloat

  | InvalidFloat

  | ExpectingTrue
  | ExpectingFalse
  | ExpectingUppercaseChar
  | ExpectingNot
  | ExpectingAmpersand
  | ExpectingPipe

let show_problem : problem -> string =
  fun p ->
    match p with
      | ExpectingEnd -> "expecting end of string"


      | ExpectingLeftParen -> "expecting ')"
      | ExpectingRightParen -> "expecting '('"
      | ExpectingComma -> "expecting ','"
      | ExpectingFloat -> "expecting a floating point number"

      | InvalidFloat -> "invalid floating point number"

      | ExpectingTrue -> "expecting literal 'true'"
      | ExpectingFalse -> "expecting literal 'false'"
      | ExpectingUppercaseChar -> "expecting uppercase character"
      | ExpectingNot -> "expecting keyword 'not'"
      | ExpectingAmpersand -> "expecting '&'"
      | ExpectingPipe -> "expecting '|'"

type context =
  unit

type 'a parser =
  (context, problem, 'a) Bark.parser

type 'a test_suite =
 'a parser * (string * ('a, problem option) result) list

let show_dead_end : (context, problem) dead_end -> string =
  fun { row; col; problem; _ } ->
    "[row=" ^ string_of_int row ^ ", col=" ^ string_of_int col ^ "] "
      ^ show_problem problem

(* Points *)

type point =
  { x : float
  ; y : float
  }

let point : point parser =
  succeed (fun x y -> {x; y})
    |. symbol (Token ("(", ExpectingLeftParen))
    |. spaces
    |= (Bark.float ExpectingFloat InvalidFloat)
    |. spaces
    |. symbol (Token (",", ExpectingComma))
    |. spaces
    |= (Bark.float ExpectingFloat InvalidFloat)
    |. spaces
    |. symbol (Token (")", ExpectingRightParen))

let show_point : point -> string =
  fun {x; y} ->
    parenthesize ["point"; string_of_float x; string_of_float y]

let point_tests : point test_suite =
  ( point
  , [ ("(21.5, 67.0)", Ok { x = 21.5; y = 67.0 })
    ; ("(21.5  67.0)", Error (Some ExpectingComma))
    ; ("(21.5, 67.0", Error (Some ExpectingRightParen))
    ; ("  (   21.5  ,   67.0  )  ", Error (Some ExpectingLeftParen))
    ; ("(   1.0  , \n\n  2116.0  \n )  ", Ok { x = 1.0; y = 2116.0 } )
    ]
  )

(* Boolean expressions *)

type bool_expr =
  | True
  | False
  | Variable of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec bool_expr' : unit -> bool_expr parser =
  fun () ->
    one_of
      [ succeed True
          |. keyword (Token ("true", ExpectingTrue))
      ; succeed False
          |. keyword (Token ("false", ExpectingFalse))
      ; map (fun s -> Variable s) @@
          get_chomped_string @@
            chomp_if
              (function | 'A' .. 'Z' -> true | _ -> false)
              ExpectingUppercaseChar
      ; succeed (fun b -> Not b)
          |. keyword (Token ("not", ExpectingNot))
          |. spaces
          |= lazily bool_expr'
      ; succeed (fun b1 op b2 -> op b1 b2)
          |. symbol (Token ("(", ExpectingLeftParen))
          |. spaces
          |= lazily bool_expr'
          |. spaces
          |= one_of
               [ succeed (fun b1 b2 -> And (b1, b2))
                   |. symbol (Token ("&", ExpectingAmpersand))
               ; succeed (fun b1 b2 -> Or (b1, b2))
                   |. symbol (Token ("|", ExpectingPipe))
               ]
          |. spaces
          |= lazily bool_expr'
          |. spaces
          |. symbol (Token (")", ExpectingRightParen))
      ]

let bool_expr : bool_expr parser =
  bool_expr' ()

let rec show_bool_expr : bool_expr -> string =
  function
    | True -> "true"
    | False -> "false"
    | Variable s -> s
    | Not b -> "not " ^ show_bool_expr b
    | And (b1, b2) -> "(" ^ show_bool_expr b1 ^ " & " ^ show_bool_expr b2 ^ ")"
    | Or (b1, b2) -> "(" ^ show_bool_expr b1 ^ " | " ^ show_bool_expr b2 ^ ")"

let bool_expr_tests : bool_expr test_suite =
  ( bool_expr
  , [ ("true", Ok True)
    ; ("false", Ok False)
    ; ("false true", Error (Some ExpectingEnd))
    ; ("true | false", Error (Some ExpectingEnd))
    ; ("(true)", Error None)
    ; ( "(not (true | false) & P)"
      , Ok (And (Not (Or (True, False)), Variable "P"))
      )
    ; ( "(not (true | false) & p)"
      , Error None
      )
    ; ( "(not (true | false) & PQ)"
      , Error None
      )
    ]
  )

(* Testing *)

let finalize : 'a parser -> 'a parser =
  fun parse ->
    succeed (fun x -> x)
      |= parse
      |. spaces
      |. endd ExpectingEnd

let parse_and_show : 'a parser -> ('a -> string) -> string -> string =
  fun parse show input ->
    match run parse input with
      | Ok x ->
          show x

      | Error dead_ends ->
          "Dead ends:\n"
            ^ String.concat "\n" (List.map show_dead_end dead_ends)

let test_one : 'a parser -> string * ('a, problem option) result -> unit =
  fun parse (input, expect) ->
    let pass =
      match (run (finalize parse) input, expect) with
        | (Ok output, Ok correct_output) ->
            output = correct_output

        | (Error dead_ends, Error correct_problem_opt) ->
            begin match correct_problem_opt with
              | Some correct_problem ->
                  begin match dead_ends with
                    | [{ problem; _ }] ->
                        problem = correct_problem

                    | _ ->
                        false
                  end

              | None ->
                  true
            end

        | _ ->
            false
    in
      if not pass then
        print_endline @@
          "Test failure: " ^ input
      else
        ()

let test : 'a test_suite -> unit =
  fun (parse, tests) ->
    ignore @@
      List.map (test_one parse) tests

let _ =
  test point_tests;
  test bool_expr_tests
