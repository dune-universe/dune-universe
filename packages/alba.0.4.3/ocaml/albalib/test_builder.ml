open Fmlib
open Alba_core
open Build_expression


module Pretty_printer = Pretty_printer.Pretty (String_printer)

module Term_print = Context.Pretty (Pretty_printer)

module Expression = Ast.Expression

module Expression_parser = Parser_lang.Make (Expression)

module Error_print = Build_problem.Print (Pretty_printer)



let standard_context: Context.t =
    Standard_context.make ()



let string_of_term_type (term: Term.t) (typ: Term.t): string
    =
    String_printer.run (
        Pretty_printer.run 0 70 70
            (Term_print.print (Term.Typed (term,typ)) standard_context))
let _ = string_of_term_type


let string_of_description (descr: Build_problem.description): string
    =
    String_printer.run (
        Pretty_printer.run 0 70 70
            (Error_print.description descr))
let _ = string_of_description



let build_expression
    (str: string)
    : (Term.t * Term.typ, Build_problem.t) result
    =
    let open Expression_parser in
    let p = run (expression ()) str in
    assert (has_ended p);
    assert (has_succeeded p);
    build Option.(value (result p)) standard_context




let%test _ =
    match build_expression "Proposition" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "Proposition: Any"
    | _ ->
        false



let%test _ =
    match build_expression "Any" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "Any: Any(1)"
    | _ ->
        false



let%test _ =
    match build_expression "Int" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "Int: Any"
    | _ ->
        false



let%test _ =
    match build_expression "abc" with
    | Error (_, No_name) ->
        true
    | _ ->
        false



let%test _ =
    match build_expression "Int -> all (B: Any): (Int -> B) -> B" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "Int -> (all (B: Any): (Int -> B) -> B): Any(1)"
    | _ ->
        false


let%test _ =
    match build_expression "identity" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "identity: all (A: Any): A -> A"
    | _ ->
        false



let%test _ =
    match build_expression "identity: Int -> Int" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "(identity: Int -> Int): Int -> Int"
    | _ ->
        false


let%test _ =
    match build_expression "Int -> String: Proposition" with
    | Error (_, Wrong_type _) ->
        true
    | _ ->
        false


let%test _ =
    let tp_str = "(Character -> String) -> String"
    in
    match build_expression ("(|>) 'a': " ^ tp_str)  with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "((|>) 'a': " ^ tp_str ^ "):\n    "  ^ tp_str
    | _ ->
        false


let%test _ =
    match build_expression "all a b: a = b" with
    | Error (_, Cannot_infer_bound) ->
        true
    | _ ->
        false


let%test _ =
    match build_expression "all a b: 'x' = b" with
    | Error (_, Cannot_infer_bound) ->
        true
    | _ ->
        false



let%test _ =
    match build_expression "all a (b: Int): a = b" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(all a (b: Int): a = b): Proposition"
    | _ ->
        false


let%test _ =
    match build_expression "(|>) \"A\" (+) \"a\"" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(|>) \"A\" (+) \"a\": String"
    | _ ->
        false


let%test _ =
    match build_expression "1 |> (+) 2" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "1 |> (+) 2: Int"
    | _ ->
        false


let%test _ =
    match build_expression "'a'= 'b'  " with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "'a' = 'b': Proposition"
    | _ ->
        false


let%test _ =
    match build_expression "1 + 2" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "1 + 2: Int"
    | _ ->
        false



let%test _ =
    match build_expression "all x: x + 2 = 3" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        = "(all x: x + 2 = 3): Proposition"
    | _ ->
        false



let%test _ =
    match build_expression "(\\x := x + 2) 1" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(\\ x := x + 2) 1: Int"
    | _ ->
        false


let%test _ =
    match build_expression "(\\x f := f x) 1 ((+) 2)" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(\\ x f := f x) 1 ((+) 2): Int"
    | _ ->
        false


let%test _ =
    match build_expression "(\\x y f := f x y) 1 2 (+)" with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(\\ x y f := f x y) 1 2 (+): Int"
    | _ ->
        false


let%test _ =
    match build_expression "\\ x y := x = y" with
    | Error (_, Cannot_infer_bound) ->
        true
    | _ ->
        false


let%test _ =
    match build_expression "(+) 1 2 3" with
    | Error (_, Not_a_function _) ->
        true
    | _ ->
        false



let%test _ =
    match
        build_expression
            "1 + a + b where\
            \n a := 8\
            \n b := 10"
    with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(1 + a + b where a := 8; b := 10): Int"
    | _ ->
        false



let%test _ =
    match
        build_expression
            "1234567890 + f 12345677890 where\
            \n f x := 20002000 + g x\
            \n g x := 1234567890 * x"
    with
    | Ok (term, typ) ->
        string_of_term_type term typ
        =
        "(1234567890 + f 12345677890 where\
        \n    f x := 20002000 + g x\
        \n    g x := 1234567890 * x):\
        \n    Int"
    | _ ->
        false



(* Ambiguous Expressions *)

let%test _ =
    match build_expression "(+)" with
    | Error (_, Ambiguous _ ) ->
        true
    | _ ->
        false


let%test _ =
    match build_expression "\\ x y := x + y" with
    | Error (_, Ambiguous _ ) ->
        true
    | _ ->
        false





(* Propositions *)

let%test _ =
    match build_expression "\\a := p: a => a where p x := x" with
    | Ok _ ->
        true
    | Error (_, description) ->
        Printf.printf "%s\n"
            (string_of_description description);
        false


let%test _ =
    match build_expression "\\a: a => a := identity" with
    | Ok _ ->
        true
    | Error (_, description) ->
        Printf.printf "%s\n"
            (string_of_description description);
        false


let%test _ =
    match build_expression "\\a b: a => b => a := \\ x y := x" with
    | Ok _ ->
        true
    | Error (_, description) ->
        Printf.printf "%s\n"
            (string_of_description description);
        false
