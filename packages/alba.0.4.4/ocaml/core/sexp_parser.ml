open Fmlib
open Module_types
open Common


module Located = Character_parser.Located

type range = Position.range

type 'a located = range * 'a


let join_ranges ((pos1,_): range) ((_,pos2)): range =
    pos1, pos2



module Builder = Welltyped.Builder (struct type t = range end)

type builder = range * Builder.t




module Make (Final: ANY) =
struct
    module State =
        struct
            type t = Welltyped.context
        end

    module Semantic =
        struct
            type t = range * Type_error.t
        end


    include Character_parser.Normal (State) (Final) (Semantic) (Unit)

    type p = parser

    type term_tag =
      | Application
      | Lambda
      | Pi


    let term_tags: term_tag String_map.t =
        String_map.(empty
                    |> add "app" Application
                    |> add "all" Pi
                    |> add "lambda" Lambda)


    type declaration_tag =
      | Builtin
      | Definition
      | Class


    let declaration_tags: declaration_tag String_map.t =
        String_map.(
            empty
            |> add "builtin" Builtin
            |> add "def"     Definition
            |> add "class"   Class
      )
    let _ = declaration_tags



    let located (p: 'a t): 'a located t =
        map
            (fun res ->
                 let v = Located.value res
                 and range = Located.range res
                 in
                 range, v)
            (located p)


    let whitespace_char: char t =
        expect
            (fun c -> c = ' ' || c = '\n' || c = '\t')
            "space, newline or tab"


    let whitespace: int t =
        skip_zero_or_more whitespace_char


    let raw_name: string t =
        word
            Char.is_letter
            (fun c -> Char.is_letter c || Char.is_digit c || c = '_')
            "identifier"


    let name: string located t =
        located raw_name


    let name_ws: string located t =
        name |. whitespace

    let char_ws (c: char): unit t =
        char c |. whitespace

    let left_paren_ws: unit t =
        char_ws '('


    let right_paren_ws: unit t =
        char_ws ')'


    let assign_ws: unit t =
        string ":=" |. whitespace


    let parenthesized_located (p: unit -> 'a t): 'a located t =
        return
            (fun (r1, _) v (r2, _) ->
                 join_ranges r1 r2, v)
        |= located (char_ws '(')
        |== p
        |= located (char_ws ')')


    let parenthesized (p: unit -> 'a t): 'a t =
        (return identity)
        |. left_paren_ws
        |== p
        |. right_paren_ws


    let operator_characters: string = "+-^*|/=~<>"


    let is_operator_character (c: char): bool =
        String.has (fun d -> c = d) 0 operator_characters


    let operator: string located t =
        located (word
                     is_operator_character
                     is_operator_character
                     "operator character")
        |. whitespace
    let _ = operator


    let number: string located t =
        located (word Char.is_digit Char.is_digit "digit")
        |. whitespace
    let _ = number





    let some_tag (expecting: string) (map: 'a String_map.t): 'a located t =
        (backtrackable
            (name_ws >>= fun (range, tag) ->
             match String_map.maybe_find tag map with
             | None ->
                 unexpected expecting
             | Some tag ->
                 return (range,tag))
            expecting)
        |. whitespace


    let parenthesized_tagged
            (expecting: string)
            (map: 'a String_map.t)
            (p: 'a located -> 'b t)
        : 'b t
        =
        parenthesized
            (fun _ ->
                 some_tag expecting map
                 >>=
                 p)




    let atom: builder t =
        map
            (fun (range,name) -> range, Builder.identifier range name)
            name_ws



    let rec expression _: builder t =
        atom
        <|>
        compound ()

    and compound _: builder t =
        parenthesized_tagged
            "<term tag>"
            term_tags
            (fun (range, tag) ->
                match tag with
                | Application ->
                    application range
                | Pi ->
                    pi range
                | Lambda ->
                    assert false)

    and application (_: Located.range): builder t =
        let make_application (r1, f) (r2, arg) =
            let range = join_ranges r1 r2 in
            range, Builder.application range f arg
        in
        return
            (fun f arg args ->
                 assert (args = []); (* to be removed *)
                 List.fold_left
                     make_application
                     (make_application f arg)
                     args)
        |== expression
        |== expression
        |= zero_or_more (expression ())

    and pi (_: range): builder t =
        return
             (fun fargs res ->
                assert (fargs <> []);
                List.fold_right
                    (fun (r1, (name,arg_typ)) (r2, res_typ) ->
                         let range = join_ranges r1 r2 in
                         range,
                         Builder.pi
                             range name arg_typ res_typ)
                    fargs
                    res)
        |== formal_arguments
        |. char_ws ':'
        |= result_type ()

    and result_type _: builder t =
        expression ()

    and formal_arguments _: Builder.formal_argument located list t =
        zero_or_more (parenthesized_located formal_argument)

    and formal_argument _: Builder.formal_argument t =
        (return (fun name (_, typ) -> name, typ))
        |= name_ws
        |. char_ws ':'
        |== expression

    and signature _: Builder.signature t =
        return
            (fun fargs (_, res) ->
                 List.map snd fargs,
                 res)
        |== formal_arguments
        |. char_ws ':'
        |== expression



    let judgement: Welltyped.judgement t
        =
        expression () >>= fun (_, expr) ->
        get_state >>= fun context ->
        match
            (Builder.make_term context expr)
        with
        | Ok jm ->
            return jm
        | Error error ->
            fail error


    let declaration _: unit t =
        parenthesized_tagged
            "<declaration tag>"
            declaration_tags
            (fun (r1, tag) ->
                match tag with
                | Builtin ->
                    get_state >>= fun context ->
                    name_ws >>= fun name ->
                    signature () >>= fun sign ->
                    (
                        match Builder.make_builtin context name sign with
                        | Ok context ->
                            put_state context
                        | Error error ->
                            fail error
                    )
                | Definition ->
                    get_state >>= fun context ->
                    name_ws >>= fun name ->
                    signature () >>= fun sign ->
                    assign_ws >>= fun _ ->
                    expression () >>= fun (r2,exp) ->
                    (
                        let range = join_ranges r1 r2 in
                        match
                            Builder.make_definition
                                range name sign exp context
                        with
                        | Ok context ->
                            put_state context
                        | Error error ->
                            fail error
                    )
                | Class ->
                    assert false)


    let declarations _: unit t =
        map
            (fun n ->
                 Printf.printf "%d declarations parsed\n" n;
                 ())
            (skip_zero_or_more
                 (declaration ()))


    let run_string
            (p: Final.t t) (c: Welltyped.context) (src: string)
        : p
        =
        run (p |. expect_end) c src

    (* avoid warnings *)
    let _ = run_string
    let _ = declarations
end








(* --------------------------------------------------------------------- *)
(* Unit tests *)
(* --------------------------------------------------------------------- *)


module Expression_parser =
struct
    include Make (struct type t = builder end)
end


module Context_parser =
struct
    include Make (Unit)
end


let build_expression
        (src: string)
        (c: Welltyped.context)
    : (Welltyped.judgement, Builder.problem) result =
    let open Expression_parser in
    let p = run_string (expression ()) c src in
    assert (has_ended p);
    if not (has_succeeded p) then
    (
        let module PP = Pretty_printer.Pretty (String_printer) in
        let err = error p in
        if Error.is_semantic err then
            assert false
        else (
            let module Source_print = Position.Print (PP) in
            let pos = position p in
            Printf.printf "%s\n"
                (String_printer.run
                     (PP.run 0 70 70
                          (Source_print.print_source src (pos,pos))));
        )

    );
    assert (has_succeeded p);
    Builder.make_term
        c
        (snd (Option.value (result p)))



let build_expression_empty
        (src: string)
    : (Welltyped.judgement, Builder.problem) result
    =
    build_expression src Welltyped.empty


let build_context
        (src: string)
        (c: Welltyped.context)
    : Welltyped.context
    =
    let open Context_parser in
    let p = run_string (declarations ()) c src in
    assert (has_ended p);
    if not (has_succeeded p) then
    (
        let module PP = Pretty_printer.Pretty (String_printer) in
        let module Source_print = Position.Print (PP) in
        let to_string (pp: PP.t): string =
            String_printer.run (PP.run 0 70 70 pp)
        in
        let source_to_string src range =
            to_string (Source_print.print_source src range)
        in
        let err = error p in
        if Error.is_semantic err then
            let range, error = Error.semantic err in
            let module Print = Type_error.Print (PP) in
            Printf.printf "%s\n\n%s\n"
                (source_to_string src range)
                (to_string (Print.print error))
        else (
            let pos = position p in
            Printf.printf "%s\n" (source_to_string src (pos,pos))
        )
    );
    assert (has_succeeded p);
    state p
let _ = build_context



let is_term_ok (print: bool) (src: string): bool =
    let module PP = Pretty_printer.Pretty (String_printer) in
    let to_string (pp: PP.t): string =
        String_printer.run (PP.run 0 70 70 pp)
    in
    match
        build_expression src Welltyped.empty
    with
    | Ok jm ->
        let module Print = Welltyped.Print (PP) in
        if print then
            Printf.printf "%s\n" (to_string (Print.judgement jm));
        true
    | Error (range, error) ->
        let module Print = Type_error.Print (PP) in
        let module Source_print = Position.Print (PP) in
        Printf.printf "%s\n\n%s\n"
            (to_string (Source_print.print_source src range))
            (to_string (Print.print error));
        false


(* Some simple expressions *)
let%test _ =
    is_term_ok false "Any"


let%test _ =
    is_term_ok false "Proposition"



let%test _ =
    is_term_ok false "(all (A:Any) (a:A): A)"




let%test _ =
    is_term_ok false "(all (a:Proposition): Proposition)"


let%test _ =
    is_term_ok false "(all (a:Proposition)(x:a): a)"


let%test _ =
    is_term_ok false "(all (A:Any) (x:A) (a:Proposition): a)"


let%test _ =
    is_term_ok
        false
        "(all (A: Any) \
         \n  (F: (all (y: A): Any))\
         \n  (a: A)\
         \n  (f: (all (x: A): (app F x)))\
         \n  : (app F a))"



(* Failure cases *)

let%test _ =
    match
        build_expression_empty "(all (A:Any) (a:A): a)"
    with
    | Error (_, Type_error.Not_a_type) ->
        true
    | _ ->
        false

let%test _ =
    match
        build_expression_empty "(all (a:Any): Any)"
    with
    | Error (_, Type_error.Naming_type_variable) ->
        true
    | _ ->
        false


let%test _ =
    match
        build_expression_empty "(all (A:Any) (B:A): A)"
    with
    | Error (_, Type_error.Naming_no_type_variable) ->
        true
    | _ ->
        false


let%test _ =
    match
        build_expression_empty "(all (A:Proposition): A)"
    with
    | Error (_, Type_error.Naming_no_type_variable) ->
        true
    | _ ->
        false




let%test _ =
    match
        build_expression_empty "(all (A:Any) (a:A) (x:a): A)"
    with
    | Error (_, Type_error.Not_a_type) ->
        true
    | _ ->
        false


let%test _ =
    match
        build_expression_empty "(app Any Any)"
    with
    | Error (_, Type_error.Not_a_function (_, _)) ->
        true
    | _ ->
        false


let%test _ =
    match
        build_expression_empty
            "(all (A: Any) (F: (all (x: A): Any)): (app F A))"
    with
    | Error (_, Type_error.Wrong_type (_, _, _)) ->
        true
    | _ ->
        false





(* New tests *)




(* Filling the context *)
(*
let%test _ =
    let _ = build_context
            "(def identity (A: Any) (x: A): A := x)"
            Welltyped.empty in
    true*)
