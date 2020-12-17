open Alba_core
open Ast


module Inductive_parser =
    Parser_lang.Make (
        struct
            type t = Source_entry.inductive array
        end)


module Expression_parser =
    Parser_lang.Make (Expression)


module Definition_parser =
    Parser_lang.Make (
        struct
            type t = Expression.definition
        end)




let add_definition (src: string) (c: Context.t): Context.t =
    let open Definition_parser in
    let p = run (global_definition ()) src in
    assert (has_ended p);
    assert (has_succeeded p);
    match result p with
    | None ->
        assert false (* Cannot happen. *)
    | Some def ->
        match Builder.add_definition def c with
        | Error _ ->
            assert false
        | Ok c ->
            c





let add_inductive (src: string) (c: Context.t): Context.t =
    let open Inductive_parser in
    let p = run (inductive_family ()) src in
    assert (has_ended p);
    assert (has_succeeded p);
    match result p with
    | None ->
        assert false (* Cannot happen. *)
    | Some def ->
        match Builder.add_inductive def c with
        | Error _ ->
            Printf.printf "Standard_context.add_inductive\n%s\n" src;
            assert false
        | Ok c ->
            c


let add_builtin
    (fun_flag: bool)
    (descr: string) (name: string) (src: string) (c: Context.t):
    Context.t
=
    let open Expression_parser in
    let p = run (expression ()) src in
    assert (has_ended p);
    assert (has_succeeded p);
    match result p with
    | None ->
        assert false (* Cannot happen. *)
    | Some exp ->
        match Build_expression.build exp c with
        | Error _ ->
            assert false
        | Ok (typ, _) ->
            if fun_flag then
                Context.add_builtin_function descr name typ c
            else
                Context.add_builtin_type descr name typ c






let add_logic (c: Context.t): Context.t =
    c
    |>
    add_definition
        "Predicate (A: Any): Any := A -> Proposition"
    |>
    add_definition
        "Relation (A: Any) (B: Any): Any := A -> B -> Proposition"
    |>
    add_definition
        "Endorelation (A: Any): Any := Relation A A"
    |>
    add_inductive
        "class (=) (A: Any) (a: A): Predicate A := identical: a = a"
    |>
    add_definition
        "(=>) (a: Proposition) (b: Proposition): Proposition :=\
        \n  a -> b"
    |>
    add_inductive
        "class false: Proposition :="
    |>
    add_definition
        "(not) (a: Proposition): Proposition := a => false"
    |>
    add_inductive
        "class true: Proposition := trueValid"
    |>
    add_inductive
        "class (and) (a: Proposition) (b: Proposition): Proposition :=\
        \n    (,): a => b => a and b"
    |>
    add_inductive
        "class (or) (a: Proposition) (b: Proposition): Proposition :=\
        \n    left:  a => a or b\
        \n    right: b => a or b"
    |>
    add_inductive
        "class exist (A: Any) (f: A -> Proposition): Proposition :=\
        \n    witness (a: A): f a => exist f"




let add_basics (c: Context.t): Context.t =
    c
    |>
    add_definition
        "identity (A: Any) (a: A): A := a"
    |>
    add_definition
        "(|>) (A: Any) (a: A) (F: A -> Any) (f: all x: F x): F a := f a"
    |>
    add_definition
        "(<|) (A: Any) (F: A -> Any) (f: all x: F x) (a: A): F a := f a"
    |>
    add_definition
        "(>>) (A: Any) (B: Any) (C: Any) (f: A -> B) (g: B -> C): A -> C :=
        \n    \\x := g (f x)"
    |>
    add_definition
        "(<<) (A: Any) (B: Any) (C: Any) (f: B -> C) (g: A -> B): A -> C :=
        \n    \\x := f (g x)"
    |>
    add_inductive
        "class Decision (a: Proposition) (b: Proposition): Any := \
        \n    left:  a -> Decision a b
        \n    right: b -> Decision a b"
    |>
    add_inductive
        "class Maybe (A: Any): Any := \
        \n    nothing: Maybe A\
        \n    just : A -> Maybe A"
    |>
    add_inductive
        "class List (A: Any): Any := \
        \n    []: List A\
        \n    (+:): A -> List A -> List A"




let add_builtins (c: Context.t): Context.t =
    c
    |>
    add_builtin false "int_type" "Int" "Any"
    |>
    add_builtin true  "int_plus" "+" "Int -> Int -> Int"
    |>
    add_builtin true  "int_minus" "-" "Int -> Int -> Int"
    |>
    add_builtin true  "int_negate" "-" "Int -> Int"
    |>
    add_builtin true "int_times" "*" "Int -> Int -> Int"
    |>
    add_builtin false "string_type" "String" "Any"
    |>
    add_builtin true  "string_concat" "+" "String -> String -> String"
    |>
    add_builtin false "character_type" "Character" "Any"





let make (): Context.t =
    Context.empty
    |>
    add_logic
    |>
    add_builtins
    |>
    add_basics



let _ = make ()
