(** Parser for the intermediate language. *)

(**

   The intermediate language is lisp like i.e. easy to parse without any
   syntactic sugar. All binders must be fully typed and there are no implicit
   arguments.

   Expressions are built according to the syntax:

   {v
        term ::=
            |   atom
            |   literal
            |   application
            |   function-type
            |   function-abstraction
            |   pattern-match
            |   fixpoint
            |   mutual-fixpoint

        application ::=
            (app term term+)

        function-type ::=
            (all (name: term)+ : term)

        function-abstraction ::=
            (lambda (name: term)+ . term)


        atom ::= identifier | operator | deBruijn | literal

        name ::=
            | identifier
            | _

        pattern-match ::=
            (case term term+)    -- elimination function, list of functions

        fixpoint ::=
            (fix name number (name: term)+ : term . term)
                -- decr argument, args, result type, def term
   v}



   Declarations are built according to the syntax:

   {v
        declaration ::=
            | builtin
            | definition
            | inductive

        builtin ::=
            (builtin string (name: term)* : term)

        definition ::=
            (def string (name: term)* : term term)

        inductive ::=
            (class string (name: term)* : term constructor* )

        constructor ::=
            (string constructor-argument* (: term+)? )

        constructor-argument ::=
            (name: ( term* ) term)

        mutual-inductive ::=
            (mutual-ind inductive+)
    v}

   A constructor type must always construct the corresponding inductive type.
   The type and the parameters are the same for all constructors of the type.
   The indices (if present) can be different. Therefore only the indices are
   present as an optional [: term+].

   The types of the constructor arguments are either simple types or function
   types. Therefore they consist of a (possibly empty) list of types and a
   mandatory type.

   Some examples:
   {v
        (all (A: Any) (a: A): A)


        (def => (a: Proposition) (b: Proposition): Proposition
            (all (_: a): b))


        (builtin Int: Any)

        (class false: Proposition)

        (class true: Proposition (trueValid))

        (class List (A: Any): Any
            ([])
            (+:
                (_: () A)
                (_: () (app List A))
            ))

        (class Natural (): Any
            (zero)
            (succ (_: Natural)))

        (def pred (n: Natural): Natural
            (case
                (lambda (_: Natural) . Natural)
                zero
                (lambda (m: Natural) . m)))

        (def isSucc: (all (_: Natural): Proposition)
            (case
                (lambda (_: Natural) . Proposition)
                (false)
                (lambda (_: Natural) . true)))

    v}

 *)


open Fmlib
open Module_types



type range = Character_parser.Located.range




module Make (Final: ANY):
sig
    type _ t (** Type of a parser combinator. *)

    type p  (** Type of the parser. *)

    val needs_more: p -> bool

    val has_succeeded: p -> bool

    val has_failed: p -> bool

    val state: p -> Welltyped.context

    val put_character: p -> char -> p

    val put_end: p -> p

    val run: Final.t t -> Welltyped.context -> string -> p

    val result: p -> Final.t option

    val make: Final.t t -> Welltyped.context -> p

    val judgement: Welltyped.judgement t
end


