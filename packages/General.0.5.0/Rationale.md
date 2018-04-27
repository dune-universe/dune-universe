Rationale
=========

Function aliases and partial applications
-----------------------------------------

Don't use `let f = g`. Use `let f x ~y = g x ~y`.

Why:

- to allow measuring test coverage
- this is required in some cases to help the type system (Error: The type of this expression, ... contains type variables that cannot be generalized), so let's do this consistently

Why not:

- this is more verbose

Remarks

- this adds a stack frame. Sometimes it's ok, sometimes not.

Short variable names
--------------------

Don't use `a`, `b`, `c`. Use `x`, `y`, `z`, `s`, `t`, `u`, `v`, `w`.

Why:

- the plural form of `a` (ie. `as`) is a keyword

Functions that accept messages
------------------------------

Don't accept a plain `string`. Accept a `(...) format`.

Why:

- This will avoid using `f (Sprintf format ...)` and allow `f format ...`
- `Printf.ksprintf` is life, `Printf.ksprintf` is good for you

General template:

    let f x y z format =
        Printf.ksprintf
            (fun message u v w ->
                plain_f x y z message u v w
            )
            format

    let () = f x y z "%s%n" "x" 4 u v w
