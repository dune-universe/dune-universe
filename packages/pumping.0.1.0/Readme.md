# Pumping

Pumping is a library to enrich the OCaml type system in order to conveniently check regular languages.

The goal of type systems is to check that some expression respect some properties. More powerful type systems enrich the kind of properties that can be expressed.
On this regard, the OCaml type system is [quite powerful][generic-ppx].
Indeed, jacques Garrigue showed that [arbitrary Turing machines can be encoded
in GADTs][GADTexhaustiveness].
These methods allow to check very expressive properties, but are quite inconvenient to use. The *pumping* library allows to use properties that covers regular languages
by leveraging a well known tool: regular expressions.

[generic-ppx]: http://www.ocamlpro.com/2014/04/01/the-generic-syntax-extension/
[GADTexhaustiveness]: http://eptcs.web.cse.unsw.edu.au/paper.cgi?ML2015.2

## Regular expressions in types

Pumping introduces a new ppx annotation `re` (also available as `pumping.re`) that
allows to write regular expressions in types:
```ocaml
type t = [%re? Star (`a | `b)]
```

We can then check if a word is in this regular language. Words here are sequence of polymorphic variants terminated by `` `End``.
```ocaml
let x : t = `a (`a (`b `End))
```

Types produced by pumping are structural: we don't even need a type declaration!
```ocaml
let x : [%re? `a, (`b | `c)] = `a (`c `End)
```

Most of the usual regular expression operators are available, such as star and ranges.
```ocaml
let x
 : [%re? Star ('a'..'z'), (`Foo | `Bar)]
 = `f (`o (`o (`Bar `End)))
```

Intersection is also available:
```ocaml
(`B `End : [%re? (`A | `B) :: (`B | `C)])
```

## Convenient notation for regular expressions

Unfortunately, PPXs are limited by the OCaml syntax. The usual syntax for regular expressions
is short and well known by most people, but not easily encoded in the OCaml syntax.
In order to be able to express regular properties in an even more terse manner, pumping
also allows to use the POSIX notation for regular expressions:
```ocaml
let x : [%re"foo|bar"] = `foo `End
```

Most of the POSIX syntaxes are supported, including charsets, ranges and bounded repetition:
```ocaml
let x : [%re"[a-z]{2,6}"] = `p (`o (`t (`a (`t (`o `End)))))
```

A new operator, `&`, is introduced for the intersection:
```ocaml
let x : [%re"[ab]&[bc]"] = `b `End
```

## Equivalence of regular expressions

Since the type defined by pumping are structural, it is possible to use the typechecker
to test for the equivalence of regular expressions:
```ocaml
let f : [%re"aa*a"] -> [%re"a{2,}"] = fun x -> x
```

We can also leverage subtyping to test for the inclusion of regular expressions:
```ocaml
let f x = (x : [%re"a*"] :> [%re"[ab]*"])
```

## Implementation

The implementation is a fairly simple adaption of [Brzozowski derivative][deriv]. In
particular, it follows the technique described in the article "[Regular-expression derivatives reexamined][deriv2]".
Recursive types with (polymorphic) variants naturally describes a graph, thanks to
unification type variables and the `as` construction.

Indeed, `[%re"(OC|oc|c)(a)*ml"]` is simply:

```ocaml
[   `oc of 'v2
  | `c of 'v2
  | `OC of [ `ml of [ `End ] as 'v3  | `a of 'v2 ] as 'v2
] as 'v1
```

The typechecker then does a very good job at simplifying the types. Unfortunately, it also
unfolds the sub-parts of non-recursive types, which sometimes causes
an exponential blowup.

[deriv]: https://en.wikipedia.org/wiki/Brzozowski_derivative
[deriv2]: https://www.cl.cam.ac.uk/~so294/documents/jfp09.pdf

## Going further

Several features are yet missing in this implementation. Indeed, the expressivity of
regular languages is well known, but limited.

- In the style of PERL named regular expressions, it would be interesting to introduce
  annotation on types, in order to describe mutually dependent regular expressions:
  ```ocaml
  type%re t1 = [%re? Star `Foo, t2]
  and t2 = [%re? (`Bar | `Baz | t2)]
  ```
- It is unclear how to handle grouping, backreferences and lookaround constructions.
- Deterministic Pushdown automatons are fairly easy to encode (using a "counter" type), but
  a new convenient syntax would need to be proposed.
