# mula
ML's radishal library for matching with Universal Levenshtein Automata.

This library not only computes if strings are within a certain edit distance,
but also computes what the edit distance is.

We support both the standard Levenshtein distance as well as the
Demarau-Levenshtein distance which includes transpositions of two adjacent
characters as a primitive edit operation.

We can also lazily feed characters into automata and get the current edit
distance.

For OCaml strings, we offer the `Mula.Strings` module which contains submodules
`Lev` for the standard Levenshtein distance and `Dem` for the (restricted)
Demarau-Levenshtein distance.

Examples of use:
```ocaml
# #require "mula";;
# Mula.Strings.Lev.get_distance ~k:2 "abcd" "abdc";;
- : int option = Some 2
# Mula.Strings.Dem.get_distance ~k:2 "abcd" "abdc";;
- : int option = Some 1
# Mula.Strings.Lev.get_distance ~k:2 "abcd" "efgh";;
- : int option = None
```

We can also lazily feed characters and strings into an `nfa` and get live error counts:
```ocaml
# let lev_nfa = Mula.Strings.Lev.start ~k:2 ~str:"abcd";;
val lev_nfa : Mula.Strings.Lev.nfa_state = <abstr>
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"ab" |> current_error);;
- : int option = Some 0
# Mula.Strings.Lev.(feed lev_nfa ~ch:'a' |> feed ~ch:'b' |> feed ~ch:'c' |> current_error);;
- : int option = Some 0
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"abd" |> current_error);;
- : int option = Some 1
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"ab" |> feed_str ~str:"dc" |> current_error);; (* counts 'd' as an insert edit *)
- : int option = Some 1
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"ab" |> feed_str ~str:"dc" |> end_input);;
- : int option = Some 2
```

`Mula` also offers a functor if you want to use your own representations of
strings:
```ocaml
# #require "mula";;
# module St = struct
  type ch = int
  type t = int array

  let length = Array.length
  let get = Array.get

  let equal = Int.equal
end;;
module St :
  sig
    ...
  end
# module M = Mula.Match.Make(St);;
module M :
  sig
    module Lev :
      sig
        type nfa_state = Mula.Match.Make(St).Lev.nfa_state
        val start : k:int -> str:St.t -> nfa_state
        val feed : nfa_state -> ch:int -> nfa_state
        val current_error : nfa_state -> int option
        val end_input : nfa_state -> int option
        val feed_str : nfa_state -> str:St.t -> nfa_state
        val get_distance : k:int -> St.t -> St.t -> int option
      end
    module Dem :
      sig
        ...
      end
  end
```

### About the Name
মুলা (mula/moola) means radish in the author's first language.
