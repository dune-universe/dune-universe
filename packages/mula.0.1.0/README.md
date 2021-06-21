# mula
ML's radishal library for matching with Universal Levenshtein Automata.

This library not only computes if strings are within a certain edit distance,
but also computes what the edit distance is.

We support both the standard Levenshtein distance as well as the
Demarau-Levenshtein distance which includes transpositions of two adjacent
characters as a primitive edit operation.

We can also lazily feed characters into atomata and get the current edit
distance.

Examples of use:
```ocaml
# #require "mula";;
# Mula.Strings.Lev.get_distance ~k:2 "abcd" "abdc";;
- : int option = Some 2
# Mula.Strings.Dem.get_distance ~k:2 "abcd" "abdc";;
- : int option = Some 1
# Mula.Strings.Lev.get_distance ~k:2 "abcd" "efgh";;
- : int option = None
# let lev_nfa = Mula.Strings.Lev.start ~k:2 ~str:"abcd";;
val lev_nfa : Mula.Strings.Lev.nfa_state = <abstr>
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"ab" |> current_error);;
- : int option = Some 2
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"abd" |> current_error);;
- : int option = Some 1
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"ab" |> feed_str ~str:"dc" |> current_error);; (* counts 'd' as an insert edit *)
- : int option = Some 1
# Mula.Strings.Lev.(feed_str lev_nfa ~str:"ab" |> feed_str ~str:"dc" |> end_input);;
- : int option = Some 2
```

### About the Name
মুলা (mula/moola) means radish in the author's first language.
