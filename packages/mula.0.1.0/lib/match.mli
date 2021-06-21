(** {1 Universal Levenshtein Automata}
    The {{:https://hal.archives-ouvertes.fr/hal-01360482/file/LATA2016.pdf}paper
    by Touzet} details the Universal Levenshtein Automata.

    Some nice computational properties of the (not-deterministic) automata include:
    - There are no epsilon transitions.
    - The automata are computable on demand, there is no need to store states and transitions in a data structure.
    - The states of the automata carry error counts.
    - There is a simple subsumption relation that prunes sets of states, so transitions produce small sets of states.

    These allow for efficient implementation together with interesting uses.
    {ol
      {- We not only know if two strings are within a certain edit distance, but
         we also know what the edit distance is if they are within the edit
         distance limit.}
      {- If several strings are compared, we can rank them by their edit
         distance.}
      {- We can lazily feed characters and string fragments to the atomata and
         get the current error count.}
    }
    *)

(** This module offers a functor to build matchers for different representations
    of strings and characters. For a prebuilt matcher for OCaml strings and
    characters, see {!module:Strings}. *)

(** {2 String Abstraction}
    We abstract over strings and characters so that we do not rely on any
    specific encoding.
    We only need the following:
    - a function to compute length of strings,
    - a function to fetch a character at an index, and
    - a function to check if two characters are equal.
    *)

module type S =
  sig
  (** {1 String Abstraction}
      We abstract over strings and characters so that we do not rely on any
      specific encoding.
      We only need the following:
      - a function to compute length of strings,
      - a function to fetch a character at an index, and
      - a function to check if two characters are equal.
      *)

    (** The type for characters *)
    type ch

    (** The type for strings *)
    type t

    (** [length s] should compute the length of the string [s] *)
    val length : t -> int

    (** [get s i] should fetch the character at index [i] of a string [s] *)
    val get : t -> int -> ch

    (** [equal c1 c2] should return true if [c1] and [c2] are equal, and false otherwise. *)
    val equal : ch -> ch -> bool
  end

(** {2 Levenshtein Automata}
    Given a string representation, we produce two Automata:
    - {!module:Make.Lev}, for the standard Levenshtein distance.
    - {!module:Make.Dem}, for the Demarau-Levenshtein distance which
      includes transpositions as a primitve edit operation.
    *)

module Make (St : S) :
    sig
    (** {1 Levenshtein Automata}
        Given a string representation, we produce two Automata:
        - {!module:Lev}, for the standard Levenshtein distance.
        - {!module:Dem}, for the Demarau-Levenshtein distance which
          includes transpositions as a primitve edit operation.
        *)

      module Lev : sig
        (** {1 Standard Universal Levenshtein Automaton} *)

        (** Abstract type for the state of the atomata. *)
        type nfa_state

        (** [start ~k ~str] produces the starting state of the atomaton for edit distance [k].
            Here [k] must not be negative and must not be greater [(Sys.int_size - 1) / 2]. *)
        val start : k:int -> str:St.t -> nfa_state

        (** [feed nfa ch] produces a new state where the atomaton has been fed
            the character [ch]. *)
        val feed : nfa_state -> ch:St.ch -> nfa_state

        (** [current_error nfa] produces [Some n] if the current error count
            recorded by the nfa is a number [n], or [None] if the error count is
            larger than the limit [k] that the nfa was started with.
            [current_error nfa] may be smaller than [end_input nfa], since it
            does not account for delete operations at the end of the fed string.
        *)
        val current_error : nfa_state -> int option

        (** [end_input nfa] computes the edit distance between the starting
            string and the string fed to [nfa]. It produces [Some n] if the edit
            distance in a number [n] that is less than the limit [k] that the
            automaton was started with, and [None] otherwise. *)
        val end_input : nfa_state -> int option

        (** [feed_str nfa ~str] produces a new state where the atomaton has been fed
            the characters from the string [str]. *)
        val feed_str : nfa_state -> str:St.t -> nfa_state

        (** [get_distance ~k str1 str2] computes the edit distance between two
            strings. It creates an atomaton with limit [k] and [str1], and then
            feeds it the string [str2], and thet outputs the result of calling
            [end_input] on the nfa. *)
        val get_distance : k:int -> St.t -> St.t -> int option
      end
      module Dem : sig
        (** {1 Universal Demarau-Levenshtein Automaton} *)

        (** Abstract type for the state of the atomata. *)
        type nfa_state

        (** [start ~k ~str] produces the starting state of the atomaton for edit distance [k].
            Here [k] must not be negative and must not be greater [(Sys.int_size - 1) / 2]. *)
        val start : k:int -> str:St.t -> nfa_state

        (** [feed nfa ch] produces a new state where the atomaton has been fed
            the character [ch]. *)
        val feed : nfa_state -> ch:St.ch -> nfa_state

        (** [current_error nfa] produces [Some n] if the current error count
            recorded by the nfa is a number [n], or [None] if the error count is
            larger than the limit [k] that the nfa was started with.
            [current_error nfa] may be smaller than [end_input nfa], since it
            does not account for delete operations at the end of the fed string.
        *)
        val current_error : nfa_state -> int option

        (** [end_input nfa] computes the edit distance between the starting
            string and the string fed to [nfa]. It produces [Some n] if the edit
            distance in a number [n] that is less than the limit [k] that the
            automaton was started with, and [None] otherwise. *)
        val end_input : nfa_state -> int option

        (** [feed_str nfa ~str] produces a new state where the atomaton has been fed
            the characters from the string [str]. *)
        val feed_str : nfa_state -> str:St.t -> nfa_state

        (** [get_distance ~k str1 str2] computes the edit distance between two
            strings. It creates an atomaton with limit [k] and [str1], and then
            feeds it the string [str2], and thet outputs the result of calling
            [end_input] on the nfa. *)
        val get_distance : k:int -> St.t -> St.t -> int option
      end
    end
