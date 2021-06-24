(** {1 Universal Levenshtein Automata for OCaml strings}*)

(** We provide two kinds of Automata:
    - {!module:Lev}, for the standard Levenshtein distance.
    - {!module:Dem}, for the Demarau-Levenshtein distance which
      includes transpositions as a primitve edit operation.
*)

module Lev : sig

  (** Abstract type for the state of the automata. *)
  type nfa_state

  (** [start ~k ~str] produces the starting state of the automaton for edit
      distance [k].  Here [k] must not be negative and must not be greater
      [(Sys.int_size - 1) / 2]. *)
  val start : k:int -> str:string -> nfa_state

  (** [feed nfa ch] produces a new state where the automaton has been fed the
      character [ch]. *)
  val feed : nfa_state -> ch:char -> nfa_state

  (** [current_error nfa] produces [Some n] if the current error count recorded
      by the nfa is a number [n], or [None] if the error count is larger than
      the limit [k] that the nfa was started with.  [current_error nfa] may be
      smaller than [end_input nfa], since it does not account for delete
      operations at the end of the fed string. *)
  val current_error : nfa_state -> int option

  (** [end_input nfa] computes the edit distance between the starting string and
      the string fed to [nfa]. It produces [Some n] if the edit distance in a
      number [n] that is less than the limit [k] that the automaton was started
      with, and [None] otherwise. *)
  val end_input : nfa_state -> int option

  (** [feed_str nfa ~str] produces a new state where the automaton has been fed
      the characters from the string [str]. *)
  val feed_str : nfa_state -> str:string -> nfa_state

  (** [get_distance ~k str1 str2] computes the edit distance between two
      strings. It creates an automaton with limit [k] and [str1], and then feeds
      it the string [str2], and thet outputs the result of calling [end_input]
      on the nfa. *)
  val get_distance : k:int -> string -> string -> int option
end
module Dem : sig
  type nfa_state

  (** [start ~k ~str] produces the starting state of the automaton for edit
      distance [k].  Here [k] must not be negative and must not be greater
      [(Sys.int_size - 1) / 2]. *)
  val start : k:int -> str:string -> nfa_state

  (** [feed nfa ch] produces a new state where the automaton has been fed the
      character [ch]. *)
  val feed : nfa_state -> ch:char -> nfa_state

  (** [current_error nfa] produces [Some n] if the current error count recorded
      by the nfa is a number [n], or [None] if the error count is larger than
      the limit [k] that the nfa was started with.  [current_error nfa] may be
      smaller than [end_input nfa], since it does not account for delete
      operations at the end of the fed string. *)
  val current_error : nfa_state -> int option

  (** [end_input nfa] computes the edit distance between the starting string and
      the string fed to [nfa]. It produces [Some n] if the edit distance in a
      number [n] that is less than the limit [k] that the automaton was started
      with, and [None] otherwise. *)
  val end_input : nfa_state -> int option

  (** [feed_str nfa ~str] produces a new state where the automaton has been fed
      the characters from the string [str]. *)
  val feed_str : nfa_state -> str:string -> nfa_state

  (** [get_distance ~k str1 str2] computes the edit distance between two
      strings. It creates an automaton with limit [k] and [str1], and then feeds
      it the string [str2], and thet outputs the result of calling [end_input]
      on the nfa. *)
  val get_distance : k:int -> string -> string -> int option
end
