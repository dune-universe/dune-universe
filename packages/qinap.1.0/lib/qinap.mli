(** A (very small) parsing library. *)

(** Module containing functions for working with lists of characters. *)
module Clist : sig

  (** Type alias for a list of characters. *)
  type t = char list

  (** [to_string cs] returns a string containing the characters in [cs]. *)
  val to_string : t -> string

  (** [of_string s] returns a list containing the characters in [s]. *)
  val of_string : string -> t

end

(** Module containing functions for working with strings. *)
module Ext_string : sig

  (** [hd s] returns the first character of the string [s].
      Wraps its result in an option type, to avoid raising exceptions. *)
  val hd : string -> char option

  (** [tl s] returns [s] with its first character removed.
      If applied to the empty string, [tl] result the empty string. *)
  val tl : string -> string

end

(** ['a parser] parses values of type ['a].
    A parser consumes a string and either returns a tuple with the result
    of the parsing and the remaining unparsed input or fails. *)
type 'a parser = string -> ('a * string) option

(** {1 Primitive parsers} *)

(** [result x] returns a parser that consumes no input and always succeeds
    with the value [x]. It is an implementation of [return] for the
    parser monad. *)
val result : 'a -> 'a parser

(** [zero] is a parser that always fails without consuming any of its input. *)
val zero : 'a parser

(** [item] parses exactly one character from its input.
    Fails if applied to an empty string. *)
val item : char parser

(** {1 Primitive combinators} *)

(** [p => f] returns a parser that applies the function [f] to the result
    of [p] and fails if [p] fails.
    This combinator is an implementation of [map] for the parser functor. *)
val ( => ) : 'a parser -> ('a -> 'b) -> 'b parser

(** [let* p f] returns a parser that chains the parser [p] and the parser
    that results from applying [f] to the result of [p].
    This combinator is an implementation of [bind] for the parser monad.
    [let*] can be used similarly to Haskell's do-notation to chain parsers
    without using many anonymous functions. *)
val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser

(** [p <|> q] parses its input using [p]; if that fails,
    it parses the original input using [q].
    Can be read as a logical "or".
    [<|>] is the choice combinator. *)
val ( <|> ) : 'a parser -> 'a parser -> 'a parser

(** {1 Derived combinators} *)

(** [p >> q] parses its input using [p], throws away the result of [p],
    and parses the remaining input using [q]. *)
val ( >> ) : 'a parser -> 'a parser -> 'a parser

(** [p << q] parses its input using [p], passes the remaining input using [q],
    and throws away the result of [q], leaving only the result of [p]. *)
val ( << ) : 'a parser -> 'a parser -> 'a parser

(** [p <~> ps] parses its input using [p], parses the remaining input using [ps],
    and preppends the result of [p] to the list obtained by applying [ps].
    You can think of [p <~> ps] as the parser version of [x :: xs]. *)
val ( <~> ) : 'a parser -> 'a list parser -> 'a list parser

(** [many p] parses its input using [p] zero or more times
    and returns the results in a list. *)
val many : 'a parser -> 'a list parser

(** [many1 p] parses its input using [p] one or more times
    and returns the results in a list. *)
val many1 : 'a parser -> 'a list parser

(** {1 Derived parsers} *)

(** [satisfy ~f] parsers a character [x] from its input if [f x] is true
    and fails otherwise. *)
val satisfy : f:(char -> bool) -> char parser

(** [char c] parses a character from its input if it is equal to [c]
    and fails otherwise. *)
val char : char -> char parser

(** [clist cs] takes a list of characters [cs] as its argument
    and succeeds if it can parse all of them from its input in order. *)
val clist : Clist.t -> Clist.t parser

(** [string s] takes a string [s] as its argument
    and succeeds if it can parse that string from its input. *)
val string : string -> string parser

(** [one_of cs] tries to parse a character from the list [cs], in order,
    and succeeds returning the parsed character if a match is found,
    failing otherwise. *)
val one_of : char list -> char parser

(** [between ~l ~r p] parses [p] between [l] and [r],
    whose results are discarded. *)
val between : 'a parser -> l:('b parser) -> r:('c parser) -> 'a parser

(** {1 Parsers and combinators for dealing with whitespace} *)

(** [sep_by1 p ~sep] returns a list of the results of the parser [p],
    ignoring the results given by the separator parser [sep], which
    is applied one or more times between each use of [p]. *)
val sep_by1 : 'a parser -> sep:('b parser) -> 'a list parser

(** [sep_by p ~sep] returns a list of the results of the parser [p],
    ignoring the results given by the separator parser [sep], which
    is applied zero or more times between each use of [p]. *)
val sep_by : 'a parser -> sep:('b parser) -> 'a list parser

(** [space] is a parser that succeeds if it finds a whitespace character,
    returning it, and fails otherwise. *)
val space : char parser

(** [spaces] is a parser that suceeds if it finds one or more whitespace
    characters, returning a list of the parsed characters,
    and fails otherwise. *)
val spaces : char list parser

(** {1 Parsers and combinators for dealing with numbers} *)

(** [digit] parses exactly one digit from its input
    and fails if that is not possible. *)
val digit : char parser

(** [digits] parses zero or more digits from its input and returns them
    a string, and fails if that is not possible. *)
val digits : string parser

(** [digits1] parses one or more digits from its input and returns them
    a string, and fails if that is not possible. *)
val digits1 : string parser

(** [natural] parses a natural number from its input as an int
    and fails if that is not possible. *)
val natural : int parser

(** [integer] parses an integer from its input as an int
    and fails if that is not possible. *)
val integer : int parser

(** [float] parses a real number, written as a decimal expansion with
    digits before and after the point, returns it as a float,
    and fails if that is not possible. *)
val float : float parser