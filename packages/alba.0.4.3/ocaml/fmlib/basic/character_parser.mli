open Common
open Module_types


(** Position within a stream.*)
module Position:
sig
  type t
  val line:   t -> int
  val column: t -> int
  val next_line:   t -> t
  val next_column: t -> t
end



module Located:
sig
  type 'a t
  val make: Position.t -> 'a -> Position.t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val use: 'a t -> (Position.t -> 'a -> Position.t -> 'b) -> 'b
  val start: 'a t -> Position.t
  val end_:  'a t -> Position.t
  val range: 'a t -> Position.t * Position.t
  val value: 'a t -> 'a
end



module type CONTEXT =
  sig
    type t
    type msg
    val message: t -> msg
    val position: t -> Position.t
    val line: t -> int
    val column: t -> int
  end



module Indent:
sig
    type t
    (** Allowed indentations *)


    val is_offside: int -> t -> bool
    (**
        [is_offside col ind] Is the column [col] offside for the allowed
        indentation [ind]?
    *)

    val lower_bound: t -> int
    (** The lower bound of the indentation set. *)

    val has_only_one_position: t -> bool
    (** Does the indentation set have exactly one position? *)


    val string_of_set: t -> string
    (**
        [string_of_set ind]
    *)
end



module type PARSER =
  sig
    type state
    (** State type *)


    (** Parser Type *)
    type parser


    (** Does the parser need more tokens (i.e. either [put_character] or
    [put_end])?  *)
    val needs_more: parser -> bool

    (** Has the parser terminated (opposite of [needs_more p])? *)
    val has_ended:  parser -> bool

    (** Has the parser succeeded *)
    val has_succeeded:  parser -> bool

    (** Has the parser failed *)
    val has_failed:  parser -> bool

    (** The current position. *)
    val position:   parser -> Position.t

    (** The current line. *)
    val line:   parser -> int

    (** The current column. *)
    val column: parser -> int

    val state: parser -> state
    (** The state of the parser. *)

    val error_tabs: parser -> int list

    (** [put_character p c] feeds the parser [p] with the character token [c].
    Only possible if [needs_more p] is valid. *)
    val put_character: parser -> char -> parser

    (** [put_end p] signals to the parser [p] the end of stream. Only possible
     if          [needs_more p] is valid.*)
    val put_end: parser -> parser
  end








module type COMBINATORS =
  sig
    (** {2 Basic Combinators} *)

    type expect

    include Generic_parser.COMBINATORS

    val unexpected: expect -> 'a t
    val backtrackable:   'a t -> expect -> 'a t
    val followed_by:     'a t -> expect -> unit t
    val not_followed_by: 'a t -> expect -> unit t
    val (<?>):           'a t -> expect -> 'a t

    (** {2 Position and State Combinators} *)

    val get_position: (Position.t) t
    val located: 'a t -> 'a Located.t t

    type state
    val get_state: state t
    val update: (state -> state) -> unit t


    (** {2 Indentation Combinators} *)

    val absolute: 'a t -> 'a t
    val absolute_at: int -> 'a t -> 'a t
    val indented: 'a t -> 'a t
    val maybe_indented: 'a t -> 'a t
    val detached: 'a t -> 'a t
    val get_bounds: (int * int option) t

    val one_or_more_aligned:  'a t -> 'a list t
    val zero_or_more_aligned: 'a t -> 'a list t
    val skip_one_or_more_aligned:  'a t -> int t
    val skip_zero_or_more_aligned: 'a t -> int t


    (** {2 Context Combinator} *)

    type context
    val in_context: context -> 'a t -> 'a t

  end







(** Simple Parser. *)
module Simple (Final: ANY):
sig
    (** {1 Modules and Types} *)

    module Context: CONTEXT with type msg = string

    module Error: Generic_parser.ERROR with type expect   = string * Indent.t
                                        and type semantic = string

    type final = Final.t
    type token = char option


    (** {1 Combinators} *)

    include COMBINATORS
        with type expect   = string
        and  type semantic = string
        and  type state    = Unit.t
        and  type context  = string


    (** {2 Character Combinators} *)
    val expect: (char -> bool) -> string -> char t
    val expect_end: unit t
    val one_of_chars: string -> string -> char t
    val string: string -> unit t
    val char: char -> unit t
    val space: unit t
    val letter: char t
    val digit: char t

    val word: (char->bool) -> (char->bool) -> string -> string t
    (** [word start_char inner_char error_message]

        A word starts with a character satisfying [start_char] followed by zero
        or more characters satisfying [inner_char].

        In case that the first character does not satisfy [start_char] an
        unsatisfied expectation with [error_message] is entered.
    *)


    (** {1 Parser} *)

    (** {2 During Parsing} *)

    include PARSER with type state = Unit.t


    (** {2 Terminated Parser} *)

    (** The result the parser has produced which is either a final value or a
       list of dead ends. Only valid if the parser has terminated. *)
    val result: parser -> final option

    val result_string: parser -> (final -> string) -> string

    val error: parser -> Error.t

    (** The list of tokens (i.e. optional characters) which the parser has not
       processed at the point of termination. *)
    val lookahead: parser -> token list

    val lookahead_string: parser -> string


    (** {2 Create and Run the Parser} *)

    (** [make pc] makes a parser from a parser combinator [pc].*)
    val make: final t -> parser

    (** [run p str] makes a parser from the combinator [pc] and runs the parser
       on the string [str]. *)
    val run: final t -> string -> parser
end








(** Normal Parser.

- [State]: User state.
- [Final]: Final result type of the parser.
- [Semantic]: Semantic error message (triggered by [fail message])
- [Context_msg]: Each new context is opened with a value of type [Context_msg].

*)
module Normal (State: ANY) (Final: ANY) (Semantic: ANY) (Context_msg: ANY):
sig
    (** {1 Modules and Types} *)

    module Context: CONTEXT with type msg = Context_msg.t

    module Error: Generic_parser.ERROR with type expect   = string * Indent.t
                                        and type semantic = Semantic.t

    type final = Final.t
    type token = char option

    (** {1 Combinators} *)

    include COMBINATORS
        with type expect   = string
        and  type semantic = Semantic.t
        and  type state    = State.t
        and  type context  = Context_msg.t


    (** {2 Character Combinators} *)
    val expect: (char -> bool) -> string -> char t
    val expect_end: unit t
    val one_of_chars: string -> string -> char t
    val string: string -> unit t
    val char: char -> unit t
    val space: unit t
    val letter: char t
    val digit: char t

    val word: (char->bool) -> (char->bool) -> string -> string t
    (** [word start_char inner_char error_message]

        A word starts with a character satisfying [start_char] followed by zero
        or more characters satisfying [inner_char].

        In case that the first character does not satisfy [start_char] an
        unsatisfied expectation with [error_message] is entered.
    *)


    (** {1 Parser} *)

    (** {2 During Parsing} *)

    include PARSER with type state = State.t


    (** {2 Terminated Parser} *)

    (** The result the parser has produced which is either a final value or a
       list of dead ends. Only valid if the parser has terminated. *)
    val result: parser -> final option

    val result_string: parser -> (final -> string) -> string

    val error: parser -> Error.t

    (** The list of tokens (i.e. optional characters) which the parser has not
       processed at the point of termination. *)
    val lookahead: parser -> token list

    val lookahead_string: parser -> string


    (** {2 Create and Run the Parser} *)

    (** [make pc st] makes a parser from a parser combinator [pc] with the
    initial state [st].*)
    val make: final t -> State.t -> parser

    (** [run pc st str] makes a parser from the combinator [pc], the initial
    state [st] and runs the parser on the string [str]. *)
    val run: final t -> State.t -> string -> parser
end







(** Advanced Parser.

- [State]: User state.
- [Final]: Final result type of the parser.
- [Expect_msg]: Error message, if something syntactically expected is not there.
- [Semantic]: Semantic error message (triggered by [fail message])
- [Context_msg]: Each new context is opened with a value of type [Context_msg].


*)
module Advanced
         (** User state *)
         (State:       ANY)
         (Final:       ANY)
         (Expect_msg:  ANY)
         (Semantic:    ANY)
         (Context_msg: ANY):
sig
  (** {1 Modules and Types} *)

    module Context:  CONTEXT with type msg = Context_msg.t

    module Error:
        Generic_parser.ERROR
            with type expect   = Expect_msg.t * Indent.t
            and  type semantic = Semantic.t

    type final = Final.t
    type token = char option


    (** {1 Combinators} *)

    include COMBINATORS
        with type expect   = Expect_msg.t
        and  type semantic = Semantic.t
        and  type state    = State.t
        and  type context  = Context_msg.t


    (** {2 Character Combinators} *)

    val expect:          (char -> bool) -> Expect_msg.t -> char t
    val expect_end:      Expect_msg.t -> unit t
    val one_of_chars:    string -> Expect_msg.t -> char t
    val string:          string -> (int -> Expect_msg.t) -> unit t
    val char:            char -> Expect_msg.t -> unit t
    val space:           Expect_msg.t -> unit t
    val letter:          Expect_msg.t -> char t
    val digit:           Expect_msg.t -> char t
    val word:            (char->bool) -> (char->bool) -> Expect_msg.t -> string t

    (** {1 Parser} *)

    (** {2 During Parsing} *)

    include PARSER with type state = State.t


    (** {2 Terminated Parser} *)

    (** The result the parser has produced which is either a final value or a
       list of dead ends. Only valid if the parser has terminated. *)
    val result: parser -> final option


    val error: parser -> Error.t


    (** The list of tokens (i.e. optional characters) which the parser has not
       processed at the point of termination. *)
    val lookahead: parser -> token list



    (** {2 Create and Run the Parser} *)

    (** [make pc st] makes a parser from a parser combinator [pc] and the
       initial state [st]. *)
    val make: final t -> State.t -> parser

    (** [run pc st str] makes a parser from the combinator [pc] and the
       initial state [st] and runs the parser on the string [str]. *)
    val run: final t -> State.t -> string -> parser
end
