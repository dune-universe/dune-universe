(* Parsers *)

type ('context, 'problem, 'value) parser

type 'context located =
  { row : int
  ; col : int
  ; context : 'context
  }

type ('context, 'problem) dead_end =
  { row : int
  ; col : int
  ; problem : 'problem
  ; context_stack : 'context located list
  }

val run :
  ('c, 'x, 'a) parser ->
  string ->
  ('a, ('c, 'x) dead_end list) result

val in_context :
  'context ->
  ('context, 'x, 'a) parser ->
  ('context, 'x, 'a) parser

type 'x token =
  | Token of string * 'x

(* Building blocks *)

val is_alpha : char -> bool
val is_num : char -> bool

val int : 'x -> ('c, 'x, int) parser
val float : 'x -> 'x -> ('c, 'x, float) parser

val symbol : 'x token -> ('c, 'x, unit) parser
val keyword : 'x token -> ('c, 'x, unit) parser

module String_set : sig
  include Set.S with type elt = string
end

val variable :
  start:(char -> bool) ->
  inner:(char -> bool) ->
  reserved:String_set.t ->
  expecting:'x ->
  ('c, 'x, string) parser

val endd : 'x -> ('c, 'x, unit) parser

(* Pipelines *)

val succeed : 'a -> ('c, 'x, 'a) parser

val (|=) :
  ('c, 'x, 'a -> 'b) parser ->
  ('c, 'x, 'a) parser ->
  ('c, 'x, 'b) parser

val (|.) :
  ('c, 'x, 'keep) parser ->
  ('c, 'x, 'ignore) parser ->
  ('c, 'x, 'keep) parser

val lazily : (unit -> ('c, 'x, 'a) parser) -> ('c, 'x, 'a) parser

val and_then :
  ('a -> ('c, 'x, 'b) parser) ->
  ('c, 'x, 'a) parser ->
  ('c, 'x, 'b) parser

val problem : 'x -> ('c, 'x, 'a) parser

(* Branches *)

val one_of : ('c, 'x, 'a) parser list -> ('c, 'x, 'a) parser

val map : ('a -> 'b) -> ('c, 'x, 'a) parser -> ('c, 'x, 'b) parser

val backtrackable : ('c, 'x, 'a) parser -> ('c, 'x, 'a) parser
val commit : 'a -> ('c, 'x, 'a) parser

val token : 'x token -> ('c, 'x, unit) parser

(* Loops *)

type trailing =
  | Forbidden
  | Optional
  | Mandatory

val sequence :
  start:('x token) ->
  separator:('x token) ->
  endd:('x token) ->
  spaces:(('c, 'x, unit) parser) ->
  item:(('c, 'x, 'a) parser) ->
  trailing:trailing ->
    ('c, 'x, 'a list) parser

type ('state, 'a) step =
  | Loop of 'state
  | Done of 'a

val loop :
  'state ->
  ('state -> ('c, 'x, ('state, 'a) step) parser) ->
  ('c, 'x, 'a) parser

(* Whitespace *)

val spaces : ('c, 'x, unit) parser

val line_comment : 'x token -> ('c, 'x, unit) parser

type nestable =
  | NotNestable
  | Nestable

val multi_comment : 'x token -> 'x token -> nestable -> ('c, 'x, unit) parser

(* Chompers *)

val get_chomped_string : ('c, 'x, 'a) parser -> ('c, 'x, string) parser

val chomp_if : (char -> bool) -> 'x -> ('c, 'x, unit) parser
val chomp_while : (char -> bool) -> ('c, 'x, unit) parser
val chomp_until : 'x token -> ('c, 'x, unit) parser
val chomp_until_end_or : string -> ('c, 'x, unit) parser

val map_chomped_string :
  (string -> 'a -> 'b) -> ('c, 'x, 'a) parser -> ('c, 'x, 'b) parser

val with_indent : int -> ('c, 'x, 'a) parser -> ('c, 'x, 'a) parser

(* Indentation *)

val get_indent : ('c, 'x, int) parser
val get_position : ('c, 'x, int * int) parser

(* Positions *)

val get_row : ('c, 'x, int) parser
val get_col : ('c, 'x, int) parser
val get_offset : ('c, 'x, int) parser
val get_source : ('c, 'x, string) parser

(* Syntax *)

module Syntax : sig
  val ( let+ ) :
    ('c, 'x, 'a) parser ->
    ('a -> 'b) ->
    ('c, 'x, 'b) parser
  val ( and+ ) :
    ('c, 'x, 'a) parser ->
    ('c, 'x, 'b) parser ->
    ('c, 'x, 'a * 'b) parser
  val ( and* ) :
    ('c, 'x, 'a) parser ->
    ('c, 'x, 'b) parser ->
    ('c, 'x, 'a * 'b) parser
  val ( let* ) :
    ('c, 'x, 'a) parser ->
    ('a -> ('c, 'x, 'b) parser) ->
    ('c, 'x, 'b) parser
end
