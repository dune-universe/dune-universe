open Module_types


module Error  (Exp:ANY) (Sem:ANY):
sig
  type t =
    | Syntax of Exp.t list
    | Semantic of Sem.t
end



module type ERROR =
  sig
    type t
    type semantic
    type expect
    val is_semantic: t -> bool
    val semantic: t -> semantic
    val expectations: t -> expect list
    val make_semantic: semantic -> t
    val make_expectations: expect list -> t
  end




module type COMBINATORS =
  sig
    type 'a t
    type expect
    type semantic
    val return: 'a -> 'a t
    val succeed: 'a -> 'a t
    val unexpected: expect -> 'a t
    val fail:    semantic -> 'a t
    val consumer: 'a t -> 'a t
    val map:     ('a -> 'b) -> 'a t -> 'b t
    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    val (<|>):   'a t -> 'a t -> 'a t
    val (<?>):   'a t -> expect -> 'a t
    val backtrackable: 'a t -> expect -> 'a t

    val not_followed_by: 'a t -> expect -> unit t
    val optional: 'a t -> 'a option t
    val one_of:   'a t list -> 'a t
    val zero_or_more: 'a t -> 'a list t
    val one_or_more:  'a t -> 'a list t
    val one_or_more_separated:  'a t -> _ t -> 'a list t
    val zero_or_more_separated: 'a t -> _ t -> 'a list t
    val skip_zero_or_more: 'a t -> int t
    val skip_one_or_more:  'a t -> int t

    val (|=): ('a -> 'b) t -> 'a t -> 'b t
    val (|.): 'a t -> _ t -> 'a t
  end



module Make (T:ANY) (S:ANY) (Expect:ANY) (Semantic:ANY) (F:ANY):
sig
  type token = T.t
  type final = F.t

  type parser

  include COMBINATORS
          with type expect = Expect.t
           and type semantic = Semantic.t

  module Error: ERROR with type semantic = Semantic.t
                       and type expect   = Expect.t

  val needs_more: parser -> bool
  val has_ended:  parser -> bool
  val put_token:  parser -> token -> parser
  val state:      parser -> S.t
  val result:     parser -> final option
  val error:      parser -> Error.t

  val error_string:
    parser
    -> (Expect.t -> string)
    -> (Semantic.t -> string)
    -> string

  val lookahead:  parser -> token list
  val has_succeeded: parser -> bool
  val has_failed: parser -> bool

  val make_parser: S.t -> final t -> parser

  val get: S.t t
  val update: (S.t -> S.t) -> unit t
  val get_and_update: (S.t -> S.t) -> S.t t

  val token: (S.t -> token -> ('a * S.t, Expect.t) result) -> 'a t
end
