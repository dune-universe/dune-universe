module Info:
sig
  type t =
    | Position of int * int
    | Unknown
end

module Application_type:
sig
  type t =
    | First
    | First_implicit
    | Target
    | Operator
    | Implicit
    | Any
end

module Precedence:
sig
  type t

  val lowest :t
  val comma: t
  val argument_list:t
  val quantifier: t
  val arrow: t
  val disjunction: t
  val conjunction: t
  val negation: t
  val relop: t
  val addition: t
  val multiplication:t
  val exponentiation:t
  val application: t
  val dot: t
  val highest:t

  val lower_needs_parens: t -> t -> bool
  val left_needs_parens:  t -> t -> bool
  val right_needs_parens: t -> t -> bool
end


module Operator:
sig
  type t
  val of_string: string -> t option
  val string: t -> string
  val precedence: t -> Precedence.t
  val arrow: t
  val fat_arrow: t
  val eq: t
  val andop: t
  val orop: t
  val plus: t
  val caret: t
end



module Feature_name:
sig
  type t =
    | Name of string
    | Operator of Operator.t
    | Bracket
    | True
    | False
    | Number of int
  module Map: Map.S
end

val some_feature_name: string -> Feature_name.t option
val some_feature_bracket: Feature_name.t option
val some_feature_operator: Operator.t -> Feature_name.t option
val some_feature_number: int -> Feature_name.t option
val some_feature_true:  Feature_name.t option
val some_feature_false: Feature_name.t option
val some_feature_name_opt: string option -> Feature_name.t option
