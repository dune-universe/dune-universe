(** Time-stamp: <modified the 10/07/2017 (at 15:57) by Erwan Jahier> *)

(** An action is an intermediary data type that is used to translate expressions
    into [Soc.gao]. It is basically a clocked Soc.atomic_operation with arguments.

    The idea is that each expression is translated into one or several actions.
    And those clocks are then translated into guards, so that each action is
    translated into a gao.

    A more natural Module to define that type in would have been Soc, but that
    module could  be shared with other front-ends (e.g., heptagon or lucid),
    and I prefer that module not to depend on 
    - such a cutting (expr -> action -> gao)
    - The [Eff.clock] name (could have been a module parameter though).
  *)

type rhs = Soc.var_expr list
type lhs = Soc.var_expr list
type t = Lic.clock * rhs * lhs * Soc.atomic_operation * Lxm.t

val to_string: t -> string

(* This one is meant to be used for error msgs *)
val to_string_msg: t -> string
