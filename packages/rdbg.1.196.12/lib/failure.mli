(* Time-stamp: <modified the 27/04/2018 (at 11:05) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
**
** File: failure.ml
** Author: Erwan Jahier
*)

type info =
  | Boolean of Expr.t (* *)
  | Numeric of Expr.t (* *)
