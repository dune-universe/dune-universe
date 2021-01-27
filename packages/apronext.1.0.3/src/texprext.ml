(** This file is an extension for the Texpr1 module from the Apron
   library *)

(** Note : It only adds function, nothing is removed. Extensions are at
the end of the module *)

open Apron
include Texpr1

(***********************)
(* Useful constructors *)
(***********************)

let cst_f f = Texpr1.Cst (Coeff.s_of_float f)
let cst_i i = Texpr1.Cst (Coeff.s_of_int i)
let var_s s = Texpr1.Var (Var.of_string s)

let unary ?typ:(t=Real) ?round:(r=Near) op e =
  unop op e t r

let neg ?typ ?round =
  unary ?typ ?round Neg

let cast ?typ ?round =
  unary ?typ ?round Cast

let sqrt ?typ ?round =
  unary ?typ ?round Sqrt

let binary ?typ:(t=Real) ?round:(r=Near) op e1 e2 =
  binop op e1 e2 t r

let sub ?typ ?round =
  binary ?typ ?round Sub

let add ?typ ?round =
  binary ?typ ?round Add

let mul ?typ ?round =
  binary ?typ ?round Mul

let div ?typ ?round =
  binary ?typ ?round Div

let pow ?typ ?round =
  binary ?typ ?round Pow

(* mod is a keyword :( *)
let modulo ?typ ?round =
  binary ?typ ?round Mod
