(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

(** Dual numbers.
    This module provides arithmetic operations on dual numbers represented as
    double-precision floating-point numbers (type [float]).  *)


type t = { re : float; dre : float; }
(** The type of dual numbers.  [re] is the value and [dre] the
    derivative. *)

val dual : ?y:float -> float -> t
(** Create a dual number.  By default [dre] is set to 1. *)

val print_dual : t -> unit
(** Pretty print dual number. *)

val equal : t -> t -> bool
(** Check equality between two dual numbers. *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val neg : t -> t
(** Unary negation. *)

val mul : t -> t -> t
(** Multiplication. *)

val div : t -> t -> t
(** Division *)

val pow : t -> float -> t
(** Power function.  [pow d1 d2] returns [d1] to the [d2] power. *)

val sqrt : t -> t
(** Square root. *)

val exp : t -> t
(** Exponentiation.  [exp d] returns [e] to the [d] power. *)

val log : t -> t
(** Natural logarithm (in base [e]). *)

val sin : t -> t
(** Sine function. *)

val cos : t -> t
(** Cosine function. *)

val tan : t -> t
(** Tangent function. *)

val asin : t -> t
(** Inverse sine function. *)

val acos : t -> t
(** Inverse cosine function. *)

val atan : t -> t
(** Inverse tangent function. *)

val sinh : t -> t
(** Hyperbolic sine function. *)

val cosh : t -> t
(** Hyperbolic cosine function. *)

val tanh : t -> t
(** Hyperbolic tangent function. *)

val root : (t -> t) -> t -> t
(** [root f x0] returns the root of [f] with starting value [x0] via
   Newton's method. *)
