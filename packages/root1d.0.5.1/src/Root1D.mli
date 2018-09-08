(* File: Root1D.mli

   Copyright (C) 2007-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/anum/software/

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
   CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
   NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  *)

(** 1D Root finding algorithms.

    @version 0.5.1 *)

val brent : ?tol:float -> (float -> float) -> float -> float -> float
(** [brent f a b] returns an approximation [x] of a root of [f] in
    the interval [[a,b]] with absolute accuracy [6. *. epsilon_float
    *. abs_float(x) +. tol].

    @raise Invalid_argument if [f(a) *. f(b) > 0.].

    @param tol desired length of the interval of uncertainty of the final
    result (must be [>= 0]).  Default: [sqrt epsilon_float].

    Ref.: Brent, R. (1973) Algorithms for Minimization without
    Derivatives. Englewood Cliffs, NJ: Prentice-Hall.  *)

val bisection : ?eps: float -> (float -> float) -> float -> float -> float
(** [bisection f a b] find an approximation of a root in the
    interval [[a,b]] using the bisection algorithm.

    @raise Invalid_argument if [f(a) *. f(b) > 0.] or [eps <= 0.]

    @param eps is the desired relative error on the solution.  More
    precisely, it terminates when the interval \[a,b\] verifies
    |a-b| ≤ eps max(|a|, |b|).  Default: [sqrt epsilon_float]. *)

val illinois : ?eps: float -> (float -> float) -> float -> float -> float
(** [illinois f a b] find an approximation of a root in the interval
    [[a,b]] using the Illinois algorithm (which is the Regula Falsi
    method with a small twist).  Order of convergence: ³√3 ≈ 1.442.

    @raise Invalid_argument if [f(a) *. f(b) > 0.] or [eps <= 0.]

    @param eps is the desired relative error on the solution.  More
    precisely, it terminates when the interval \[a,b\] verifies
    |a-b| ≤ eps max(|a|, |b|).  Default: [sqrt epsilon_float].

    Ref.: {{:http://link.springer.com/article/10.1007/BF01934364}
    M. Dowell, P. Jarrat, A modified Regula Falsi method for
    computing the root of anequation, BIT 11 (1971), 168–174}. *)

val newton : ?good_enough:(float -> float -> float -> bool) ->
  (float -> float * float) -> float -> float
(** [newton f_f' x0] returns an approximate root of [f] close to the
    initial guess [x0] using Newton's method.  [f_f'] is a function
    such that [f_f' x] returns the couple [(f x, f' x)] where [f' x]
    is the derivative of [f] at [x].

    @raise Failure if the derivative vanishes during the
    computations.

    @param good_enough takes as arguments the current approximation
    [x], the previous approximation [xprev], and [f(x)] and returns
    whether [x] is a good enough approximation. Default:
    [abs_float(f x) < sqrt epsilon_float].*)

val brent2 : ?tol:float -> (float -> float * int) -> float -> float -> float
(** [brent2 f a b] finds a zero of the function [f] in the same way
    [brent f a b] does except that [f x] returns the couple [(y, z)]
    for the number [y * 2**z].  Thus underflow and overflow can be
    avoided for a function with large range.

    Ref.: Brent, R. (1973) Algorithms for Minimization without
    Derivatives. Englewood Cliffs, NJ: Prentice-Hall. *)

(*
val secant : ?good_enough:(float -> float -> bool) ->
             (float -> float) -> float -> float -> float

val false_position : ?good_enough:(float -> float -> float -> float -> bool) ->
                     (float -> float) -> float -> float -> float

val muller : (float -> float) -> float -> float -> float
  (** http://en.wikipedia.org/wiki/M%C3%BCller%27s_method *)

 *)
;;
