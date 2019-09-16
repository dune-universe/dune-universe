(* File: integration1d.mli

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Routines for one dimensional numerical integration. *)

(** Choice of local integration rule: which Gauss-Kronrod pair is used. *)
type integrator =
  | GAUSS15 (** 7 - 15 points *)
  | GAUSS21 (** 10 - 21 points *)
  | GAUSS31 (** 15 - 31 points *)
  | GAUSS41 (** 20 - 41 points *)
  | GAUSS51 (** 25 - 51 points *)
  | GAUSS61 (** 30 - 61 points *)

type reliability =
  | OK (** normal and reliable termination of the routine. it is
           assumed that the requested accuracy has been achieved.  *)
  | Limit (** maximum number of subdivisions allowed has been
              achieved. one can allow more subdivisions
              by increasing the value of [limit].

              However, if this yields no improvement it is rather
              advised to analyze the integrand in order to determine
              the integration difficulties. If the position of a local
              difficulty can be determined (e.g. singularity,
              discontinuity within the interval) one will probably
              gain from splitting up the interval at this point and
              calling the integrator on the subranges. If possible, an
              appropriate special-purpose integrator should be used
              which is designed for handling the type of difficulty
              involved.  *)
  | Roundoff (** The occurrence of roundoff error is detected, which
                 prevents the requested tolerance from being achieved.  *)
  | Bad_integrand (** Extremely bad integrand behaviour occurs at some
                      points of the integration interval.  *)

type result = {
  res: float; (** Approximation to the integral. *)
  err: float; (** Estimate of the modulus of the absolute error, which
                  should equal or exceed abs(res - result).  *)
  neval: int; (** Number of integrand evaluations. *)
  nsub: int; (** Number of sub-intervals used [<= limit]. *)
  msg: reliability;
}

exception Function_not_finite of float
(** [Function_not_finite(x)] is raised when the function one
    seeks to integrate returns NaN, [infinity] or [neg_infinity] at [x].  *)

type workspace
(** Temporary memory used by the integration routines. *)

val workspace : integrator -> int -> workspace
  (** [workspace i limit] creates a workspace for the integrator [i]
      that can handle up to [limit] sub-intervals.  *)

val qag : ?limit:int -> ?workspace:workspace -> integrator ->
  ?epsabs:float -> ?epsrel:float ->
  (float -> float) -> float -> float -> result
(** [qag integrator] returns a function [integ] so that [integ f a b]
    is an approximation, [i], to the integral [f] over the interval
    [(a,b)] hopefully satisfying following claim for accuracy [abs(i -
    true_result) <= max epsabs (epsrel*abs(i))].
    ⚠ BEWARE that [qag integrator] creates a workspace that will be
    used for all calls of [integ], so you should not call [integ]
    inside [f] or call the same [integ] in parallel.

    Keywords: automatic integrator, general-purpose, integrand
    examinator, globally adaptive, Gauss-Kronrod.

    @param epsabs absolute accuracy requested (default [1.49E-8]).
    @param epsrel relative accuracy requested (default [1.49E-8]).
    @param limit gives an upper-bound on the number of sub-intervals
    in the partition of (a,b); must satisfy [limit >= 1]. Default: [50].

    @param workspace Temporary memory used by the integration
    routines.  It is recommended to allocate this memory outside
    loops.  Default: new temporary memory is allocated as needed.  A
    nice way of allocating temprary ressources once only is to use
    partial evaluation: [let integ = qag integrator] and then use it
    repeatedly as [integ f a b].

    @raise Failure if [epsabs <= 0 && epsrel <= max (50 * eps_float) 0.5E-28]
    @raise Function_not_finite if the function [f] returns NaN or an
                               infinite value at an evaluation point.
 *)



(**/**)
(** Old routine *)
val simp_adapt : ?fh:out_channel -> ?tol:float ->
  ?hmin:float -> (float -> float) -> float -> float -> float * float
