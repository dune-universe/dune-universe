(* File: odepack.mli

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Binding to ODEPACK.  This is a collection of solvers for the
    initial value problem for ordinary differential equation systems.
    See {{:http://computation.llnl.gov/casc/odepack/}the ODEPACK page}
    and {{:http://www.netlib.org/odepack/}Netlib}.

    You can jump to the interface of the {!lib}.

    {2 Example of use}

    To solve the equation ∂ₜ²u = f(t,u) with initial conditions
    u(t₀) = u₀ and ∂ₜu(t₀) = u'₀, you must first reduce it to a first
    order ODE: ∂ₜ(y₁,y₂) = (y₂, f(t,y₁)) with the initial condition
    y(t₀) = (u₀, u'₀).  Then write an OCaml function to evaluate the
    right hand side of this ODE:
    {[
      let ode t (y: vec) (dy: vec) =
        dy.{1} <- y.{2};
        dy.{2} <- f t y.{1}                                 ]}
    and get an approximate value of the vector y(t) with
    {[
      let init = Array1.of_array float64 fortran_layout [|u₀; u'₀|] in
      Odepack.vec(Odepack.lsoda ode init t₀ t)
    ]}
    You must explicitly project the return value of [Odepack.lsoda]
    with [Odepack.vec] to get the state of the system because there
    are several other operations that you can perform on this value
    (see above).  The value of u(t) is the given by the first
    component of [y], so you can define (an approximation of) u with
    {[
      let u ~u0 ~u'0 t =
        let init = Array1.of_array float64 fortran_layout [|u0; u'0|] in
        Odepack.vec(Odepack.lsoda ode init t₀ t).{1}
    ]}

   @version 0.6.9
   @author Christophe Troestler (Christophe.Troestler@umons.ac.be)
 *)

open Bigarray

(** {2:lib Odepack library} *)

type vec = (float, float64_elt, fortran_layout) Array1.t
(** Representation of vectors. *)

type mat = (float, float64_elt, fortran_layout) Array2.t
(** Representation of matrices. *)

type t
(** A mutable value holding the current state of solving the ODE. *)

(** Types of Jacobian matrices. *)
type jacobian =
  | Auto_full (** Internally generated (difference quotient) full Jacobian *)
  | Auto_band of int * int
  (** Internally generated (difference quotient) band Jacobian.  It
      takes [(l,u)] where [l] (resp. [u]) is the number of lines below
      (resp. above) the diagonal (excluded). *)
  | Full of (float -> vec -> mat -> unit)
  (** [Full df] means that a function [df] is provided to compute the
      full Jacobian matrix (∂fᵢ/∂yⱼ) of the vector field f(t,y).
      [df t y jac] must store ∂fᵢ/∂yⱼ([t],[y]) into [jac.{i,j}]. *)
  | Band of int * int * (float -> vec -> int -> mat -> unit)
  (** [Band(l, u, df)] means that a function [df] is provided to compute
      the banded Jacobian matrix with [l] (resp. [u]) diagonals below
      (resp. above) the main one (not counted).  [df t y d jac] must
      store ∂fᵢ/∂yⱼ([t],[y]) into [jac.{i-j+d, j}].  [d] is the row of
      [jac] corresponding to the main diagonal of the Jacobian matrix.  *)

val lsoda : ?rtol:float -> ?rtol_vec:vec -> ?atol:float -> ?atol_vec:vec ->
  ?jac:jacobian -> ?mxstep:int -> ?copy_y0:bool ->
  ?debug:bool -> ?debug_switches:bool ->
  (float -> vec -> vec -> unit) -> vec -> float -> float -> t
(** [lsoda f y0 t0 t] solves the ODE dy/dt = F(t,y) with initial
    condition y([t0]) = [y0].  The execution of [f t y y'] must
    compute the value of the F([t], [y]) and store it in [y'].
    It uses a dense or banded Jacobian when the problem is stiff, but it
    automatically selects between nonstiff (Adams) and stiff (BDF)
    methods.  It uses the nonstiff method initially, and dynamically
    monitors data in order to decide which method to use.

    @param rtol  relative error tolerance parameter.  Default [1e-6].
    @param rtol_vec  relative error tolerance vector.
    @param atol  absolute error tolerance parameter.  Default [1e-6].
    @param atol_vec  absolute error tolerance vector.

    If [rtol_vec] (resp. [atol_vec]) is specified, it is used in place
    of [rtol] (resp. [atol]).  Specifying only [rtol] (resp. [atol])
    is equivalent to pass a constant [rtol_vec] (resp. [atol_vec]).
    The solver will control the vector E = (E(i)) of estimated local
    errors in [y], according to an inequality of the form
    max-norm(E(i)/EWT(i)) <= 1, where EWT(i) =
    [rtol_vec.{i} * abs_float(y.{i}) + atol_vec.{i}].

    @param jac is an optional Jabobian matrix.  If the problem is
    expected to be stiff much of the time, you are encouraged to supply
    [jac], for the sake of efficiency.  Default: [Auto_full].

    @param mxstep maximum number of (internally defined) steps allowed
    during one call to the solver.  The default value is 500.

    @param copy_y0 if [false], the vector [y0] is MODIFIED to contain
    the value of the solution at time [t].  Otherwise [y0] is
    unchanged (the current solution vector is then obtained by
    {!Odepack.vec}).  Default: [true].

    @param debug allows [lsoda] to print messages.  Default [true].
    The messages contain valuable information, it is not recommended
    to turn them off.

    @param debug_switches prints a message to stdout on each
    (automatic) method switch (between nonstiff and stiff).
    Default: [false].
*)

val lsodar : ?rtol:float -> ?rtol_vec:vec -> ?atol:float -> ?atol_vec:vec ->
  ?jac:jacobian -> ?mxstep:int -> ?copy_y0:bool ->
  ?debug:bool -> ?debug_switches:bool ->
  g:(float -> vec -> vec -> unit) -> ng:int ->
  (float -> vec -> vec -> unit) -> vec -> float -> float -> t
(** [lsodar f y0 t0 t ~g ~ng] is like {!lsoda} but has root searching
    capabilities.  The algorithm will stop before reaching time [t] if
    a root of one of the [ng] constraints is found.  You can determine
    whether the [lsodar] stopped at a root using {!has_root}.  It only
    finds those roots for which some component of [g], as a function
    of t, changes sign in the interval of integration.  The function
    [g] is evaluated like [f], that is: [g t y gout] must write to
    [gout.{1},..., gout.{ng}] the values of the [ng] constraints.  *)


val vec : t -> vec
(** [vec ode] returns the current value of the solution vector.  *)

val time : t -> float
(** [t ode] returns the current time at which the solution vector was
    computed. *)

val advance : ?time: float -> t -> unit
(** [advance ode ~time:t] modifies [ode] so that an approximation of
    the value of the solution at times [t] is computed.  Note that, if
    the solver has root searching capabilities and a time is provided,
    the solver may stop before that time if a root is found.  The time
    is recorded for future calls to [advance ode].  If the solver has
    no root finding capabilities and no time is provided, this
    function does nothing. *)

val has_root : t -> bool
(** [has_root ode] says wheter the solver stopped (i.e. the current
    state of [ode] is) because a root was found.  If the solver has no
    root searching capabilities, this returns [false]. *)

val root : t -> int -> bool
(** [root t i] returns true iff the [i]th constraint in [lsodar] has a
    root.  It raises [Invalid_argument] if [i] is not between 1 and
    [ng], the number of constraints (included).  This only makes sense
    if [has_root t] holds.  *)

val roots : t -> bool array
(** [roots t] returns an array [r] such that [r.(i)] holds if and only
    if the [i]th constraint has a root. *)

val sol : t -> float -> vec
(** [sol ode t] modifies [ode] so that it holds an approximation of
    the solution at [t] and returns this approximation.  Any root that
    might be found is ignored. *)
;;
