(* File: lbfgs.mli

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE.txt for more details. *)

(** Binding to
    {{:http://users.eecs.northwestern.edu/~nocedal/lbfgsb.html}L-BFGS-B}.
    These is a limited-memory quasi-Newton code for unconstrained and
    for bound-constrained optimization.

    The authors of the original FORTRAN code expect that if you use
    their software in a publication, you quote one of these references:

    - R. H. Byrd, P. Lu and J. Nocedal. A Limited Memory Algorithm for
    Bound Constrained Optimization, (1995), SIAM Journal on
    Scientific and Statistical Computing , 16, 5, pp. 1190-1208.
    - C. Zhu, R. H. Byrd and J. Nocedal. L-BFGS-B: Algorithm 778:
    L-BFGS-B, FORTRAN routines for large scale bound constrained
    optimization (1997), ACM Transactions on Mathematical Software,
    Vol 23, Num. 4, pp. 550-560.
    - J.L. Morales and J. Nocedal. L-BFGS-B: Remark on Algorithm 778:
    L-BFGS-B, FORTRAN routines for large scale bound constrained
    optimization (2011), to appear in ACM Transactions on
    Mathematical Software.
*)

open Bigarray

type work
(** Represent the memory space needed to solve a minimization problem.
    It is usually allocated automatically but it is possible to
    do it manually to, say, allocate it once only before a loop. *)

exception Abnormal of float * string
(** [Abnormal(f, msg)] is raised if the routine terminated abnormally
    without being able to satisfy the termination conditions.  In such
    an event, the variable [x] (see {!F.min}) will contain the current
    best approximation found and [f] is the value of the target
    function at [x].  [msg] is a message containing additional
    information (returned by the original FORTRAN code).

    If the error message is not precise enough, it is recommended to
    turn printing on to understand what is the problem. *)

(** Control of the frequency at which information is outputted. *)
type print =
| No    (** No output is generated. *)
| Last  (** Print one line at the last iteration. *)
| Every of int (** [Every k] prints the value of the function and |proj
                   gradient| every [k] iterations.  Valid values are
                   [0 <= k <= 98], otherwise the closer value in that
                   interval is used. *)
| Details (** Print details of every iteration (except vectors). *)
| All   (** Print details of every iteration (except vectors)
            including changes of active set and final x.  *)
| Full  (** Print details of every iteration including x and g. *)

type state
(** Holds informations on the current state of the computation that
    can help to decide whether to stop. *)

(** Fortran Layout. *)
module F :
sig
  type vec = (float, float64_elt, fortran_layout) Array1.t
  (** Vectors. *)

  val min : ?print:print -> ?work:work -> ?nsteps: int -> ?stop:(state -> bool) ->
    ?corrections:int -> ?factr:float -> ?pgtol:float ->
    ?n:int ->
    ?ofsl:int -> ?l:vec -> ?ofsu:int -> ?u:vec ->
    (vec -> vec -> float) -> ?ofsx:int -> vec -> float
  (** [min f_df x] compute the minimum of the function [f] given by
    [f_df].  [x] is an initial estimate of the solution vector.  On
    termination, [x] will contain the best approximation found.  [f_df
    x df] is a function that computes f(x) and its gradiant f'(x),
    returns f(x) and stores f'(x) in [df].  The [x] passed to [f_df x
    df] is physically equal to the [x] given in [min f_df x].  Can
    raise {!Abnormal}.

    @param l lower bound for each component of the vector [x].  Set
    [l.(i)] to [neg_infinity] to indicate that no lower bound is desired.
    Default: no lower bounds.

    @param u upper bound for each component of the vector [x].  Set
    [u.(i)] to [infinity] to indicate that no upper bound is desired.
    Default: no upper bounds.

    @param n the dimension of the space of unknowns.
    Default: [dim x - ofsx + 1].
    @param ofsl offset for the matrix [l].  Default: [1].
    @param ofsu offset for the matrix [u].  Default: [1].
    @param ofsx offset for the matrix [x].  Default: [1].

    @param factr tolerance in the termination test for the algorithm.
    The iteration will stop when
    [(f^k - f^{k+1})/max{ |f^k|, |f^{k+1}|, 1} <= factr*epsilon_float].
    Set e.g. [factr] to [1e12] for low accuracy, [1e7] for moderate
    accuracy and [1e1] for extremely high accuracy.  Setting [factr] to
    [0.] suppresses this termination test.  Default: [1e7].

    @param pgtol The iteration will stop when
    [max{ |proj g_i| : i = 1,..., n} <= pgtol]
    where [proj g_i] is the ith component of the projected gradient.
    Setting [pgtol] to [0.] suppresses this termination test.
    Default: [1e-5].

    @param corrections maximum number of variable metric corrections
    used to define the limited memory matrix.  Values < 3 are not
    recommended, and large values of [corrections] can result in
    excessive computing time.  The range 3 <= [corrections] <= 20 is
    recommended.  Default: [10].  This value in called [M] in L-BFGS-B
    debugging output.

    @param nsteps maximum number of steps.  Default: no limitation.

    @param stop a function that tells whether we stop the computation
    after at the current approximation.

    @param print Tells the amount of debugging information desired.
    Default: [No]. *)


  val max : ?print:print -> ?work:work -> ?nsteps: int -> ?stop:(state -> bool) ->
    ?corrections:int -> ?factr:float -> ?pgtol:float ->
    ?n:int ->
    ?ofsl:int -> ?l:vec -> ?ofsu:int -> ?u:vec ->
    (vec -> vec -> float) -> ?ofsx:int -> vec -> float
  (** [max f_df x] computes the maximum of the function [f] given by
      [f_df].  [x] is an initial estimate of the solution vector.  This
      function is provided for convenience and calls {!F.min} to which
      the reader is referred for further explanations. *)
end

(** C layout. *)
module C :
sig
  type vec = (float, float64_elt, c_layout) Array1.t
  (** Vectors indexed by [0 .. n-1]. *)

  val min : ?print:print -> ?work:work -> ?nsteps: int -> ?stop:(state -> bool) ->
    ?corrections:int -> ?factr:float -> ?pgtol:float ->
    ?n:int ->
    ?ofsl:int -> ?l:vec -> ?ofsu:int -> ?u:vec ->
    (vec -> vec -> float) -> ?ofsx:int -> vec -> float
  (** See {!F.min}.  Note that the default value for [ofsl], [ofsu]
      and [ofsx] is [0] and the one for [n] is [dim x - ofsx]. *)

  val max : ?print:print -> ?work:work -> ?nsteps: int -> ?stop:(state -> bool) ->
    ?corrections:int -> ?factr:float -> ?pgtol:float ->
    ?n:int ->
    ?ofsl:int -> ?l:vec -> ?ofsu:int -> ?u:vec ->
    (vec -> vec -> float) -> ?ofsx:int -> vec -> float
  (** See {!F.max}. Note that the default value for [ofsl], [ofsu]
      and [ofsx] is [0] and the one for [n] is [dim x - ofsx].  *)
end

val work : ?corrections:int -> int -> work
(** [work n] allocate the work space for a problem of size at most [n].

    @param corrections See {!F.min}. *)


(************************************************************************)
(** {2 Accessing the state} *)

val is_constrained : state -> bool
(** Tells whether the problem is constrained. *)

val nintervals : state -> int
(** The total number of intervals explored in the search of Cauchy
    points. *)

val nskipped_updates : state -> int
(** The total number of skipped BFGS updates before the current
    iteration. *)

val iter : state -> int
(** The number of current iteration. *)

val nupdates : state -> int
(** The total number of BFGS updates prior the current iteration.  *)

val nintervals_current : state -> int
(** The number of intervals explored in the search of Cauchy point in
    the current iteration. *)

val neval : state -> int
(** The total number of function and gradient evaluations. *)

val neval_current : state -> int
(** The number of function value or gradient evaluations in the
    current iteration. *)

val previous_f : state -> float
(** Returns f(x) in the previous iteration. *)

val norm_dir : state -> float
(** 2-norm of the line search direction vector. *)

val eps : state -> float
(** The machine precision epsmch generated by the code. *)

val time_cauchy : state -> float
(** The accumulated time spent on searching for Cauchy points. *)

val time_subspace_min : state -> float
(** The accumulated time spent on subspace minimization. *)

val time_line_search : state -> float
(** The accumulated time spent on line search. *)

val slope : state -> float
(** The slope of the line search function at the current point of line
    search. *)

val slope_init : state -> float
(** The slope of the line search function at the starting point of the
    line search. *)

val normi_grad : state -> float
(** The infinity norm of the projected gradient *)
;;

