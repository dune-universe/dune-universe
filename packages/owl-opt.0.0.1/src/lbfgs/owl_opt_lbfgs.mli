open Owl

module Make (P : Owl_opt.Prms.PT) : sig
  (** objective function value type *)
  type fv = Algodiff.D.t

  (** paramter type *)
  type prm = Algodiff.D.t

  (** user-defined paramter record type *)
  type prms = prm P.t

  (** objective function type *)
  type f = prms -> fv

  (** internal state *)
  type state

  (** stopping criterion function type *)
  type stop = state -> bool

  (** [iter s] returns the number of iterations for optimisation state [s] *)
  val iter : state -> int

  (** [prms s] returns the optimisation parameters of state [s] *)
  val prms : state -> prms

  (** [fv s] returns the objective function value of state [s] *)
  val fv : state -> float

  (** [f s] returns the objective function of state [s] *)
  val f : state -> f

  (** [init ~prms0 ~f ()] returns an initialises optimisation state for initial parmaters [prms0] and objective function [f] *)
  val init : prms0:prms -> f:f -> unit -> state

  val min
    :  ?stop:stop
    -> ?pgtol:float
    -> ?factr:float
    -> ?corrections:int
    -> state
    -> state

  val max
    :  ?stop:stop
    -> ?pgtol:float
    -> ?factr:float
    -> ?corrections:int
    -> state
    -> state
end
