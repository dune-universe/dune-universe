(** The UCB1 module is parameterised by a finite set of actions
    presented as an array of abstract "arms", each arm corresponding
    to an action. *)
module type Arm_sig =
sig
  type t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** Phantom types used to tag the state of the bandit. *)
type awaiting_reward
type ready_to_move

module Make : functor (Arm : Arm_sig) ->
sig
  (** The state of a bandit. *)
  type 'state t

  (** Create a fresh bandit with given arms. *)
  val create : Arm.t array -> ready_to_move t

  (** Select the UCB1-optimal action to play. The bandit expects a reward. *)
  val next_action : ready_to_move t -> Arm.t * awaiting_reward t

  (** Assign a reward to the bandit. The reward must be in the [0,1] interval. *)
  val set_reward : awaiting_reward t -> float -> ready_to_move t

  (** Total rewards obtained by the bandit. *)
  val total_rewards : ready_to_move t -> float

  (** Pretty-print useful statistics on the bandit, for debugging purposes.*)
  val pp_stats : Format.formatter -> 'state t -> unit
end
