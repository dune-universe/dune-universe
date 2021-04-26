open Ucb1

type 'a gen = Random.State.t -> 'a

module type S = sig
  (** Type of states. *)
  type terminal

  type nonterminal

  (** States must be distinguished as either terminal or nonterminal. *)
  type state = Terminal of terminal | Nonterminal of nonterminal

  (** Actions that can be taken at each state. *)
  type action

  (** Actions available at a given state. *)
  val actions : nonterminal -> action array

  (** Given a state and an action, one can move to the next state. *)
  val next : nonterminal -> action -> state

  (** Reward at a terminal state. *)
  val reward : terminal -> float

  (** The MCTS is parameterised by a Monte-Carlo exploration. Setting
      to [`Uniform] will use an uniform search. *)
  val exploration_depth : [ `Unbounded | `Bounded of int ]

  val exploration_kernel : [ `Uniform | `Kernel of nonterminal -> state gen ]

  val pp_action : Format.formatter -> action -> unit

  val pp_terminal : Format.formatter -> terminal -> unit

  val pp_nonterminal : Format.formatter -> nonterminal -> unit
end

(** Monte-Carlo Tree Search yields a policy, i.e. a way to decide which
    action to take at each nonterminal state. *)
module type Policy = sig
  type t

  type action

  val policy : playouts:int -> t -> action gen
end

module MCTS : functor (X : S) ->
  Policy with type t = X.nonterminal and type action = X.action =
functor
  (X : S)
  ->
  struct
    type t = X.nonterminal

    type action = X.action

    module Bandit = Ucb1.Make (struct
      type t = int

      let compare (x : int) (y : int) =
        if x < y then -1 else if x > y then 1 else 0

      let pp = Format.pp_print_int
    end)

    type tree =
      | Terminal of terminal_node
      | Nonterminal of nonterminal_node
      | Unexplored of X.nonterminal

    and bandit = ready_to_move Bandit.t

    and nonterminal_node =
      { state : X.nonterminal;
        mutable bandit : bandit;
        actions : action array;
        branches : tree lazy_t array
      }

    and terminal_node = { final : X.terminal; reward : float }

    let uniform_exploration : t -> X.state gen =
     fun state ->
      let actions = X.actions state in
      fun rng_state ->
        let act =
          let index = Random.State.int rng_state (Array.length actions) in
          actions.(index)
        in
        X.next state act

    let exploration =
      match X.exploration_kernel with
      | `Uniform -> uniform_exploration
      | `Kernel f -> f

    let rec explore_until_termination (node : X.nonterminal) rng_state =
      let next = exploration node rng_state in
      match next with
      | X.Terminal state -> X.reward state
      | X.Nonterminal state -> explore_until_termination state rng_state

    let rec explore_until_termination_bounded (gas : int) (node : X.nonterminal)
        rng_state =
      if gas < 0 then 0.0
      else
        let next = exploration node rng_state in
        match next with
        | X.Terminal state -> X.reward state
        | X.Nonterminal state ->
            explore_until_termination_bounded (gas - 1) state rng_state

    let exploration_loop =
      match X.exploration_depth with
      | `Unbounded -> explore_until_termination
      | `Bounded gas -> explore_until_termination_bounded gas

    let rec assign_reward path reward =
      match path with
      | [] -> ()
      | (node, bandit) :: tl ->
          let bandit = Bandit.set_reward bandit reward in
          node.bandit <- bandit ;
          assign_reward tl reward

    let rec playout (node : nonterminal_node) path rng_state =
      let (act, awaiting) = Bandit.next_action node.bandit in
      let path = (node, awaiting) :: path in
      match Lazy.force node.branches.(act) with
      | Terminal { reward; _ } -> assign_reward path reward
      | Nonterminal node' -> playout node' path rng_state
      | Unexplored nonterminal ->
          let new_node = expand_node nonterminal in
          node.branches.(act) <- Lazy.from_val (Nonterminal new_node) ;
          let reward = exploration_loop nonterminal rng_state in
          assign_reward path reward

    and expand_node nonterminal =
      let actions = X.actions nonterminal in
      let arms = Array.init (Array.length actions) (fun i -> i) in
      let bandit = Bandit.create arms in
      let branches =
        Array.map
          (fun act ->
            Lazy.from_fun (fun () ->
                match X.next nonterminal act with
                | X.Terminal final ->
                    Terminal { final; reward = X.reward final }
                | X.Nonterminal state -> Unexplored state))
          actions
      in
      { state = nonterminal; bandit; actions; branches }

    let policy ~playouts initial_state rng_state =
      let root = expand_node initial_state in
      for _i = 0 to playouts - 1 do
        playout root [] rng_state
      done ;
      let (act, _) = Bandit.next_action root.bandit in
      root.actions.(act)
  end
