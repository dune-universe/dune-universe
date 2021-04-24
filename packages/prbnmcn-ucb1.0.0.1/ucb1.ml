type awaiting_reward

type ready_to_move

module type Arm_sig = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

type arm_statistics =
  { number_of_activations : float;
    cumulative_reward : float;
    empirical_reward : float
  }

let pp_stats fmtr { number_of_activations; cumulative_reward; empirical_reward }
    =
  Format.fprintf
    fmtr
    "@[<v 2>{ act. count: %f@,cumulative: %f@,empirical:  %f }@]"
    number_of_activations
    cumulative_reward
    empirical_reward

let compute_expected_reward (round : float) (arm : arm_statistics) =
  if arm.number_of_activations = 0.0 then infinity
  else
    arm.empirical_reward +. sqrt (2.0 *. log round /. arm.number_of_activations)

module Make (Arm : Arm_sig) = struct
  module M = Map.Make (Arm)

  type 'state t =
    { arms : Arm.t array;
      stats : arm_statistics M.t;
      round : int;
      last : Arm.t
    }

  let initial_statistics =
    { number_of_activations = 0.0;
      cumulative_reward = 0.0;
      empirical_reward = 0.0
    }

  let create (arms : Arm.t array) : ready_to_move t =
    if Array.length arms = 0 then invalid_arg "create: length 0 array" ;
    let stats =
      Array.fold_left (fun m arm -> M.add arm initial_statistics m) M.empty arms
    in
    { arms; stats; round = 1; last = arms.(0) }
  (* The initial value of [last] does not matter.*)

  let next_action : ready_to_move t -> Arm.t * awaiting_reward t =
   fun state ->
    let round = float state.round in
    let (selected_arm, _reward) =
      M.fold
        (fun arm stats ((_selected_arm, best_reward) as acc) ->
          let r = compute_expected_reward round stats in
          if best_reward < r then (arm, r) else acc)
        state.stats
        (state.arms.(0), 0.0)
    in
    let state = { state with last = selected_arm } in
    (selected_arm, (state :> awaiting_reward t))

  let set_reward : awaiting_reward t -> float -> ready_to_move t =
   fun state reward ->
    let last_stats = M.find state.last state.stats in
    let cum_rew = last_stats.cumulative_reward +. reward in
    let emp_rew = cum_rew /. last_stats.number_of_activations in
    let num_act = last_stats.number_of_activations +. 1. in
    let updated_stats =
      { cumulative_reward = cum_rew;
        empirical_reward = emp_rew;
        number_of_activations = num_act
      }
    in
    let state =
      { state with
        round = state.round + 1;
        stats = M.add state.last updated_stats state.stats
      }
    in
    (state :> ready_to_move t)

  let total_rewards state =
    M.fold
      (fun _arm stats acc -> acc +. stats.cumulative_reward)
      state.stats
      0.0

  let find_best_arm (stats : arm_statistics M.t) (f : arm_statistics -> float) =
    let (res_opt, value) =
      M.fold
        (fun arm stats ((_winner, value) as acc) ->
          let v = f stats in
          if v > value then (Some arm, v) else acc)
        stats
        (None, 0.0)
    in
    match res_opt with None -> assert false | Some res -> (res, value)

  let pp_stats fmtr { stats; round; _ } =
    let (most_played, _) =
      find_best_arm stats (fun { number_of_activations; _ } ->
          number_of_activations)
    in
    let (most_cumulative_reward, _) =
      find_best_arm stats (fun { cumulative_reward; _ } -> cumulative_reward)
    in
    let (most_empirical_reward, _) =
      find_best_arm stats (fun { empirical_reward; _ } -> empirical_reward)
    in
    Format.fprintf
      fmtr
      "most played: %a\n%a\n"
      Arm.pp
      most_played
      pp_stats
      (M.find most_played stats) ;
    Format.fprintf
      fmtr
      "best cumulative reward: %a\n%a\n"
      Arm.pp
      most_cumulative_reward
      pp_stats
      (M.find most_cumulative_reward stats) ;
    Format.fprintf
      fmtr
      "best empirical reward: %a\n%a\n"
      Arm.pp
      most_empirical_reward
      pp_stats
      (M.find most_empirical_reward stats) ;
    let total_cumulative_rewards =
      M.fold (fun _arm stats acc -> acc +. stats.cumulative_reward) stats 0.0
    in
    Format.fprintf
      fmtr
      "Total cumulative reward in %d rounds: %f\n"
      (round - 1)
      total_cumulative_rewards
end
