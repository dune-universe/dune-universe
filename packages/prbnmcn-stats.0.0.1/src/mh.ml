module type MH_parameters = sig
  include Basic_intf.Pp

  val proposal : t -> t Gen.t

  val proposal_log_density : t -> t -> Log_space.t

  val log_weight : t -> Log_space.t
end

module Make_core_sampling_loop (X : Basic_intf.Pp) = struct
  let silent () = ()

  let mcmc ~verbosity ~(initial : X.t) ~(burn_in : int) sample_step :
      X.t Stats_intf.gen Stats_intf.gen =
    let (burn_in_progress, sample_progress, trace) =
      match verbosity with
      | `Silent -> (silent, silent, false)
      | `Progress ->
          let burn_in = Tools.make_progress_printer burn_in "burn-in" in
          let sample = Tools.make_progress_printer 0 "sampling" in
          (burn_in, sample, false)
      | `Trace -> (silent, silent, true)
    in
    let rec sample_loop (index : int) (bound : int) (current_state : X.t)
        rng_state =
      if index > bound then current_state
      else (
        if trace then
          Format.eprintf "burn-in %d/%d: %a\n%!" index bound X.pp current_state ;
        burn_in_progress () ;
        sample_loop
          (index + 1)
          bound
          (sample_step current_state rng_state)
          rng_state)
    in
    fun rng_state ->
      let after_burn_in = sample_loop 1 burn_in initial rng_state in
      let state_ref = ref after_burn_in in
      fun rng_state ->
        let _ = sample_progress () in
        let new_state = sample_step !state_ref rng_state in
        state_ref := new_state ;
        new_state
end

module Make (X : MH_parameters) = struct
  let sample_step (current_state : X.t) : X.t Gen.t =
    let open Gen.Infix in
    let* proposal_state = X.proposal current_state in
    let p_forward = X.proposal_log_density current_state proposal_state in
    let p_backward = X.proposal_log_density proposal_state current_state in
    if Log_space.equal p_backward Log_space.zero then return proposal_state
    else
      let forward_flow = Log_space.mul (X.log_weight current_state) p_forward in
      let backward_flow =
        Log_space.mul (X.log_weight proposal_state) p_backward
      in
      let ratio = Log_space.div backward_flow forward_flow in
      let acceptance = Log_space.(to_float (min one ratio)) in
      let* flip = Gen.bernouilli acceptance in
      if flip then return proposal_state else return current_state

  include Make_core_sampling_loop (X)

  let mcmc ~verbosity ~(initial : X.t) ~(burn_in : int) :
      X.t Stats_intf.gen Stats_intf.gen =
    mcmc ~verbosity ~initial ~burn_in sample_step
end
