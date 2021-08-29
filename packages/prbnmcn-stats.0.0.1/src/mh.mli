(** Metropolis-Hastings MCMC *)

(** {1:Metropolis-Hastings MCMC} *)

(** [MH_parameters] packs sufficient data to construct a
    Metropolis-Hastings sampler. *)
module type MH_parameters = sig
  include Basic_intf.Pp

  (** Proposal kernel. *)
  val proposal : t -> t Gen.t

  (** Logarithm of the density of the proposal kernel (wrt some unknown
      underlying measure). *)
  val proposal_log_density : t -> t -> Log_space.t

  (** Logarithm of the density of the (unnormalized) target measure
      (wrt the same underlying measure as the proposal). *)
  val log_weight : t -> Log_space.t
end

(** Generic sampling loop. *)
module Make_core_sampling_loop (X : Basic_intf.Pp) : sig
  (** The [sampler] function produces a generative probability.
      More precisely: each sample from [mcmc ~verbosity ~initial ~burn_in]
      is a distinct Markov chain, which samples can in-turn be obtained by
      calling iteratively the closure returned by [mcmc].
      Concretely, the outer `Stats.gen` corresponds to sampling the burn-in
      while the inner corresponds to sampling from the actual chain.
   *)
  val mcmc :
    verbosity:[ `Silent | `Progress | `Trace ] ->
    initial:X.t ->
    burn_in:int ->
    (X.t -> X.t Gen.t) ->
    X.t Gen.t Gen.t
end

(** Metropolis-Hastings functor. *)
module Make (X : MH_parameters) : sig
  (** [mcmc] produces a sampler using [Make_core_sampling_loop] with the
      added guarantee that, provided the [MH_parameters] are consistent,
      the empirical measure will sample from the target measure specified
      by [X.log_weight].

      Be warned that the samples obtained from the chain are _not_
      independent.The guarantee is that (if the proposal and weight
      are consistent) the empirical measure will converge in law
      to the target measure. *)
  val mcmc :
    verbosity:[ `Silent | `Progress | `Trace ] ->
    initial:X.t ->
    burn_in:int ->
    X.t Gen.t Gen.t
end
