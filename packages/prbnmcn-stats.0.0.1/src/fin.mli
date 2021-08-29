(** Finite measures implemented as finitely supported functions. *)

(** {1:finite Finitely supported distributions.} *)

(** {2:finite_generic Generic constructors.} *)

(** [measure reals vecs data] returns the finite measure associated to
    [data]. [data] is a list of weighted elements, with weights admitting
    the structure specifiedby [reals]. An implementation of free modules
    over [reals] must provided as [vecs].

    @raise Invalid_argument if a weight is negative. *)
val measure :
  (module Basic_intf.Reals with type t = 'r) ->
  (module Basic_intf.Free_module_std with type Basis.t = 't and type R.t = 'r) ->
  ('t * 'r) list ->
  ('t, 'r) Stats_intf.fin_mes

(** [probability reals vecs data] packs [data] as a [fin_prb].

    @raise Invalid_argument if the sum of the weights is not equal to one
    or if a weight is negative. *)
val probability :
  (module Basic_intf.Reals with type t = 'r) ->
  (module Basic_intf.Free_module_std with type Basis.t = 't and type R.t = 'r) ->
  ('t * 'r) list ->
  ('t, 'r) Stats_intf.fin_prb

(** Forgetful map from the type of finite probabilities to the type of measures. *)
val as_measure : ('a, 'b) Stats_intf.fin_prb -> ('a, 'b) Stats_intf.fin_mes

(** {2:finite_field_specific Functions specific to the underlying field.} *)

(** [float]-valued finitely supported distributions.  *)
module Float : sig
  include Stats_intf.Fin_dist with type r = float

  (** Distances and divergences between measures. *)
  module Dist : sig
    (** Kullbacak-Leibler divergence. Note that this will diverge if the two measures
        do not have the same support. *)
    val kl : 'a mes -> 'a mes -> float

    (** Lp distance.

        @raise Invalid_argument if [p < 1] *)
    val lp : p:r -> 'a mes -> 'a mes -> r

    (** L-infinity distance *)
    val linf : 'a mes -> 'a mes -> r
  end
end

(** [Q]-valued finitely supported distributions.  *)
module Rational : sig
  include Stats_intf.Fin_dist with type r = Q.t

  module Dist : sig
    (** L-infinity distance *)
    val linf : 'a mes -> 'a mes -> r
  end
end
