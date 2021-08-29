(** Empirical distributions. *)

(** {1:empirical Empirical distributions.} *)

(** ['a t] is the type of empirical distributions over ['a]. *)
type 'a t = 'a Stats_intf.emp

(** {2 Transformers} *)

(** [map f dist] maps the function [f] on [dist]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [of_generative ~nsamples gen] transforms the generative distribution [gen]
    into a random empirical distribution. Each empirical distribution is obtained
    by taking [nsamples] samples from [gen]. *)
val of_generative : nsamples:int -> 'a Stats_intf.gen -> 'a t Stats_intf.gen

(** [of_raw_data array] creates an empirical distribution matching the contents of
    [array]. *)
val of_raw_data : 'a array -> 'a t

(** [to_raw_data dist] returns an array containing the empirical data. *)
val to_raw_data : 'a t -> [> `Empirical of 'a array ]

(** {2 Statistics} *)

(** [truncate ord dist p] discards the last 1-p percentiles of
    [dist]. The underlying data is totally ordered by [ord].

    @raise Invalid_argument if [p < 0 || p > 1] *)
val truncate :
  (module Basic_intf.Ordered with type t = 'elt) -> 'elt t -> float -> 'elt t

(** [quantile ord dist p] computes the [p]th percentile of [dist].
    The underlying data is totally ordered by [ord].

    @raise Invalid_argument if [p < 0 || p > 1] *)
val quantile :
  (module Basic_intf.Ordered with type t = 'elt) -> 'elt t -> float -> 'elt

(** [remove_outliers ~nsigmas dist] discards elements which are above
    [nsigmas] (empirical) standard deviations above or below the (empirical) mean.

    @raise Invalid_argument if [nsigmas < 0] *)
val remove_outliers : nsigmas:float -> float t -> float t

(** The module type of empirical statistics. *)
module type Empirical_statistics = sig
  (** [r] is the type representing real numbers. *)
  type r

  (** [empirical_mean dist] computes the empirical mean (or {e sample mean}) of [dist]. *)
  val empirical_mean : r t -> r

  (** [empirical_mean_generic mod dist] computes the empirical mean (or {e sample mean}) of [dist].
      This functions works on empirical distributions over arbitrary r-modules. *)
  val empirical_mean_generic :
    (module Basic_intf.Module with type t = 'elt and type R.t = r) ->
    'elt t ->
    'elt

  (** [empirical_variance dist] computes the (uncorrected) empirical variance associated to [dist]. *)
  val empirical_variance : r t -> r
end

(** Empirical statistics over floats. *)
module Float : Empirical_statistics with type r = float

(** Empirical statistics over rationals. *)
module Rational : Empirical_statistics with type r = Q.t
