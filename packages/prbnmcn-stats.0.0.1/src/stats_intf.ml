(** Type signatures. *)

(* ------------------------------------------------------------------------- *)

(** The type of (inclusive) ranges. *)
type range = { min : float; max : float }

(* ------------------------------------------------------------------------- *)

(** Primitive {e representations} of distributions: empirical, generative or
    finitely supported. *)

(** ['a emp] is the type of empirical measures *)
type 'a emp = 'a array

(** ['a gen] is the type of samplers *)
type 'a gen = Random.State.t -> 'a

(** [Fin_fun] is the module type describing the implementation of finite functions. *)
module type Fin_fun = sig
  (** [t] is the type of the domain of the finite function. *)
  type t

  (** [r] is the type of the range of the finite function. Typically, [r] will
      admit the structure of a ring. *)
  type r

  (** [V] is the core of the implementation of the finite function, presented as a
      free [R]-module. *)
  module V : Basic_intf.Free_module_std with type R.t = r and type Basis.t = t

  (** [total_weight] is the sum of the values of all points in the domain of the finite function. *)
  val total_weight : r

  (** [weightmap] is the value holding the finite function. *)
  val weightmap : V.t
end

(** [Fin_kernel] is the module type packing the implementation of finite kernels. *)
module type Fin_kernel = sig
  (** [t] is the type of the domain of the finite kernel. *)
  type t

  (** [t] is the type of the range of the finite kernel. *)
  type u

  (** [r] is the type of the measure. We expect [r] to admit the structure
      of a ring. *)
  type r

  (** [V] is the core of the implementation of the finite functions, presented as a
      free [R]-module. *)
  module V : Basic_intf.Free_module_std with type R.t = r and type Basis.t = u

  (** A [kernel] associates to each point in the domain a finitely supported
      function (typically interpreted as a finite measure). *)
  val kernel : t -> V.t
end

(** [('a, 'r) fin_fun] is the type of finite functions from ['a] to ['r]. *)
type ('a, 'r) fin_fun = (module Fin_fun with type t = 'a and type r = 'r)

(** [fin_mes] is the type of finitely supported measures. *)
type ('a, 'r) fin_mes = M of ('a, 'r) fin_fun [@@unboxed]

(** [fin_mes] is the type of finitely supported probability measures. *)
type ('a, 'r) fin_prb = P of ('a, 'r) fin_fun [@@unboxed]

(** [('a, 'b,'r)] is the type of finite kernels from ['a] to ['b] with
    measure valued in ['r]. *)
type ('a, 'b, 'r) kernel =
  (module Fin_kernel with type t = 'a and type u = 'b and type r = 'r)

(* ------------------------------------------------------------------------- *)

(** Type classes. *)

(** [Sampling_monad] is the module type of a sampling-based monad *)
module type Sampling_monad = Basic_intf.Monad with type 'a t = 'a gen

(** [Gen] allows to manipulate generative probabilities (ie samplers). *)
module type Gen = sig
  type parameters = { nsamples : int; state : Random.State.t }

  include
    Sampling_monad
      with type 'a res =
            parameters ->
            (module Basic_intf.Std with type t = 'a) ->
            ('a, float) fin_mes

  (** [float bound] samples uniformly in [0; bound] *)
  val float : float -> float t

  (** [int bound] samples uniformly in [0; bound] *)
  val int : int -> int t

  (** [bool] samples a boolean uniformly *)
  val bool : bool t

  (** [uniform elts] samples an element of the [elts] array by sampling an index uniformly.
      @raise Invalid_argument if [elts] has length 0 *)
  val uniform : 'a array -> 'a t

  (** [bernouilli alpha] samples [true] with probability [alpha].

      @raise Invalid_argument if [alpha] is not in the [0;1] interval. *)
  val bernouilli : float -> bool t

  (** [geometric p] samples a nonnegative integer according to the geometric law of parameter [p]. *)
  val geometric : float -> int t

  (** [subsample ~n gen] samples one out of [n] samples from [gen]. *)
  val subsample : n:int -> 'a t -> 'a t

  (** [of_empirical emp] samples from [emp] matching exactly the empirical frequencies. *)
  val of_empirical : 'a emp -> 'a t

  (** Exponential distribution via inverse CDF. *)
  val exponential : rate:float -> float t

  (** Gaussian distribution via Box-Muller transform.
    Returns a pair of _independent_ gaussian variates with
    prescribed mean and standard deviation. *)
  val box_muller : mean:float -> std:float -> (float * float) t

  (** Gaussian distribution (wrapper over box-muller transform). *)
  val gaussian : mean:float -> std:float -> float t

  (** Samples uniformly in the given [range].

      @raise Invalid_argument if [range] is empty. *)
  val range : range -> float t

  (** Categorical distribution. Total mass need not be one. Does not aggregate mass of equal elements.
      @raise Invalid_argument if some weights are negative or if the total mass is zero. *)
  val categorical : ('a * float) list -> 'a t

  (** Constructs a sampler from a finite measure through [categorical].
      NB: this is mostly useful for sampling repeatedly from the distribution. *)
  val of_fin_mes : ('a, float) fin_mes -> 'a t

  module Rational : sig
    (** Categorical distribution. Total mass need not be one. Does not aggregate mass of equal elements.
      @raise Invalid_argument if some weights are negative or if the total mass is zero. *)
    val categorical : ('a * Q.t) list -> 'a t
  end
end

module type Fin_dist = sig
  type r

  (** The type of finite functions with domain ['a] and range [r]. *)
  type 'a finfn = ('a, r) fin_fun

  (** The type of finite probability measures with domain ['a] and range [r]. *)
  type 'a prb = ('a, r) fin_prb

  (** The type of finite measures with domain ['a] and range [r]. *)
  type 'a mes = ('a, r) fin_mes

  (** Constructing measures and probabilities *)

  (** Creates a finitely supported {e measure} from weighted points.
      A measure is not necessarily normalized.
      The underlying set needs to be totally ordered.

      @raise Invalid_argument if a weight is negative. *)
  val measure :
    (module Basic_intf.Free_module_std with type Basis.t = 't and type R.t = r) ->
    ('t * r) list ->
    ('t, r) fin_mes

  (** Creates a finitely supported _probability_ from weighted points.
      A probability is normalized.
      The underlying set needs to be totally ordered.

      @raise Invalid_argument if the sum of the weights is not equal to one
      or if a weight is negative. *)
  val probability :
    (module Basic_intf.Free_module_std with type Basis.t = 't and type R.t = r) ->
    ('t * r) list ->
    ('t, r) fin_prb

  (** Creates a _kernel_. Needs not be normalized. *)
  val kernel :
    ?h:(module Basic_intf.Std with type t = 'a) ->
    (module Basic_intf.Free_module_std with type Basis.t = 'b and type R.t = r) ->
    ('a -> ('b * r) list) ->
    ('a, 'b, r) kernel

  (** Kernel composition. *)
  val compose :
    ?h:(module Basic_intf.Std with type t = 'a) ->
    ('a, 'b, r) kernel ->
    ('b, 'c, r) kernel ->
    ('a, 'c, r) kernel

  (** Computes the pushforward of a finitely supported measure along a kernel. *)
  val pushforward : ('t, r) fin_mes -> ('t, 'u, r) kernel -> ('u, r) fin_mes

  (** Bayesian inverse of a kernel. Defined up to a null set wrt the
      pushforward of the prior. Assumes that the kernel is
      {e probability}-valued. *)
  val inverse :
    ?h:(module Basic_intf.Std with type t = 'u) ->
    ('t, r) fin_prb ->
    ('t, 'u, r) kernel ->
    ('u, r) fin_prb * ('u, 't, r) kernel

  (** Constant kernel. *)
  val constant_kernel : ('b, r) fin_prb -> ('a, 'b, r) kernel

  (** Normalize a measure to obtain a probability measure.

      @raise Invalid_argument if the measure has zero mass. *)
  val normalize : ('t, r) fin_mes -> ('t, r) fin_prb

  (** Computes the empirical measure of an array of elements. Each element
      present in the array is mapped to its count. *)
  val counts_of_empirical :
    (module Basic_intf.Free_module_std with type Basis.t = 't and type R.t = r) ->
    't array ->
    ('t, r) fin_mes

  (** Finitely supported uniform distribution. *)
  val uniform :
    (module Basic_intf.Free_module_std with type Basis.t = 't and type R.t = r) ->
    't array ->
    ('t, r) fin_prb

  (** Biased coin. Raises an error if [bias] is not in [0,1]. *)
  val coin : bias:r -> (bool, r) fin_prb

  (** Binomial distribution.
      [binomial p n] returns the probability of having
      [k] successes over [n] experiments, according to
      a biased coin [p]. *)
  val binomial : (bool, r) fin_prb -> int -> (int, r) fin_prb

  (** Using measures and probabilities *)

  (** Integrates a function against a finitely supported measure. *)
  val integrate : ('t, r) fin_mes -> ('t -> r) -> r

  (** Evaluates a finitely supported probability on argument. Returns 0 if
      the argument is out of the support. *)
  val eval_prb : ('t, r) fin_prb -> 't -> r

  (** Evaluates a finitely supported measure on argument. Returns 0 if
      the argument is out of the support. *)
  val eval_mes : ('t, r) fin_mes -> 't -> r

  (** Evaluate a kernel at a point, yielding a finitely supported measure. *)
  val eval_kernel : 'a -> ('a, 'b, r) kernel -> ('b * r) list

  (** Samples from a finitely supported distribution presented as an unnormalized
      measure. This is mostly useful when sampling only once or twice from a
      distribution: consider converting to a categorical sampler when sampling
      repeatedly. Complexity: O(n) with [n] the cardinality of the support. *)
  val sample : ('a, r) fin_mes -> 'a gen

  (** Returns the total mass associated to a finitely supported measure. *)
  val total_mass : ('t, r) fin_mes -> r

  (** Compute the mean of a finite measure supported on an [Intf.Module].*)
  val mean_generic :
    (module Basic_intf.Module with type t = 'elt and type R.t = r) ->
    ('elt, r) fin_mes ->
    'elt

  (** Compute the mean of a finite measure supported by r. *)
  val mean : (r, r) fin_mes -> r

  (** Compute the variance of a finite measure supported by r. *)
  val variance : (r, r) fin_mes -> r

  (** Returns the raw data underlying a finitely supported measure. *)
  val raw_data_measure : ('t, r) fin_mes -> [> `Measure of ('t * r) list ]

  (** Returns the raw data underlying a finitely supported probability. *)
  val raw_data_probability :
    ('t, r) fin_prb -> [> `Probability of ('t * r) list ]

  (** Pretty print a measure, with elements sorted according to
      the order relation on the support. *)
  val pp_fin_mes : Format.formatter -> ('a, r) fin_mes -> unit

  (** Pretty print a measure, with elements sorted by increasing measure.. *)
  val pp_fin_mes_by_measure : Format.formatter -> ('a, r) fin_mes -> unit

  (** Fold over the union of the supports of the given measures. *)
  val fold_union : ('t -> r -> r -> 'a -> 'a) -> 't mes -> 't mes -> 'a -> 'a
end

(** We use an OCamlgraph-compatible module type to describe
    undirected graphs. We assume that all graphs are undirected
    and simple. *)
module type Graph = sig
  type t

  module V : Basic_intf.Std

  type vertex = V.t

  type edge

  val nb_vertex : t -> int

  val nb_edges : t -> int

  val out_degree : t -> vertex -> int

  val mem_vertex : t -> vertex -> bool

  val mem_edge : t -> vertex -> vertex -> bool

  val succ : t -> vertex -> vertex list

  val succ_e : t -> vertex -> edge list

  val iter_vertex : (vertex -> unit) -> t -> unit

  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_edges : (vertex -> vertex -> unit) -> t -> unit

  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit

  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end
