(** The [prbnmcn-stats] statistical library. *)

(** {1:introduction Introduction}

    [prbnmcn-stats] ([stats] for short) was designed to construct simple probabilistic models and compute
    basic statistics over empirical data obtained from various experiments.

    The core data type is that of a probability distribution. Distributions come
    under various guises.
    - Empirical distributions are the typical outcome of performing a bunch of measurements or
      experiments, concretely they are nothing more than a collection of data.
      The empirical mean, empirical variance and quantiles are typical statistics that
      can be computed on empirical distributions.
    - Generative distributions a.k.a. samplers are used when constructing
      probabilistic models, they are typically used to {e generate} synthetic data.
    - Measures are the mathematical representation of a distribution,
      associating weights to (sets of) data. This is the form we use to compute
      statistics such as the mean, the variance, etc of a distribution.
      Typically, we manipulate only finitely supported measures.

    [stats] provides modules dedicated for each case, and maps between the various
    forms under which probability distributions appear.
*)

module Stats_intf = Stats_intf
module Fin = Fin
module Emp = Emp
module Gen = Gen
module Pdfs = Pdfs
module Combi = Combi
module Mh = Mh
module Finbij = Finbij
module Graph = Graph
module Log_space = Log_space
module Binning = Binning
