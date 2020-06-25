type bin = {
  center : float;
  (** the center of the bin *)

  count : int;
  (** the number of values in the bin *)
}
(** [bin] represents one of the bins in a 1D histogram. The bin is
    centered in [center] and its mass is [count], half of which is on
    either side of [center]. *)

type histogram

val bins : histogram -> bin list
(** [bins h] returns the list of bins, sorted by the bin center,
    comprising histogram [h] *)

val num_bins : histogram -> int
(** [num_bins h] returns the size of the histogram [h] in terms of the
    number of bins; equivalent to [List.length (bins h)] *)

val max_bins : histogram -> int
(** [max_bins h] returns the maximum number of bins of this histogram;
    when the number of unique values added to the histogram exceeds
    this [max_bins h], [h] becomes an approximation. *)

val total_count : histogram -> int
(** [total_count h] returns the number of values added to histogram [h] *)

val range : histogram -> (float * float) option
(** [range h] returns the minimum and maximum values seen in the
    construction of histogram h, or [None] if no values have yet been
    added *)

val create : int -> histogram
(** [create max_bins] creates a histogram with up to [max_bins] bins *)

val add : float -> histogram -> histogram
(** [add v h] adds a value to [v] to histogram [h], returning the
    updated histogram *)

val addc : float -> int -> histogram -> histogram
(** [addc v c h] adds a value to [v] to histogram [h] with count [c],
    returning the updated histogram.  It is equivalent to calling [add
    v h] [c] times. *)

val merge : histogram list -> int -> histogram
(** [merge h_list max_bins] creates a new histogram from the histograms
    in [h_list], whose size is no bigger than [max_bins] *)

val sum : histogram -> float -> float
(** [sum hist b] returns an estimate of the number of points in the
    interval [[−infinity,b]]. For an estimate to be feasible, [b] must
    be strictly between the left and right bin centers. Otherwise,
    [Not_found] is raised. *)

exception Empty

val uniform : histogram -> int -> (int * float) list
(** [uniform hist num_intervals] returns estimates of the quantiles of
    the distribution represented by histogram [hist]. The quantiles are
    associated with the boundaries of [num_interval] intervals, in
    ascending order. For example, [uniform hist 4] returns an estimate
    of the interquartile range, consisting of the 25-th percentile,
    50-th percentile (median) and 75-th percentile. (The minimum and
    maximum of the histogram are seperately available through the
    function [range]. [uniform] can return fewer than the desired
    quantiles in histograms that have extremely dense regions. Raises
    [Empty] if the histogram has not been [add]'d any data.*)

val mean : histogram -> float
(** [mean hist] returns an estimate of the mean of the distribution
    represented by [hist]. Raises [Empty] if the histogram has not been
    [add]'d any data. *)

val mean_stdev : histogram -> float * float
(** [mean_stdev hist] returns estimates of the mean and standard
    deviation of the distribution represented bh [hist]. Raises
    [Empty] if the histogram has not been [add]'d any data. *)
