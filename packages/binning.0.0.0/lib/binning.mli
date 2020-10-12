(** A datastructure (based on Hashtbl) to accumulate values in bins.

    A value of type [Binning.t] can be seen as a generalized
   histogram: values are mapped to bins, and each bin keeps a summary
   value that is updated when an element is added (like a count, the
   list of elements, their mean, etc).  *)

(** {7 Generic API} *)

type ('a, 'bin, 'increment, 'summary) t
(** General type for binnings: values of type ['a] are mapped to
   ['bin]s, holding a ['summary] value updated using an ['increment] *)

val create :
  ?n:int ->
  bin:('a -> 'b) ->
  zero:'d ->
  add:('c -> 'd -> 'd) ->
  unit ->
  ('a, 'b, 'c, 'd) t
(** [create ~n ~zero ~bin ~add] creates a binning, which maps
    values to bins with [bin], uses [zero] as a neutral element
    (that is the value associated to a bin before any value has been
    added to it) and updates the value of a bin with [add]. [n] is an
    estimation of the maximum number of bins. *)

val add : ('a, _, 'c, _) t -> 'a -> 'c -> unit
(** [add accu x y] updates the value in [accu] for
    the bin of [x] by an increment [y] *)

val seq : (_, 'a, _, 'b) t -> ('a * 'b) Seq.t

val find : (_, 'a, _, 'b) t -> 'a -> 'b
(** [find accu x] returns the value associated to [b] in [accu],
   returns the neutral element of the binning if the bin is empty. *)

val transform :
  bin:('a -> 'b) ->
  zero:'d ->
  add:('c -> 'd -> 'd) ->
  ('a * 'c) Seq.t ->
  ('b * 'd) Seq.t

val transform1 :
  bin:('a -> 'b) ->
  zero:'c ->
  add:('a -> 'c -> 'c) ->
  'a Seq.t ->
  ('b * 'c) Seq.t

(** {7 Counters and histograms} *)

module Counter : sig
  type nonrec 'a t = ('a, 'a, int, int) t
  val create : ?n:int -> unit -> 'a t
  val tick : 'a t -> 'a -> unit
  val of_seq : ('a * int) Seq.t -> 'a t
end

val counts  : 'a Seq.t -> ('a * int) Seq.t

(** {7 Relation} *)

module Relation : sig
  type nonrec ('a, 'b) t = ('a, 'a, 'b, 'b list) t
  val create : ?n:int -> unit -> ('a,'b) t
  val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
end

val relation : ('a * 'b) Seq.t -> ('a * 'b list) Seq.t
