module SMap : Map.S with type key := string
module FSet : Set.S with type elt := float
module FMap : Map.S with type key := float
module KLL : Kll.S with type elt := float

type 'a complex = {
  count: int ;
  sum: float ;
  data: 'a ;
}

type histogram = int FMap.t complex
type summary = (KLL.t * FSet.t) complex

val cumulate : int FMap.t -> int FMap.t
(** [cumulate hist] is the cumulative histogram of [hist], suitable to
    ingestion by Prometheus. *)

val complex_cum_fmap : int -> float -> int FMap.t -> histogram
(** [complex_cum count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a cumulated histogram or CDF. *)

val complex_cum : int -> float -> (float * int) list -> histogram
(** [complex_cum count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a cumulated histogram or
    CDF. *)

val complex : int -> float -> (float * int) list -> histogram
(** [complex count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a non-cumulated
    histogram. *)

module LabelsMap : Map.S with type key := string SMap.t
(** A kv map (label) uniquely identifying a time series. *)

type 'a metric = 'a series LabelsMap.t
(** Type of a metric. A metric associates a time series to a set of
    labels. *)

and 'a series = { ts: Ptime.t option ; v: 'a }
(** Type of a time series. Contains an optional timestamp. *)

val create_series : ?ts:Ptime.t -> 'a -> 'a series

type t
(** Type of a Prometheus metric. Contains a name, an optional help
    text, a type, and labels associated to a value. *)

val add_labels : (string * string) list -> t -> t
(** [add_labels labels t] will add [labels] to all series in [t]. *)

val merge : t -> t -> t

val pp : t Fmt.t
val pp_list : t list Fmt.t

val counter   : ?help:string -> string -> ((string * string) list * float series) list -> t
val gauge     : ?help:string -> string -> ((string * string) list * float series) list -> t
val histogram : ?help:string -> string -> ((string * string) list * histogram series) list -> t
val summary   : ?help:string -> string -> ((string * string) list * summary series) list -> t
