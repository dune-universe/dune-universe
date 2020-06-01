open! Core_kernel
open! Import

(** Access the time as a span since [Time_ns.epoch] *)
val span_since_epoch : (_, Time_ns.Span.t, Time_ns.t, [< isomorphism ]) Accessor.Simple.t

(** [date_ofday], [date] and [ofday] present a time as a [Date.t * Time_ns.Ofday.t]. They
    are not well behaved accessors during time shifts (e.g. when transitioning to or from
    daylight savings time). See the explanation of well-behaveness in the [Accessor]
    documentation. *)

val date_ofday
  :  Time.Zone.t
  -> (_, Date.t * Time_ns.Ofday.t, Time_ns.t, [< isomorphism ]) Accessor.Simple.t

val date : Time.Zone.t -> (_, Date.t, Time_ns.t, [< field ]) Accessor.Simple.t
val ofday : Time.Zone.t -> (_, Time_ns.Ofday.t, Time_ns.t, [< field ]) Accessor.Simple.t
