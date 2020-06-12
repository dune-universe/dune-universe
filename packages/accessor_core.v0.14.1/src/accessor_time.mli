open! Core_kernel
open! Import

(** Access the time as a span since [Time.epoch] *)
val span_since_epoch : (_, Time.Span.t, Time.t, [< isomorphism ]) Accessor.Simple.t

(** [date_ofday], [date] and [ofday] present a time as a [Date.t * Time.Ofday.t]. They are
    not well behaved accessors during time shifts (e.g. when transitioning to or from
    daylight savings time). See the explanation of well-behaveness in the [Accessor]
    documentation. *)

val date_ofday
  :  Time.Zone.t
  -> (_, Date.t * Time.Ofday.t, Time.t, [< isomorphism ]) Accessor.Simple.t

val date : Time.Zone.t -> (_, Date.t, Time.t, [< field ]) Accessor.Simple.t
val ofday : Time.Zone.t -> (_, Time.Ofday.t, Time.t, [< field ]) Accessor.Simple.t
