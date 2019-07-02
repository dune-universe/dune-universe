(** An interruptible timer *)

(** Type of an interruptible timer. *)
type t

val create : Duration.t -> t
(** [create d] creates a new interruptible timer with initial duration [d] *)

val start : t -> unit
(** [start t] starts the given timer. *)

val stop : t -> unit
(** [stop t] stops the given timer. *)

val set_duration : Duration.t -> t -> unit
(** [set_duration d t] sets the duration of the given timer [t] to duration [d]. The timer will use this value on the next tick. *)

val set_callback : (unit -> unit) -> t -> unit
(** [set_callback f t] sets the notification callback for the given timer [t]. [f] is called to notify the user of the timer reaching 0. *)

val time : t -> Duration.t
(** [time t] retrieves the time remaining on timer [t]. *)

val paused : t -> bool
(** [paused t] retrieves the paused status of timer [t]. *)
