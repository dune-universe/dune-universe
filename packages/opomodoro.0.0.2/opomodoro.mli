type t

(** Default state for timer **)
val timer_initial : t

(** Advance state of timer **)
val advance_state : t -> t

(** Main state machine loop **)
val loop: t -> 'a
