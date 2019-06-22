
type 'a t

(** [create n] creates a ['a Top_keeper.t] that will keep up to [n]
    best scoring elements *)
val create: int -> 'a t

(** [add elt score t] add [elt] with [score] to the top_keeper [t]
    (if the score is good enough or if the top_keeper
    doesn't hold enough elements yet) *)
val add : 'a -> float -> 'a t -> 'a t

(** [high_scores_first t] retrieves the [n] best scores from [t]
    (with associated elements) *)
val high_scores_first : 'a t -> (float * 'a) list
