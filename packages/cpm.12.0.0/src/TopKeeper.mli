
type 'a t

(** [create n] creates a ['a TopKeeper.t] that will keep up to [n]
    best scoring elements. [n] must be greater than 0. *)
val create: int -> 'a t

(** [add t score elt] add [elt] with [score] to the top_keeper [t]
    (if the score is good enough or if the top_keeper
    doesn't hold enough elements yet) *)
val add: 'a t -> float -> 'a -> unit

(** [high_scores_first t] retrieve the [n] best scores from [t]
    (with associated elements); scores are in decreasing order *)
val high_scores_first: 'a t -> (float * 'a) list

(** [get_min_score t] return the current minimum score in [t] *)
val get_min_score: 'a t -> float

(** [get_curr_size t] return the current number of elements in [t].
    Note that [get_curr_size t <= get_max_size t] is always true
    (it is the data-structure invariant). *)
val get_curr_size: 'a t -> int

(** [get_max_size t] return the number of elements chosen at creation time *)
val get_max_size: 'a t -> int
