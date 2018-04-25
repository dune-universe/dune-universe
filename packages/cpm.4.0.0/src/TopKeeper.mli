type t

(** [create n] creates a [Top_keeper.t] that will keep up to [n] best elements *)
val create: int -> t

(** [add name score] add score [score] to the top_keeper under name [name] (if the score
    is good enough or if the top_keeper doesn't hold enough elements currently) *)
val add : string -> float -> t -> t

(** [high_scores_first tk] retrieves the [n] best scores (with associated names) that were kept by [tk] *)
val high_scores_first : t -> (float * string) list
