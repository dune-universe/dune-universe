val parse: string -> Sentence.t
(** [parse s] parses the sentence s *)

val parse_opt: string -> Sentence.t option
(** [parse_opt s] parses the sentence s, failsafe *)

val next: in_channel -> Sentence.t option
(** [next ch] returns the next sentence read from ch *)

val next_coord: in_channel -> Coord.t option
(** [next_coord ch] returns the next coordinate read from ch *)