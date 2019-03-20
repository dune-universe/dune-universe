(** Data structure for entailments. *)
type t = {
  context_score: float;
  entailed_words: string list;
  id: int;
  prior_score: float;
  score: float;
  word_positions: int list;
} [@@deriving of_yojson]