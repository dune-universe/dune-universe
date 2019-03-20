type t = {
  context_score: float [@key "contextScore"];
  entailed_words: string list [@key "entailedWords"][@default []];
  id: int;
  prior_score: float [@key "priorScore"];
  score: float;
  word_positions: int list [@key "wordPositions"][@default []];
} [@@deriving of_yojson {strict = false }]
