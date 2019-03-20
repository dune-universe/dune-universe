type t = {
  id: int;
  word_positions: int list [@key "wordPositions"];
} [@@deriving of_yojson {strict = false}]
