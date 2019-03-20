type param = {
  relation: string;
  word_positions: int list [@key "wordPositions"];
} [@@deriving of_yojson {strict = false}]

type t = {
  id: int;
  params: param list;
  word_positions: int list [@key "wordPositions"];
} [@@deriving of_yojson {strict = false}]
