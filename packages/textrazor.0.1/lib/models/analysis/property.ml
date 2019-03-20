type t = {
  id: int;
  word_positions: int list [@key "wordPositions"];
  property_positions: int list [@key "propertyPositions"];
} [@@deriving of_yojson {strict = false}]
