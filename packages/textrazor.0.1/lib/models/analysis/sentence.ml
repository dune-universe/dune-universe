type t = {
  position: int;
  words: Word.t list [@default []];
} [@@deriving of_yojson {strict = false }]
