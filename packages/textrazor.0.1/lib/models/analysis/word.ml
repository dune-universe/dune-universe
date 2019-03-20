type t = {
  ending_pos: int [@key "endingPos"];
  lemma: string option [@default None];
  part_of_speech: string option [@key "partOfSpeech"][@default None];
  position: int;
  starting_pos: int [@key "startingPos"];
  stem: string;
  token: string;
} [@@deriving of_yojson {strict = false }]
