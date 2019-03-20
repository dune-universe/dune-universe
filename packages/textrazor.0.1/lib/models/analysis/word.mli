(** Data structure for words. *)
type t = {
  ending_pos: int;
  lemma: string option;
  part_of_speech: string option;
  position: int;
  starting_pos: int;
  stem: string;
  token: string;
} [@@deriving of_yojson]
