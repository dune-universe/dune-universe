(** Data structure for noun phrases. *)
type t = {
  id: int;
  word_positions: int list;
} [@@deriving of_yojson]
