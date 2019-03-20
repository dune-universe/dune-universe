(** Data structure for properties. *)
type t = {
  id: int;
  word_positions: int list;
  property_positions: int list;
} [@@deriving of_yojson]
