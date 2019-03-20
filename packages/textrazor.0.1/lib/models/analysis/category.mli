(** Data structure for categories. *)
type t = {
  classifier_id: string;
  id: string;
  label: string;
  score: float;
} [@@deriving of_yojson]