(** Data structure for relation params. *)
type param = {
  relation: string;
  word_positions: int list;
} [@@deriving of_yojson]

(** Data structure for relations. *)
type t = {
  id: int;
  params: param list;
  word_positions: int list;
} [@@deriving of_yojson]
