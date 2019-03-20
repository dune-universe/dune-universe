(** Data structure for sentences. *)
type t = {
  position: int;
  words: Word.t list;
} [@@deriving of_yojson]
