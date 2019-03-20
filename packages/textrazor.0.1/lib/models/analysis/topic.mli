(** Data structure for topics. *)
type t = {
  id: int;
  label: string;
  score: float;
  wiki_link: string option;
  wikidata_id: string option;
} [@@deriving of_yojson]

(** Gets the Wikidata URI of the topic, if applicable. *)
val wikidata_uri : t -> Uri.t option

(** Gets the Wikipedia URL of the topic, if applicable. *)
val wikipedia_url : t -> Uri.t option