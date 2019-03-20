(** Module for analysis options. *)
module Options : sig
  (** Available classifiers. *)
  type classifier = [
    `IAB | `IAB_content | `IPTC | `IPTC_media | `Custom of string
  ]

  (** Available extractors. *)
  type extractor = [
    `Entailments | `Entities | `Phrases | `Relations | `Topics | `Words
  ]

  (** Available cleanup modes. *)
  type cleanup_mode = [
    `Raw | `Tags | `HTML
  ]

  (** Data structure for analysis options. *)
  type t = {
    allow_overlap: bool;
    classifiers: classifier list;
    cleanup_mode: cleanup_mode option;
    dbpedia_types: string list;
    extractors: extractor list;
    freebase_types: string list;
    language: string option;
    return_cleaned_text: bool;
    return_raw_text: bool;
    user_agent: string option;
  }

  (** A sensible default for analysis options.

      You can use this value an customize it with functional updates.
      [{default with key1 = value1; key2 = value2}]
  *)
  val default : t

  (** Gets the POST params for the given options. *)
  val to_params : t -> (string * string list) list
end

(** Data structure for analysis results. *)
type t = {
  categories: Category.t list;
  coarse_topics: Topic.t list;
  cleaned_text: string option;
  entailments: Entailment.t list;
  entities: Entity.t list;
  language: string;
  phrases: Phrase.t list;
  properties: Property.t list;
  raw_text: string option;
  relations: Relation.t list;
  sentences: Sentence.t list;
  topics: Topic.t list;
} [@@deriving of_yojson]