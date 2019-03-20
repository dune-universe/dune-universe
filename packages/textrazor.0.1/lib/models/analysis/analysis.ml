module Options = struct
  type classifier = [
    `IAB | `IAB_content | `IPTC | `IPTC_media | `Custom of string
  ]
  type extractor = [
    `Entailments | `Entities | `Phrases | `Relations | `Topics | `Words
  ]
  type cleanup_mode = [
    `Raw | `Tags | `HTML
  ]
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

  let extractor_to_string = function
  | `Entailments -> "entailments"
  | `Entities -> "entities"
  | `Phrases -> "phrases"
  | `Relations -> "relations"
  | `Topics -> "topics"
  | `Words -> "words"

  let classifier_to_string = function
  | `IAB -> "textrazor_iab"
  | `IAB_content -> "textrazor_iab_content_taxonomy"
  | `IPTC -> "textrazor_newscodes"
  | `IPTC_media -> "textrazor_mediatopics"
  | `Custom s -> s

  let cleanup_mode_to_string = function
  | `Raw -> "raw"
  | `Tags -> "stripTags"
  | `HTML -> "cleanHTML"

  let extractors_to_param {extractors; _} =
    ("extractors", List.map extractor_to_string extractors)

  let classifiers_to_param {classifiers; _} =
    ("classifiers", List.map classifier_to_string classifiers)

  let cleanup_mode_to_param {cleanup_mode; _} =
    let value = match cleanup_mode with
      | Some v -> [cleanup_mode_to_string v]
      | None -> []
    in ("cleanup.mode", value)

  let return_cleaned_to_param {return_cleaned_text; _} =
    let value = if return_cleaned_text then ["true"] else [] in
    ("cleanup.returnCleaned", value)

  let return_raw_to_param {return_raw_text; _} =
    let value = if return_raw_text then ["true"] else [] in
    ("cleanup.returnRaw", value)

  let user_agent_to_param {user_agent; _} =
    let value = match user_agent with
      | Some v -> [v]
      | None -> []
    in
    ("download.userAgent", value)
  
  let language_to_param {language; _} =
    let value = match language with
      | Some l -> [l]
      | None -> []
    in
    ("languageOverride", value)

  let dbpedia_types_to_param {dbpedia_types; _} =
    ("entities.filterDbpediaTypes", dbpedia_types)

  let freebase_types_to_param {freebase_types; _} =
    ("entities.filterFreebaseTypes", freebase_types)
  
  let allow_overlap_to_param {allow_overlap; _} =
    let value = if allow_overlap then [] else ["false"] in
    ("entities.allowOverlap", value)

  let default =
    {
      allow_overlap = true;
      dbpedia_types = [];
      extractors = [
        `Entailments; `Entities; `Phrases; `Relations; `Topics; `Words
      ];
      cleanup_mode = None;
      classifiers = [];
      freebase_types = [];
      language = None;
      return_cleaned_text = false;
      return_raw_text = false;
      user_agent = None;
    }
  
  let to_params t =
    List.filter
      (fun (_,v) -> v != [])
      [
        extractors_to_param t; classifiers_to_param t; cleanup_mode_to_param t;
        return_cleaned_to_param t; return_raw_to_param t; user_agent_to_param t;
        language_to_param t; dbpedia_types_to_param t; freebase_types_to_param t;
        allow_overlap_to_param t
      ]
end

type t = {
  categories: Category.t list [@default []];
  coarse_topics: Topic.t list [@key "coarseTopics"][@default []];
  cleaned_text: string option [@key "cleanedText"][@default None];
  entailments: Entailment.t list [@default []];
  entities: Entity.t list [@default []];
  language: string;
  phrases: Phrase.t list [@key "nounPhrases"][@default []];
  properties: Property.t list [@default []];
  raw_text: string option [@key "rawText"][@default None];
  relations: Relation.t list [@default []];
  sentences: Sentence.t list [@default []];
  topics: Topic.t list [@default []];
} [@@deriving of_yojson {strict = false}]