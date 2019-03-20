type t = {
  confidence_score: float [@key "confidenceScore"];
  ending_pos: int [@key "endingPos"];
  entity_id: string [@key "entityId"];
  freebase_id: string option [@key "freebaseId"][@default None];
  freebase_types: string list [@key "freebaseTypes"][@default []];
  id: int;
  matched_text: string [@key "matchedText"];
  matching_tokens: int list [@key "matchingTokens"];
  relevance_score: float [@key "relevanceScore"];
  starting_pos: int [@key "startingPos"];
  types: string list [@key "type"][@default []];
  wiki_link: string option [@key "wikiLink"];
  wikidata_id: string option [@key "wikidataId"][@default None];
} [@@deriving of_yojson {strict = false }]

let wikidata_base_uri =
  Uri.of_string "http://www.wikidata.org"

let freebase_base_uri =
  Uri.of_string "http://g.co"

let wikidata_uri {wikidata_id; _} =
  match wikidata_id with
  | None -> None
  | Some id ->
    Some (Uri.with_path wikidata_base_uri (Filename.concat "/entity" id))

let freebase_uri {freebase_id; _} =
  match freebase_id with
  | None -> None
  | Some id ->
    Some (Uri.with_path freebase_base_uri (Filename.concat "/kg" id))

let wikipedia_url {wiki_link; _} =
  match wiki_link with
  | None -> None
  | Some url -> Some (Uri.of_string url)