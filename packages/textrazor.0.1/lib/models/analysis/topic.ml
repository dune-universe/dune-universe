type t = {
  id: int;
  label: string;
  score: float;
  wiki_link: string option [@key "wikiLink"];
  wikidata_id: string option [@key "wikidataId"][@default None];
} [@@deriving of_yojson {strict = false }]

let wikidata_base_uri =
  Uri.of_string "http://www.wikidata.org"

let wikidata_uri {wikidata_id; _} =
  match wikidata_id with
  | None -> None
  | Some id ->
    Some (Uri.with_path wikidata_base_uri (Filename.concat "/entity" id))

let wikipedia_url {wiki_link; _} =
  match wiki_link with
  | None -> None
  | Some url -> Some (Uri.of_string url)