type t = {
  classifier_id: string [@key "classifierId"];
  id: string [@key "categoryId"];
  label: string;
  score: float;
} [@@deriving of_yojson {strict = false }]