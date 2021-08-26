open Lang

let object_type =
  let string_of_object_type = function
    | Content _hash_type -> "cnt"
    | Directory -> "dir"
    | Release -> "rel"
    | Revision -> "rev"
    | Snapshot -> "snp"
  in
  fun fmt object_type ->
    Format.fprintf fmt "%s" (string_of_object_type object_type)

let identifier_core fmt ((v, t, id) : identifier_core) =
  Format.fprintf fmt "swh:%d:%a:%s" v object_type t id

let context_qualifier fmt = function
  | Anchor identifier ->
    Format.fprintf fmt "anchor=%a" identifier_core identifier
  | Origin url -> Format.fprintf fmt "origin=%s" url
  | Path url -> Format.fprintf fmt "path=%s" url
  | Visit identifier -> Format.fprintf fmt "visit=%a" identifier_core identifier

let qualifier fmt = function
  | Context q -> Format.fprintf fmt "%a" context_qualifier q
  | Fragment (fst_line, snd_line) -> begin
    Format.fprintf fmt "lines=%d" fst_line;
    match snd_line with
    | None -> ()
    | Some snd_line -> Format.fprintf fmt "-%d" snd_line
  end

let qualifiers fmt q =
  List.iter (fun q -> Format.fprintf fmt ";%a" qualifier q) q

let identifier fmt ((i, q) : Lang.identifier) =
  Format.fprintf fmt "%a%a" identifier_core i qualifiers q
