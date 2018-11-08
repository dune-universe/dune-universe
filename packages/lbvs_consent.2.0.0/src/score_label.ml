
type molecule_name = string
type score = float
type index = int
type label = bool

type t = molecule_name * score * index * label

let create name score index is_active =
  (name, score, index, is_active)

let is_active: t -> bool = function
    (_name, _score, _index, flag) -> flag

let get_name: t -> string = function
    (name, _score, _index, _label) -> name

let to_string (name, score, index, flag) =
  Printf.sprintf "(%s,%.4f,%d,%b)" name score index flag
