(** [Jerboa.Path_mapping] has the type definition of the path mapping.*)

(** [Path_mapping.t] is a list of Path parts which will be used for matching.*)
type t = Path.t list

let path_mapping_composer path_part path_accumulator =
  Path.separator :: path_part :: path_accumulator

let compose_path_regex path_mapping =
  let path_wit_separator = Base.List.fold_right path_mapping ~f:path_mapping_composer ~init:[] in
  let regex_wit_separator = Base.List.map path_wit_separator ~f:(fun path -> path.regex) in
  let beginning_regex = [Re.bol] in
  let ending_regex = [Re.opt (Re.char '/'); Re.eol] in
  let final_regex = Base.List.concat [beginning_regex; regex_wit_separator; ending_regex] in
  Re.seq final_regex