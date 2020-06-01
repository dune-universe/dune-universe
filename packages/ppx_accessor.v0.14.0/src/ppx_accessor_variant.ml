open! Base
open! Import

type t = Constructor.t list

let of_constructor_declarations = List.map ~f:Constructor.of_constructor_declaration

let to_strs t ~loc ~type_name =
  Common.map_with_context t ~f:(fun constructor ~context:other_constructors ->
    let wildcard = Constructor.wildcard_patterns other_constructors ~loc in
    Constructor.to_strs constructor ~wildcard ~type_name)
  |> List.concat
;;
