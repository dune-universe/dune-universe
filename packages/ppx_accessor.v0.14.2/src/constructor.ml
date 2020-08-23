open! Base
open! Import

module Fields = struct
  type t =
    | Tuple of Tuple.t
    | Record of Record.t

  let of_constructor_arguments = function
    | Pcstr_tuple cts -> Tuple (Tuple.of_core_types cts)
    | Pcstr_record lds -> Record (Record.of_label_declarations lds)
  ;;
end

type t =
  { name : Name.t
  ; loc : Location.t
  ; fields : Fields.t
  }

let of_constructor_declaration cd =
  let { txt = name; loc } = cd.pcd_name in
  let name = Name.of_string name in
  let fields = Fields.of_constructor_arguments cd.pcd_args in
  { name; loc; fields }
;;

let wildcard_pattern { name; loc; fields } =
  let fields =
    match fields with
    | Tuple tuple -> Tuple.Inline.wildcard_pattern tuple ~loc
    | Record _ -> Some (ppat_any ~loc)
  in
  ppat_construct ~loc (Loc.make ~loc (Lident (Name.to_constructor_string name))) fields
;;

let wildcard_patterns ts ~loc =
  List.reduce (List.map ts ~f:wildcard_pattern) ~f:(ppat_or ~loc)
;;

let to_strs { name; loc; fields } ~wildcard ~type_name =
  match fields with
  | Tuple tuple -> [ Tuple.Inline.to_str tuple ~loc ~name ~wildcard ]
  | Record record ->
    Record.Inline.to_strs record ~type_name ~constructor_name:name ~wildcard ~loc
;;
