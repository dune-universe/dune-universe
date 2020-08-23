open! Base
open! Import

type t = Record_field.t list

let of_label_declarations = List.map ~f:Record_field.of_label_declaration

let to_strs t ~type_name =
  match t with
  | [ field ] -> [ Record_field.to_str_singleton field ~type_name ]
  | t -> List.map t ~f:(Record_field.to_str ~type_name)
;;

module Inline = struct
  let to_strs t ~type_name ~constructor_name ~wildcard ~loc =
    let strs =
      match t with
      | [ field ] ->
        [ Record_field.Inline.to_str_singleton
            field
            ~type_name
            ~constructor_name
            ~wildcard
        ]
      | t ->
        List.map t ~f:(fun field ->
          Record_field.Inline.to_str field ~type_name ~constructor_name ~wildcard)
    in
    match wildcard with
    | None -> strs
    | Some _ ->
      [ pstr_module
          ~loc
          (module_binding
             ~loc
             ~name:(Located.mk ~loc (Some (Name.to_constructor_string constructor_name)))
             ~expr:(pmod_structure ~loc strs))
      ]
  ;;
end
