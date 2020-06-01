open! Base
open! Import
module Variant = Ppx_accessor_variant
open Common

module Kind = struct
  type t =
    | Record of Record.t
    | Variant of Variant.t
    | Polymorphic_variant of Polymorphic_variant.t

  let of_type_kind tk manifest ~loc =
    match tk with
    | Ptype_abstract ->
      (match manifest with
       | None -> unsupported ~loc "abstract type"
       | Some ct -> Polymorphic_variant (Polymorphic_variant.of_core_type ct))
    | Ptype_variant cds -> Variant (Variant.of_constructor_declarations cds)
    | Ptype_record lds -> Record (Record.of_label_declarations lds)
    | Ptype_open -> unsupported ~loc "open type"
  ;;
end

type t =
  { name : string
  ; loc : Location.t
  ; kind : Kind.t
  }

let of_type_declaration td =
  let { txt = name; loc } = td.ptype_name in
  let kind = Kind.of_type_kind td.ptype_kind td.ptype_manifest ~loc in
  { name; loc; kind }
;;

let to_strs { name; loc; kind } =
  match kind with
  | Record record -> Record.to_strs record ~type_name:name
  | Variant variant -> Variant.to_strs variant ~loc ~type_name:name
  | Polymorphic_variant polymorphic_variant ->
    Polymorphic_variant.to_strs polymorphic_variant ~loc
;;
