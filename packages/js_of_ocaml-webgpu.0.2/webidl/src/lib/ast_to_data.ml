module Syntax = Webidl_syntax
open Syntax
open Data

let of_attribute qualifiers (type_with_ext, name) =
  let is_static = List.mem `Static qualifiers in
  let is_readonly = List.mem `ReadOnly qualifiers in
  let is_inherit = List.mem `Inherit qualifiers in
  `Attribute { type_with_ext; name; is_static; is_readonly; is_inherit }
;;

let of_operation specials is_static = function
  | `NoSpecialOperation (return_type, (ident, arguments)) ->
    `Operation { specials; is_static; return_type; ident; arguments }
  | `SpecialOperation (specials, return_type, (ident, arguments)) ->
    `Operation { specials; is_static; return_type; ident; arguments }
;;

let of_readonly = function
  | `Maplike (key_type, value_type) ->
    `Maplike { is_readonly = true; key_type; value_type }
  | `Setlike key_type -> `Setlike { is_readonly = true; key_type }
  | `AttributeRest attribute -> of_attribute [ `ReadOnly ] attribute
;;

let of_static = function
  | `ReadOnly (`AttributeRest attribute) ->
    of_attribute [ `ReadOnly; `Static ] attribute
  | `AttributeRest attribute -> of_attribute [ `Static ] attribute
  | `NoSpecialOperation _ as operation -> of_operation [] true operation
;;

let of_stringifier = function
  | `ReadOnly (`AttributeRest attribute) ->
    of_attribute [ `ReadOnly ] attribute
  | `AttributeRest attribute -> of_attribute [] attribute
  | `NoSpecialOperation _ as operation -> of_operation [] false operation
  | `None -> `None
;;

let of_interface_member : Ast.interface_member -> Data.interface_member
  = function
  | `Const const -> `Const const
  | `Operation operation -> of_operation [] false operation
  | `Stringifier stringifier -> `Stringifier (of_stringifier stringifier)
  | `Static static -> of_static static
  | `Iterable iterable -> `Iterable iterable
  | `Attribute (`Inherit (`AttributeRest attribute)) ->
    of_attribute [ `Inherit ] attribute
  | `Attribute (`AttributeRest attribute) -> of_attribute [] attribute
  | `ReadOnly member -> of_readonly member
  | `Maplike (key_type, value_type) ->
    `Maplike { is_readonly = false; key_type; value_type }
  | `Setlike key_type -> `Setlike { is_readonly = false; key_type }
;;

let of_mixin_member : Ast.mixin_member -> Data.mixin_member = function
  | `Const const -> `Const const
  | `Operation operation -> of_operation [] false operation
  | `Stringifier stringifier -> `Stringifier (of_stringifier stringifier)
  | `ReadOnly member -> of_readonly member
  | `Attribute (`Inherit (`AttributeRest attribute)) ->
    of_attribute [ `Inherit ] attribute
  | `Attribute (`AttributeRest attribute) -> of_attribute [] attribute
;;

let map_snd f dst = List.map (fun (x, y) -> x, f y) dst

let of_mixin (ident, members) =
  let mixin_members = map_snd of_mixin_member members in
  `Mixin { ident; mixin_members }
;;

let of_interface (ident, inheritance, members) =
  let interface_members = map_snd of_interface_member members in
  `Interface { ident; inheritance; interface_members }
;;

let of_namespace_member = function
  | `NoSpecialOperation _ as operation -> of_operation [] false operation
  | `ReadOnly (`AttributeRest attribute) ->
    of_attribute [ `ReadOnly ] attribute
  | `AttributeRest attribute -> of_attribute [] attribute
;;

let of_namespace (ident, members) =
  let namespace_members = map_snd of_namespace_member members in
  `Namespace { ident; namespace_members }
;;

let of_dictionary_member = function
  | `Required (type_with_ext, ident, default) ->
    { is_required = true; type_with_ext; ident; default }
  | `NotRequired (type_, ident, default) ->
    let type_with_ext = [], type_ in
    { is_required = false; type_with_ext; ident; default }
;;

let of_dictionary (ident, inheritance, members) =
  let dictionary_members = map_snd of_dictionary_member members in
  `Dictionary { ident; inheritance; dictionary_members }
;;

let of_partial = function
  | `PartialInterface (ident, members) ->
    of_interface (ident, None, members)
  | `Mixin mixin -> of_mixin mixin
  | `PartialDictionary (ident, members) ->
    of_dictionary (ident, None, members)
  | `Namespace namespace -> of_namespace namespace
;;

let of_callback = function
  | `CallbackRest (ident, return_type, arguments) ->
    of_operation
      []
      false
      (`NoSpecialOperation (return_type, (Some ident, arguments)))
  | `Interface interface -> of_interface interface
;;

let of_definition : Ast.definition -> Data.definition = function
  | `Callback callback -> `Callback (of_callback callback)
  | `Includes includes -> `Includes includes
  | `Interface interface -> of_interface interface
  | `Mixin mixin -> of_mixin mixin
  | `Namespace namespace -> of_namespace namespace
  | `Partial partial -> `Partial (of_partial partial)
  | `Dictionary dictionary -> of_dictionary dictionary
  | `Enum enum -> `Enum enum
  | `Typedef typedef -> `Typedef typedef
  | `Implements implements -> `Implements implements
;;

let of_difinitions definitions = map_snd of_definition definitions
