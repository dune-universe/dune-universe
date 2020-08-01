open Core

type primitive =
  [ `Boolean
  | `Byte
  | `Octet
  | `Unrestricted of [ `Float | `Double ]
  | `Float
  | `Double
  | `Unsigned of [ `Short | `Long | `LongLong ]
  | `Short
  | `Long
  | `LongLong
  ]
[@@deriving sexp]

type string_type =
  [ `ByteString
  | `DOMString
  | `USVString
  ]
[@@deriving sexp]

type buffer =
  [ `ArrayBuffer
  | `DataView
  | `Int8Array
  | `Int16Array
  | `Int32Array
  | `Uint8Array
  | `Uint16Array
  | `Uint32Array
  | `Uint8Clampedarray
  | `Float32Array
  | `Float64Array
  ]
[@@deriving sexp]

type 'type_with_ext nullable_non_any_aux =
  [ primitive
  | string_type
  | `Ident of string
  | `Sequence of 'type_with_ext
  | `Object
  | `Error
  | `DomException
  | buffer
  | `FrozenArray of 'type_with_ext
  | `Record of string_type * 'type_with_ext
  ]
[@@deriving sexp]

type ('type_with_ext, 'union_type) nullable_union_aux =
  [ 'type_with_ext nullable_non_any_aux
  | `Union of 'union_type
  ]
[@@deriving sexp]

type ('type_with_ext, 'return_type) non_any_aux =
  [ `Promise of 'return_type
  | `Nullable of 'type_with_ext nullable_non_any_aux
  | 'type_with_ext nullable_non_any_aux
  ]
[@@deriving sexp]

type ('type_with_ext, 'return_type, 'union_type, 'extends) union_member_aux =
  [ `NonAny of 'extends * ('type_with_ext, 'return_type) non_any_aux
  | `Union of 'union_type
  | `Nullable of [ `Union of 'union_type ]
  ]
[@@deriving sexp]

type ('type_with_ext, 'return_type, 'union_type, 'extends) union_type_aux =
  ('type_with_ext, 'return_type, 'union_type, 'extends) union_member_aux
  list
[@@deriving sexp]

type ('type_with_ext, 'return_type, 'union_type) type_aux =
  [ `Promise of 'return_type
  | 'type_with_ext nullable_non_any_aux
  | `Any
  | `Nullable of ('type_with_ext, 'union_type) nullable_union_aux
  | `Union of 'union_type
  ]
[@@deriving sexp]

type ('type_with_ext, 'return_type, 'union_type) return_type_aux =
  [ ('type_with_ext, 'return_type, 'union_type) type_aux
  | `Void
  ]
[@@deriving sexp]

type type_with_ext = extends * type_ [@@deriving sexp]

and type_ = (type_with_ext, return_type, union_type) type_aux
[@@deriving sexp]

and return_type = (type_with_ext, return_type, union_type) return_type_aux
[@@deriving sexp]

and union_type =
  (type_with_ext, return_type, union_type, extends) union_type_aux
[@@deriving sexp]

and nullable_non_any = type_with_ext nullable_non_any_aux [@@deriving sexp]

and non_any = (type_with_ext, return_type) non_any_aux [@@deriving sexp]

and const_value =
  [ `Bool of bool
  | `Float of float
  | `Int of int
  | `Null
  ]
[@@deriving sexp]

and const =
  [ primitive
  | `Ident of string
  ]
[@@deriving sexp]

and default_value =
  [ `Const of const_value
  | `String of string
  | `EmptySequence
  ]
[@@deriving sexp]

and argument =
  [ `Optional of type_with_ext * string * default_value option
  | `Variadic of type_ * string
  | `Fixed of type_ * string
  ]
[@@deriving sexp]

and extended_argument = extends * argument

and extended_attribute =
  [ `Custom of string
  | `NoArgs of string
  | `ArgumentList of string * extended_argument list
  | `NamedArgList of string * string * extended_argument list
  | `Ident of string * string
  | `IdentList of string * string list
  ]
[@@deriving sexp]

and extends = extended_attribute list [@@deriving sexp]

type special =
  [ `Getter
  | `Setter
  | `Deleter
  | `Legacycaller
  ]
[@@deriving sexp]

type operation_rest = string option * (extends * argument) list
[@@deriving sexp]

type no_special_operation =
  [ `NoSpecialOperation of return_type * operation_rest ]
[@@deriving sexp]

type operation =
  [ no_special_operation
  | `SpecialOperation of special list * return_type * operation_rest
  ]
[@@deriving sexp]

type dictionary_member =
  [ `Required of type_with_ext * string * default_value option
  | `NotRequired of type_ * string * default_value option
  ]
[@@deriving sexp]

type dictionary =
  string * string option * (extends * dictionary_member) list
[@@deriving sexp]

type attribute_rest = [ `AttributeRest of type_with_ext * string ]
[@@deriving sexp]

type operation_or_attribute =
  [ no_special_operation
  | attribute_rest
  ]
[@@deriving sexp]

type namespace_member =
  [ no_special_operation
  | `ReadOnly of attribute_rest
  ]
[@@deriving sexp]

type namespace = string * (extends * namespace_member) list
[@@deriving sexp]

type maplike = type_with_ext * type_with_ext [@@deriving sexp]
type setlike = type_with_ext [@@deriving sexp]

type const_type =
  [ const
  | `Nullable of const
  ]
[@@deriving sexp]

type static_member =
  [ operation_or_attribute
  | `ReadOnly of attribute_rest
  ]
[@@deriving sexp]

type readonly_member =
  [ `Maplike of maplike
  | `Setlike of setlike
  | attribute_rest
  ]
[@@deriving sexp]

type attribute =
  [ `Inherit of attribute_rest
  | attribute_rest
  ]
[@@deriving sexp]

type interface_member =
  [ `ReadOnly of readonly_member
  | `Static of static_member
  | `Const of const_type * string * const_value
  | `Operation of operation
  | `Stringifier of [ static_member | `None ]
  | `Iterable of type_with_ext * type_with_ext option
  | `Attribute of attribute
  | `Maplike of maplike
  | `Setlike of setlike
  ]
[@@deriving sexp]

type interface = string * string option * (extends * interface_member) list
[@@deriving sexp]

type mixin_member =
  [ `Attribute of attribute
  | `ReadOnly of readonly_member
  | `Const of const_type * string * const_value
  | `Operation of operation
  | `Stringifier of [ static_member | `None ]
  ]
[@@deriving sexp]

type mixin = string * (extends * mixin_member) list [@@deriving sexp]

type partial =
  [ `PartialInterface of string * (extends * interface_member) list
  | `Mixin of mixin
  | `PartialDictionary of string * (extends * dictionary_member) list
  | `Namespace of namespace
  ]
[@@deriving sexp]

type callback =
  [ `CallbackRest of string * return_type * (extends * argument) list
  | `Interface of interface
  ]
[@@deriving sexp]

type definition =
  [ `Callback of callback
  | `Interface of interface
  | `Mixin of mixin
  | `Namespace of namespace
  | `Partial of partial
  | `Dictionary of dictionary
  | `Enum of string * string list
  | `Typedef of type_with_ext * string
  | `Includes of string * string
  | `Implements of string * string
  ]
[@@deriving sexp]

type definitions = (extends * definition) list [@@deriving sexp]
