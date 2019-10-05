[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module Syntax : sig
  type t =
    | Syntax_proto2
    | Syntax_proto3
  [@@deriving eq, show]

  val default : unit -> t

  val to_int : t -> int

  val of_int : int -> t option

  val to_string : t -> string

  val of_string : string -> t option
end = struct
  type t =
    | Syntax_proto2
    | Syntax_proto3
  [@@deriving eq, show]

  let default =
    fun () -> Syntax_proto2

  let to_int =
    function
    | Syntax_proto2 -> 0
    | Syntax_proto3 -> 1

  let of_int =
    function
    | 0 -> Some Syntax_proto2
    | 1 -> Some Syntax_proto3
    | _ -> None

  let to_string =
    function
    | Syntax_proto2 -> "SYNTAX_PROTO2"
    | Syntax_proto3 -> "SYNTAX_PROTO3"

  let of_string =
    function
    | "SYNTAX_PROTO2" -> Some Syntax_proto2
    | "SYNTAX_PROTO3" -> Some Syntax_proto3
    | _ -> None
end

module rec Type' : sig
  type t = {
    name : string;
    fields : Field.t list;
    oneofs : string list;
    options : Option.t list;
    source_context : Google_protobuf_source_context_pc.Source_context.t option;
    syntax : Syntax.t;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    fields : Field.t list;
    oneofs : string list;
    options : Option.t list;
    source_context : Google_protobuf_source_context_pc.Source_context.t option;
    syntax : Syntax.t;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; fields; oneofs; options; source_context; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_repeated_user_field 2 Field.to_binary fields _o >>= fun () ->
      Bin'.serialize_repeated_field 3 Field'.String_t oneofs _o >>= fun () ->
      Bin'.serialize_repeated_user_field 4 Option.to_binary options _o >>= fun () ->
      Bin'.serialize_user_field 5 Google_protobuf_source_context_pc.Source_context.to_binary source_context _o >>= fun () ->
      Bin'.serialize_enum_field 6 Syntax.to_int syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_repeated_user_field 2 Field.of_binary _m >>= fun fields ->
      Bin'.decode_repeated_field 3 Field'.String_t _m >>= fun oneofs ->
      Bin'.decode_repeated_user_field 4 Option.of_binary _m >>= fun options ->
      Bin'.decode_user_field 5 Google_protobuf_source_context_pc.Source_context.of_binary _m >>= fun source_context ->
      Bin'.decode_enum_field 6 Syntax.of_int Syntax.default _m >>= fun syntax ->
      Ok { name; fields; oneofs; options; source_context; syntax }

  let rec to_text =
    fun { name; fields; oneofs; options; source_context; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_repeated_user_field "fields" Field.to_text fields _o >>= fun () ->
      Text'.serialize_repeated_field "oneofs" Field'.String_t oneofs _o >>= fun () ->
      Text'.serialize_repeated_user_field "options" Option.to_text options _o >>= fun () ->
      Text'.serialize_user_field "source_context" Google_protobuf_source_context_pc.Source_context.to_text source_context _o >>= fun () ->
      Text'.serialize_enum_field "syntax" Syntax.to_string syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_repeated_user_field "fields" Field.of_text _m >>= fun fields ->
      Text'.decode_repeated_field "oneofs" Field'.String_t _m >>= fun oneofs ->
      Text'.decode_repeated_user_field "options" Option.of_text _m >>= fun options ->
      Text'.decode_user_field "source_context" Google_protobuf_source_context_pc.Source_context.of_text _m >>= fun source_context ->
      Text'.decode_enum_field "syntax" Syntax.of_string Syntax.default _m >>= fun syntax ->
      Ok { name; fields; oneofs; options; source_context; syntax }
end

and Field : sig
  module Kind : sig
    type t =
      | Type_unknown
      | Type_double
      | Type_float
      | Type_int64
      | Type_uint64
      | Type_int32
      | Type_fixed64
      | Type_fixed32
      | Type_bool
      | Type_string
      | Type_group
      | Type_message
      | Type_bytes
      | Type_uint32
      | Type_enum
      | Type_sfixed32
      | Type_sfixed64
      | Type_sint32
      | Type_sint64
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end
  
  module Cardinality : sig
    type t =
      | Cardinality_unknown
      | Cardinality_optional
      | Cardinality_required
      | Cardinality_repeated
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end

  type t = {
    kind : Field.Kind.t;
    cardinality : Field.Cardinality.t;
    number : int;
    name : string;
    type_url : string;
    oneof_index : int;
    packed : bool;
    options : Option.t list;
    json_name : string;
    default_value : string;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module Kind : sig
    type t =
      | Type_unknown
      | Type_double
      | Type_float
      | Type_int64
      | Type_uint64
      | Type_int32
      | Type_fixed64
      | Type_fixed32
      | Type_bool
      | Type_string
      | Type_group
      | Type_message
      | Type_bytes
      | Type_uint32
      | Type_enum
      | Type_sfixed32
      | Type_sfixed64
      | Type_sint32
      | Type_sint64
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | Type_unknown
      | Type_double
      | Type_float
      | Type_int64
      | Type_uint64
      | Type_int32
      | Type_fixed64
      | Type_fixed32
      | Type_bool
      | Type_string
      | Type_group
      | Type_message
      | Type_bytes
      | Type_uint32
      | Type_enum
      | Type_sfixed32
      | Type_sfixed64
      | Type_sint32
      | Type_sint64
    [@@deriving eq, show]
  
    let default =
      fun () -> Type_unknown
  
    let to_int =
      function
      | Type_unknown -> 0
      | Type_double -> 1
      | Type_float -> 2
      | Type_int64 -> 3
      | Type_uint64 -> 4
      | Type_int32 -> 5
      | Type_fixed64 -> 6
      | Type_fixed32 -> 7
      | Type_bool -> 8
      | Type_string -> 9
      | Type_group -> 10
      | Type_message -> 11
      | Type_bytes -> 12
      | Type_uint32 -> 13
      | Type_enum -> 14
      | Type_sfixed32 -> 15
      | Type_sfixed64 -> 16
      | Type_sint32 -> 17
      | Type_sint64 -> 18
  
    let of_int =
      function
      | 0 -> Some Type_unknown
      | 1 -> Some Type_double
      | 2 -> Some Type_float
      | 3 -> Some Type_int64
      | 4 -> Some Type_uint64
      | 5 -> Some Type_int32
      | 6 -> Some Type_fixed64
      | 7 -> Some Type_fixed32
      | 8 -> Some Type_bool
      | 9 -> Some Type_string
      | 10 -> Some Type_group
      | 11 -> Some Type_message
      | 12 -> Some Type_bytes
      | 13 -> Some Type_uint32
      | 14 -> Some Type_enum
      | 15 -> Some Type_sfixed32
      | 16 -> Some Type_sfixed64
      | 17 -> Some Type_sint32
      | 18 -> Some Type_sint64
      | _ -> None
  
    let to_string =
      function
      | Type_unknown -> "TYPE_UNKNOWN"
      | Type_double -> "TYPE_DOUBLE"
      | Type_float -> "TYPE_FLOAT"
      | Type_int64 -> "TYPE_INT64"
      | Type_uint64 -> "TYPE_UINT64"
      | Type_int32 -> "TYPE_INT32"
      | Type_fixed64 -> "TYPE_FIXED64"
      | Type_fixed32 -> "TYPE_FIXED32"
      | Type_bool -> "TYPE_BOOL"
      | Type_string -> "TYPE_STRING"
      | Type_group -> "TYPE_GROUP"
      | Type_message -> "TYPE_MESSAGE"
      | Type_bytes -> "TYPE_BYTES"
      | Type_uint32 -> "TYPE_UINT32"
      | Type_enum -> "TYPE_ENUM"
      | Type_sfixed32 -> "TYPE_SFIXED32"
      | Type_sfixed64 -> "TYPE_SFIXED64"
      | Type_sint32 -> "TYPE_SINT32"
      | Type_sint64 -> "TYPE_SINT64"
  
    let of_string =
      function
      | "TYPE_UNKNOWN" -> Some Type_unknown
      | "TYPE_DOUBLE" -> Some Type_double
      | "TYPE_FLOAT" -> Some Type_float
      | "TYPE_INT64" -> Some Type_int64
      | "TYPE_UINT64" -> Some Type_uint64
      | "TYPE_INT32" -> Some Type_int32
      | "TYPE_FIXED64" -> Some Type_fixed64
      | "TYPE_FIXED32" -> Some Type_fixed32
      | "TYPE_BOOL" -> Some Type_bool
      | "TYPE_STRING" -> Some Type_string
      | "TYPE_GROUP" -> Some Type_group
      | "TYPE_MESSAGE" -> Some Type_message
      | "TYPE_BYTES" -> Some Type_bytes
      | "TYPE_UINT32" -> Some Type_uint32
      | "TYPE_ENUM" -> Some Type_enum
      | "TYPE_SFIXED32" -> Some Type_sfixed32
      | "TYPE_SFIXED64" -> Some Type_sfixed64
      | "TYPE_SINT32" -> Some Type_sint32
      | "TYPE_SINT64" -> Some Type_sint64
      | _ -> None
  end
  
  module Cardinality : sig
    type t =
      | Cardinality_unknown
      | Cardinality_optional
      | Cardinality_required
      | Cardinality_repeated
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | Cardinality_unknown
      | Cardinality_optional
      | Cardinality_required
      | Cardinality_repeated
    [@@deriving eq, show]
  
    let default =
      fun () -> Cardinality_unknown
  
    let to_int =
      function
      | Cardinality_unknown -> 0
      | Cardinality_optional -> 1
      | Cardinality_required -> 2
      | Cardinality_repeated -> 3
  
    let of_int =
      function
      | 0 -> Some Cardinality_unknown
      | 1 -> Some Cardinality_optional
      | 2 -> Some Cardinality_required
      | 3 -> Some Cardinality_repeated
      | _ -> None
  
    let to_string =
      function
      | Cardinality_unknown -> "CARDINALITY_UNKNOWN"
      | Cardinality_optional -> "CARDINALITY_OPTIONAL"
      | Cardinality_required -> "CARDINALITY_REQUIRED"
      | Cardinality_repeated -> "CARDINALITY_REPEATED"
  
    let of_string =
      function
      | "CARDINALITY_UNKNOWN" -> Some Cardinality_unknown
      | "CARDINALITY_OPTIONAL" -> Some Cardinality_optional
      | "CARDINALITY_REQUIRED" -> Some Cardinality_required
      | "CARDINALITY_REPEATED" -> Some Cardinality_repeated
      | _ -> None
  end

  type t = {
    kind : Field.Kind.t;
    cardinality : Field.Cardinality.t;
    number : int;
    name : string;
    type_url : string;
    oneof_index : int;
    packed : bool;
    options : Option.t list;
    json_name : string;
    default_value : string;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { kind; cardinality; number; name; type_url; oneof_index; packed; options; json_name; default_value } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_enum_field 1 Field.Kind.to_int kind _o >>= fun () ->
      Bin'.serialize_enum_field 2 Field.Cardinality.to_int cardinality _o >>= fun () ->
      Bin'.serialize_field 3 Field'.Int32_t number _o >>= fun () ->
      Bin'.serialize_field 4 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_field 6 Field'.String_t type_url _o >>= fun () ->
      Bin'.serialize_field 7 Field'.Int32_t oneof_index _o >>= fun () ->
      Bin'.serialize_field 8 Field'.Bool_t packed _o >>= fun () ->
      Bin'.serialize_repeated_user_field 9 Option.to_binary options _o >>= fun () ->
      Bin'.serialize_field 10 Field'.String_t json_name _o >>= fun () ->
      Bin'.serialize_field 11 Field'.String_t default_value _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_enum_field 1 Field.Kind.of_int Field.Kind.default _m >>= fun kind ->
      Bin'.decode_enum_field 2 Field.Cardinality.of_int Field.Cardinality.default _m >>= fun cardinality ->
      Bin'.decode_field 3 Field'.Int32_t _m >>= fun number ->
      Bin'.decode_field 4 Field'.String_t _m >>= fun name ->
      Bin'.decode_field 6 Field'.String_t _m >>= fun type_url ->
      Bin'.decode_field 7 Field'.Int32_t _m >>= fun oneof_index ->
      Bin'.decode_field 8 Field'.Bool_t _m >>= fun packed ->
      Bin'.decode_repeated_user_field 9 Option.of_binary _m >>= fun options ->
      Bin'.decode_field 10 Field'.String_t _m >>= fun json_name ->
      Bin'.decode_field 11 Field'.String_t _m >>= fun default_value ->
      Ok { kind; cardinality; number; name; type_url; oneof_index; packed; options; json_name; default_value }

  let rec to_text =
    fun { kind; cardinality; number; name; type_url; oneof_index; packed; options; json_name; default_value } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_enum_field "kind" Field.Kind.to_string kind _o >>= fun () ->
      Text'.serialize_enum_field "cardinality" Field.Cardinality.to_string cardinality _o >>= fun () ->
      Text'.serialize_field "number" Field'.Int32_t number _o >>= fun () ->
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_field "type_url" Field'.String_t type_url _o >>= fun () ->
      Text'.serialize_field "oneof_index" Field'.Int32_t oneof_index _o >>= fun () ->
      Text'.serialize_field "packed" Field'.Bool_t packed _o >>= fun () ->
      Text'.serialize_repeated_user_field "options" Option.to_text options _o >>= fun () ->
      Text'.serialize_field "json_name" Field'.String_t json_name _o >>= fun () ->
      Text'.serialize_field "default_value" Field'.String_t default_value _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_enum_field "kind" Field.Kind.of_string Field.Kind.default _m >>= fun kind ->
      Text'.decode_enum_field "cardinality" Field.Cardinality.of_string Field.Cardinality.default _m >>= fun cardinality ->
      Text'.decode_field "number" Field'.Int32_t _m >>= fun number ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_field "type_url" Field'.String_t _m >>= fun type_url ->
      Text'.decode_field "oneof_index" Field'.Int32_t _m >>= fun oneof_index ->
      Text'.decode_field "packed" Field'.Bool_t _m >>= fun packed ->
      Text'.decode_repeated_user_field "options" Option.of_text _m >>= fun options ->
      Text'.decode_field "json_name" Field'.String_t _m >>= fun json_name ->
      Text'.decode_field "default_value" Field'.String_t _m >>= fun default_value ->
      Ok { kind; cardinality; number; name; type_url; oneof_index; packed; options; json_name; default_value }
end

and Enum : sig
  type t = {
    name : string;
    enumvalue : Enum_value.t list;
    options : Option.t list;
    source_context : Google_protobuf_source_context_pc.Source_context.t option;
    syntax : Syntax.t;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    enumvalue : Enum_value.t list;
    options : Option.t list;
    source_context : Google_protobuf_source_context_pc.Source_context.t option;
    syntax : Syntax.t;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; enumvalue; options; source_context; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_repeated_user_field 2 Enum_value.to_binary enumvalue _o >>= fun () ->
      Bin'.serialize_repeated_user_field 3 Option.to_binary options _o >>= fun () ->
      Bin'.serialize_user_field 4 Google_protobuf_source_context_pc.Source_context.to_binary source_context _o >>= fun () ->
      Bin'.serialize_enum_field 5 Syntax.to_int syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_repeated_user_field 2 Enum_value.of_binary _m >>= fun enumvalue ->
      Bin'.decode_repeated_user_field 3 Option.of_binary _m >>= fun options ->
      Bin'.decode_user_field 4 Google_protobuf_source_context_pc.Source_context.of_binary _m >>= fun source_context ->
      Bin'.decode_enum_field 5 Syntax.of_int Syntax.default _m >>= fun syntax ->
      Ok { name; enumvalue; options; source_context; syntax }

  let rec to_text =
    fun { name; enumvalue; options; source_context; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_repeated_user_field "enumvalue" Enum_value.to_text enumvalue _o >>= fun () ->
      Text'.serialize_repeated_user_field "options" Option.to_text options _o >>= fun () ->
      Text'.serialize_user_field "source_context" Google_protobuf_source_context_pc.Source_context.to_text source_context _o >>= fun () ->
      Text'.serialize_enum_field "syntax" Syntax.to_string syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_repeated_user_field "enumvalue" Enum_value.of_text _m >>= fun enumvalue ->
      Text'.decode_repeated_user_field "options" Option.of_text _m >>= fun options ->
      Text'.decode_user_field "source_context" Google_protobuf_source_context_pc.Source_context.of_text _m >>= fun source_context ->
      Text'.decode_enum_field "syntax" Syntax.of_string Syntax.default _m >>= fun syntax ->
      Ok { name; enumvalue; options; source_context; syntax }
end

and Enum_value : sig
  type t = {
    name : string;
    number : int;
    options : Option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    number : int;
    options : Option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; number; options } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_field 2 Field'.Int32_t number _o >>= fun () ->
      Bin'.serialize_repeated_user_field 3 Option.to_binary options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_field 2 Field'.Int32_t _m >>= fun number ->
      Bin'.decode_repeated_user_field 3 Option.of_binary _m >>= fun options ->
      Ok { name; number; options }

  let rec to_text =
    fun { name; number; options } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_field "number" Field'.Int32_t number _o >>= fun () ->
      Text'.serialize_repeated_user_field "options" Option.to_text options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_field "number" Field'.Int32_t _m >>= fun number ->
      Text'.decode_repeated_user_field "options" Option.of_text _m >>= fun options ->
      Ok { name; number; options }
end

and Option : sig
  type t = {
    name : string;
    value' : Google_protobuf_any_pc.Any.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    value' : Google_protobuf_any_pc.Any.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_user_field 2 Google_protobuf_any_pc.Any.to_binary value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_user_field 2 Google_protobuf_any_pc.Any.of_binary _m >>= fun value' ->
      Ok { name; value' }

  let rec to_text =
    fun { name; value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_user_field "value" Google_protobuf_any_pc.Any.to_text value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_user_field "value" Google_protobuf_any_pc.Any.of_text _m >>= fun value' ->
      Ok { name; value' }
end
