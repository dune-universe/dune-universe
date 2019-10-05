[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec File_descriptor_set : sig
  type t = {
    file : File_descriptor_proto.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    file : File_descriptor_proto.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { file } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 1 File_descriptor_proto.to_binary file _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 1 File_descriptor_proto.of_binary _m >>= fun file ->
      Ok { file }

  let rec to_text =
    fun { file } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "file" File_descriptor_proto.to_text file _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "file" File_descriptor_proto.of_text _m >>= fun file ->
      Ok { file }
end

and File_descriptor_proto : sig
  type t = {
    name : string option;
    package : string option;
    dependency : string list;
    public_dependency : int list;
    weak_dependency : int list;
    message_type : Descriptor_proto.t list;
    enum_type : Enum_descriptor_proto.t list;
    service : Service_descriptor_proto.t list;
    extension : Field_descriptor_proto.t list;
    options : File_options.t option;
    source_code_info : Source_code_info.t option;
    syntax : string option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string option;
    package : string option;
    dependency : string list;
    public_dependency : int list;
    weak_dependency : int list;
    message_type : Descriptor_proto.t list;
    enum_type : Enum_descriptor_proto.t list;
    service : Service_descriptor_proto.t list;
    extension : Field_descriptor_proto.t list;
    options : File_options.t option;
    source_code_info : Source_code_info.t option;
    syntax : string option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; package; dependency; public_dependency; weak_dependency; message_type; enum_type; service; extension; options; source_code_info; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.String_t package _o >>= fun () ->
      Bin'.serialize_repeated_field 3 Field'.String_t dependency _o >>= fun () ->
      Bin'.serialize_repeated_field 10 Field'.Int32_t public_dependency _o >>= fun () ->
      Bin'.serialize_repeated_field 11 Field'.Int32_t weak_dependency _o >>= fun () ->
      Bin'.serialize_repeated_user_field 4 Descriptor_proto.to_binary message_type _o >>= fun () ->
      Bin'.serialize_repeated_user_field 5 Enum_descriptor_proto.to_binary enum_type _o >>= fun () ->
      Bin'.serialize_repeated_user_field 6 Service_descriptor_proto.to_binary service _o >>= fun () ->
      Bin'.serialize_repeated_user_field 7 Field_descriptor_proto.to_binary extension _o >>= fun () ->
      Bin'.serialize_user_field 8 File_options.to_binary options _o >>= fun () ->
      Bin'.serialize_user_field 9 Source_code_info.to_binary source_code_info _o >>= fun () ->
      Bin'.serialize_optional_field 12 Field'.String_t syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_optional_field 2 Field'.String_t _m >>= fun package ->
      Bin'.decode_repeated_field 3 Field'.String_t _m >>= fun dependency ->
      Bin'.decode_repeated_field 10 Field'.Int32_t _m >>= fun public_dependency ->
      Bin'.decode_repeated_field 11 Field'.Int32_t _m >>= fun weak_dependency ->
      Bin'.decode_repeated_user_field 4 Descriptor_proto.of_binary _m >>= fun message_type ->
      Bin'.decode_repeated_user_field 5 Enum_descriptor_proto.of_binary _m >>= fun enum_type ->
      Bin'.decode_repeated_user_field 6 Service_descriptor_proto.of_binary _m >>= fun service ->
      Bin'.decode_repeated_user_field 7 Field_descriptor_proto.of_binary _m >>= fun extension ->
      Bin'.decode_user_field 8 File_options.of_binary _m >>= fun options ->
      Bin'.decode_user_field 9 Source_code_info.of_binary _m >>= fun source_code_info ->
      Bin'.decode_optional_field 12 Field'.String_t _m >>= fun syntax ->
      Ok { name; package; dependency; public_dependency; weak_dependency; message_type; enum_type; service; extension; options; source_code_info; syntax }

  let rec to_text =
    fun { name; package; dependency; public_dependency; weak_dependency; message_type; enum_type; service; extension; options; source_code_info; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_optional_field "package" Field'.String_t package _o >>= fun () ->
      Text'.serialize_repeated_field "dependency" Field'.String_t dependency _o >>= fun () ->
      Text'.serialize_repeated_field "public_dependency" Field'.Int32_t public_dependency _o >>= fun () ->
      Text'.serialize_repeated_field "weak_dependency" Field'.Int32_t weak_dependency _o >>= fun () ->
      Text'.serialize_repeated_user_field "message_type" Descriptor_proto.to_text message_type _o >>= fun () ->
      Text'.serialize_repeated_user_field "enum_type" Enum_descriptor_proto.to_text enum_type _o >>= fun () ->
      Text'.serialize_repeated_user_field "service" Service_descriptor_proto.to_text service _o >>= fun () ->
      Text'.serialize_repeated_user_field "extension" Field_descriptor_proto.to_text extension _o >>= fun () ->
      Text'.serialize_user_field "options" File_options.to_text options _o >>= fun () ->
      Text'.serialize_user_field "source_code_info" Source_code_info.to_text source_code_info _o >>= fun () ->
      Text'.serialize_optional_field "syntax" Field'.String_t syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_optional_field "package" Field'.String_t _m >>= fun package ->
      Text'.decode_repeated_field "dependency" Field'.String_t _m >>= fun dependency ->
      Text'.decode_repeated_field "public_dependency" Field'.Int32_t _m >>= fun public_dependency ->
      Text'.decode_repeated_field "weak_dependency" Field'.Int32_t _m >>= fun weak_dependency ->
      Text'.decode_repeated_user_field "message_type" Descriptor_proto.of_text _m >>= fun message_type ->
      Text'.decode_repeated_user_field "enum_type" Enum_descriptor_proto.of_text _m >>= fun enum_type ->
      Text'.decode_repeated_user_field "service" Service_descriptor_proto.of_text _m >>= fun service ->
      Text'.decode_repeated_user_field "extension" Field_descriptor_proto.of_text _m >>= fun extension ->
      Text'.decode_user_field "options" File_options.of_text _m >>= fun options ->
      Text'.decode_user_field "source_code_info" Source_code_info.of_text _m >>= fun source_code_info ->
      Text'.decode_optional_field "syntax" Field'.String_t _m >>= fun syntax ->
      Ok { name; package; dependency; public_dependency; weak_dependency; message_type; enum_type; service; extension; options; source_code_info; syntax }
end

and Descriptor_proto : sig
  module rec Extension_range : sig
    type t = {
      start : int option;
      end' : int option;
      options : Extension_range_options.t option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end
  
  and Reserved_range : sig
    type t = {
      start : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    name : string option;
    field : Field_descriptor_proto.t list;
    extension : Field_descriptor_proto.t list;
    nested_type : Descriptor_proto.t list;
    enum_type : Enum_descriptor_proto.t list;
    extension_range : Descriptor_proto.Extension_range.t list;
    oneof_decl : Oneof_descriptor_proto.t list;
    options : Message_options.t option;
    reserved_range : Descriptor_proto.Reserved_range.t list;
    reserved_name : string list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec Extension_range : sig
    type t = {
      start : int option;
      end' : int option;
      options : Extension_range_options.t option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      start : int option;
      end' : int option;
      options : Extension_range_options.t option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { start; end'; options } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_optional_field 1 Field'.Int32_t start _o >>= fun () ->
        Bin'.serialize_optional_field 2 Field'.Int32_t end' _o >>= fun () ->
        Bin'.serialize_user_field 3 Extension_range_options.to_binary options _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_optional_field 1 Field'.Int32_t _m >>= fun start ->
        Bin'.decode_optional_field 2 Field'.Int32_t _m >>= fun end' ->
        Bin'.decode_user_field 3 Extension_range_options.of_binary _m >>= fun options ->
        Ok { start; end'; options }
  
    let rec to_text =
      fun { start; end'; options } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_optional_field "start" Field'.Int32_t start _o >>= fun () ->
        Text'.serialize_optional_field "end" Field'.Int32_t end' _o >>= fun () ->
        Text'.serialize_user_field "options" Extension_range_options.to_text options _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_optional_field "start" Field'.Int32_t _m >>= fun start ->
        Text'.decode_optional_field "end" Field'.Int32_t _m >>= fun end' ->
        Text'.decode_user_field "options" Extension_range_options.of_text _m >>= fun options ->
        Ok { start; end'; options }
  end
  
  and Reserved_range : sig
    type t = {
      start : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      start : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { start; end' } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_optional_field 1 Field'.Int32_t start _o >>= fun () ->
        Bin'.serialize_optional_field 2 Field'.Int32_t end' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_optional_field 1 Field'.Int32_t _m >>= fun start ->
        Bin'.decode_optional_field 2 Field'.Int32_t _m >>= fun end' ->
        Ok { start; end' }
  
    let rec to_text =
      fun { start; end' } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_optional_field "start" Field'.Int32_t start _o >>= fun () ->
        Text'.serialize_optional_field "end" Field'.Int32_t end' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_optional_field "start" Field'.Int32_t _m >>= fun start ->
        Text'.decode_optional_field "end" Field'.Int32_t _m >>= fun end' ->
        Ok { start; end' }
  end

  type t = {
    name : string option;
    field : Field_descriptor_proto.t list;
    extension : Field_descriptor_proto.t list;
    nested_type : Descriptor_proto.t list;
    enum_type : Enum_descriptor_proto.t list;
    extension_range : Descriptor_proto.Extension_range.t list;
    oneof_decl : Oneof_descriptor_proto.t list;
    options : Message_options.t option;
    reserved_range : Descriptor_proto.Reserved_range.t list;
    reserved_name : string list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; field; extension; nested_type; enum_type; extension_range; oneof_decl; options; reserved_range; reserved_name } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_repeated_user_field 2 Field_descriptor_proto.to_binary field _o >>= fun () ->
      Bin'.serialize_repeated_user_field 6 Field_descriptor_proto.to_binary extension _o >>= fun () ->
      Bin'.serialize_repeated_user_field 3 Descriptor_proto.to_binary nested_type _o >>= fun () ->
      Bin'.serialize_repeated_user_field 4 Enum_descriptor_proto.to_binary enum_type _o >>= fun () ->
      Bin'.serialize_repeated_user_field 5 Descriptor_proto.Extension_range.to_binary extension_range _o >>= fun () ->
      Bin'.serialize_repeated_user_field 8 Oneof_descriptor_proto.to_binary oneof_decl _o >>= fun () ->
      Bin'.serialize_user_field 7 Message_options.to_binary options _o >>= fun () ->
      Bin'.serialize_repeated_user_field 9 Descriptor_proto.Reserved_range.to_binary reserved_range _o >>= fun () ->
      Bin'.serialize_repeated_field 10 Field'.String_t reserved_name _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_repeated_user_field 2 Field_descriptor_proto.of_binary _m >>= fun field ->
      Bin'.decode_repeated_user_field 6 Field_descriptor_proto.of_binary _m >>= fun extension ->
      Bin'.decode_repeated_user_field 3 Descriptor_proto.of_binary _m >>= fun nested_type ->
      Bin'.decode_repeated_user_field 4 Enum_descriptor_proto.of_binary _m >>= fun enum_type ->
      Bin'.decode_repeated_user_field 5 Descriptor_proto.Extension_range.of_binary _m >>= fun extension_range ->
      Bin'.decode_repeated_user_field 8 Oneof_descriptor_proto.of_binary _m >>= fun oneof_decl ->
      Bin'.decode_user_field 7 Message_options.of_binary _m >>= fun options ->
      Bin'.decode_repeated_user_field 9 Descriptor_proto.Reserved_range.of_binary _m >>= fun reserved_range ->
      Bin'.decode_repeated_field 10 Field'.String_t _m >>= fun reserved_name ->
      Ok { name; field; extension; nested_type; enum_type; extension_range; oneof_decl; options; reserved_range; reserved_name }

  let rec to_text =
    fun { name; field; extension; nested_type; enum_type; extension_range; oneof_decl; options; reserved_range; reserved_name } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_repeated_user_field "field" Field_descriptor_proto.to_text field _o >>= fun () ->
      Text'.serialize_repeated_user_field "extension" Field_descriptor_proto.to_text extension _o >>= fun () ->
      Text'.serialize_repeated_user_field "nested_type" Descriptor_proto.to_text nested_type _o >>= fun () ->
      Text'.serialize_repeated_user_field "enum_type" Enum_descriptor_proto.to_text enum_type _o >>= fun () ->
      Text'.serialize_repeated_user_field "extension_range" Descriptor_proto.Extension_range.to_text extension_range _o >>= fun () ->
      Text'.serialize_repeated_user_field "oneof_decl" Oneof_descriptor_proto.to_text oneof_decl _o >>= fun () ->
      Text'.serialize_user_field "options" Message_options.to_text options _o >>= fun () ->
      Text'.serialize_repeated_user_field "reserved_range" Descriptor_proto.Reserved_range.to_text reserved_range _o >>= fun () ->
      Text'.serialize_repeated_field "reserved_name" Field'.String_t reserved_name _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_repeated_user_field "field" Field_descriptor_proto.of_text _m >>= fun field ->
      Text'.decode_repeated_user_field "extension" Field_descriptor_proto.of_text _m >>= fun extension ->
      Text'.decode_repeated_user_field "nested_type" Descriptor_proto.of_text _m >>= fun nested_type ->
      Text'.decode_repeated_user_field "enum_type" Enum_descriptor_proto.of_text _m >>= fun enum_type ->
      Text'.decode_repeated_user_field "extension_range" Descriptor_proto.Extension_range.of_text _m >>= fun extension_range ->
      Text'.decode_repeated_user_field "oneof_decl" Oneof_descriptor_proto.of_text _m >>= fun oneof_decl ->
      Text'.decode_user_field "options" Message_options.of_text _m >>= fun options ->
      Text'.decode_repeated_user_field "reserved_range" Descriptor_proto.Reserved_range.of_text _m >>= fun reserved_range ->
      Text'.decode_repeated_field "reserved_name" Field'.String_t _m >>= fun reserved_name ->
      Ok { name; field; extension; nested_type; enum_type; extension_range; oneof_decl; options; reserved_range; reserved_name }
end

and Extension_range_options : sig
  type t = {
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { uninterpreted_option }

  let rec to_text =
    fun { uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { uninterpreted_option }
end

and Field_descriptor_proto : sig
  module Type' : sig
    type t =
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
  
  module Label : sig
    type t =
      | Label_optional
      | Label_required
      | Label_repeated
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end

  type t = {
    name : string option;
    number : int option;
    label : Field_descriptor_proto.Label.t;
    type' : Field_descriptor_proto.Type'.t;
    type_name : string option;
    extendee : string option;
    default_value : string option;
    oneof_index : int option;
    json_name : string option;
    options : Field_options.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module Type' : sig
    type t =
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
      fun () -> Type_double
  
    let to_int =
      function
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
  
  module Label : sig
    type t =
      | Label_optional
      | Label_required
      | Label_repeated
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | Label_optional
      | Label_required
      | Label_repeated
    [@@deriving eq, show]
  
    let default =
      fun () -> Label_optional
  
    let to_int =
      function
      | Label_optional -> 1
      | Label_required -> 2
      | Label_repeated -> 3
  
    let of_int =
      function
      | 1 -> Some Label_optional
      | 2 -> Some Label_required
      | 3 -> Some Label_repeated
      | _ -> None
  
    let to_string =
      function
      | Label_optional -> "LABEL_OPTIONAL"
      | Label_required -> "LABEL_REQUIRED"
      | Label_repeated -> "LABEL_REPEATED"
  
    let of_string =
      function
      | "LABEL_OPTIONAL" -> Some Label_optional
      | "LABEL_REQUIRED" -> Some Label_required
      | "LABEL_REPEATED" -> Some Label_repeated
      | _ -> None
  end

  type t = {
    name : string option;
    number : int option;
    label : Field_descriptor_proto.Label.t;
    type' : Field_descriptor_proto.Type'.t;
    type_name : string option;
    extendee : string option;
    default_value : string option;
    oneof_index : int option;
    json_name : string option;
    options : Field_options.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; number; label; type'; type_name; extendee; default_value; oneof_index; json_name; options } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.Int32_t number _o >>= fun () ->
      Bin'.serialize_enum_field 4 Field_descriptor_proto.Label.to_int label _o >>= fun () ->
      Bin'.serialize_enum_field 5 Field_descriptor_proto.Type'.to_int type' _o >>= fun () ->
      Bin'.serialize_optional_field 6 Field'.String_t type_name _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.String_t extendee _o >>= fun () ->
      Bin'.serialize_optional_field 7 Field'.String_t default_value _o >>= fun () ->
      Bin'.serialize_optional_field 9 Field'.Int32_t oneof_index _o >>= fun () ->
      Bin'.serialize_optional_field 10 Field'.String_t json_name _o >>= fun () ->
      Bin'.serialize_user_field 8 Field_options.to_binary options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_optional_field 3 Field'.Int32_t _m >>= fun number ->
      Bin'.decode_enum_field 4 Field_descriptor_proto.Label.of_int Field_descriptor_proto.Label.default _m >>= fun label ->
      Bin'.decode_enum_field 5 Field_descriptor_proto.Type'.of_int Field_descriptor_proto.Type'.default _m >>= fun type' ->
      Bin'.decode_optional_field 6 Field'.String_t _m >>= fun type_name ->
      Bin'.decode_optional_field 2 Field'.String_t _m >>= fun extendee ->
      Bin'.decode_optional_field 7 Field'.String_t _m >>= fun default_value ->
      Bin'.decode_optional_field 9 Field'.Int32_t _m >>= fun oneof_index ->
      Bin'.decode_optional_field 10 Field'.String_t _m >>= fun json_name ->
      Bin'.decode_user_field 8 Field_options.of_binary _m >>= fun options ->
      Ok { name; number; label; type'; type_name; extendee; default_value; oneof_index; json_name; options }

  let rec to_text =
    fun { name; number; label; type'; type_name; extendee; default_value; oneof_index; json_name; options } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_optional_field "number" Field'.Int32_t number _o >>= fun () ->
      Text'.serialize_enum_field "label" Field_descriptor_proto.Label.to_string label _o >>= fun () ->
      Text'.serialize_enum_field "type" Field_descriptor_proto.Type'.to_string type' _o >>= fun () ->
      Text'.serialize_optional_field "type_name" Field'.String_t type_name _o >>= fun () ->
      Text'.serialize_optional_field "extendee" Field'.String_t extendee _o >>= fun () ->
      Text'.serialize_optional_field "default_value" Field'.String_t default_value _o >>= fun () ->
      Text'.serialize_optional_field "oneof_index" Field'.Int32_t oneof_index _o >>= fun () ->
      Text'.serialize_optional_field "json_name" Field'.String_t json_name _o >>= fun () ->
      Text'.serialize_user_field "options" Field_options.to_text options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_optional_field "number" Field'.Int32_t _m >>= fun number ->
      Text'.decode_enum_field "label" Field_descriptor_proto.Label.of_string Field_descriptor_proto.Label.default _m >>= fun label ->
      Text'.decode_enum_field "type" Field_descriptor_proto.Type'.of_string Field_descriptor_proto.Type'.default _m >>= fun type' ->
      Text'.decode_optional_field "type_name" Field'.String_t _m >>= fun type_name ->
      Text'.decode_optional_field "extendee" Field'.String_t _m >>= fun extendee ->
      Text'.decode_optional_field "default_value" Field'.String_t _m >>= fun default_value ->
      Text'.decode_optional_field "oneof_index" Field'.Int32_t _m >>= fun oneof_index ->
      Text'.decode_optional_field "json_name" Field'.String_t _m >>= fun json_name ->
      Text'.decode_user_field "options" Field_options.of_text _m >>= fun options ->
      Ok { name; number; label; type'; type_name; extendee; default_value; oneof_index; json_name; options }
end

and Oneof_descriptor_proto : sig
  type t = {
    name : string option;
    options : Oneof_options.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string option;
    options : Oneof_options.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; options } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_user_field 2 Oneof_options.to_binary options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_user_field 2 Oneof_options.of_binary _m >>= fun options ->
      Ok { name; options }

  let rec to_text =
    fun { name; options } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_user_field "options" Oneof_options.to_text options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_user_field "options" Oneof_options.of_text _m >>= fun options ->
      Ok { name; options }
end

and Enum_descriptor_proto : sig
  module rec Enum_reserved_range : sig
    type t = {
      start : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    name : string option;
    value' : Enum_value_descriptor_proto.t list;
    options : Enum_options.t option;
    reserved_range : Enum_descriptor_proto.Enum_reserved_range.t list;
    reserved_name : string list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec Enum_reserved_range : sig
    type t = {
      start : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      start : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { start; end' } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_optional_field 1 Field'.Int32_t start _o >>= fun () ->
        Bin'.serialize_optional_field 2 Field'.Int32_t end' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_optional_field 1 Field'.Int32_t _m >>= fun start ->
        Bin'.decode_optional_field 2 Field'.Int32_t _m >>= fun end' ->
        Ok { start; end' }
  
    let rec to_text =
      fun { start; end' } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_optional_field "start" Field'.Int32_t start _o >>= fun () ->
        Text'.serialize_optional_field "end" Field'.Int32_t end' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_optional_field "start" Field'.Int32_t _m >>= fun start ->
        Text'.decode_optional_field "end" Field'.Int32_t _m >>= fun end' ->
        Ok { start; end' }
  end

  type t = {
    name : string option;
    value' : Enum_value_descriptor_proto.t list;
    options : Enum_options.t option;
    reserved_range : Enum_descriptor_proto.Enum_reserved_range.t list;
    reserved_name : string list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; value'; options; reserved_range; reserved_name } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_repeated_user_field 2 Enum_value_descriptor_proto.to_binary value' _o >>= fun () ->
      Bin'.serialize_user_field 3 Enum_options.to_binary options _o >>= fun () ->
      Bin'.serialize_repeated_user_field 4 Enum_descriptor_proto.Enum_reserved_range.to_binary reserved_range _o >>= fun () ->
      Bin'.serialize_repeated_field 5 Field'.String_t reserved_name _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_repeated_user_field 2 Enum_value_descriptor_proto.of_binary _m >>= fun value' ->
      Bin'.decode_user_field 3 Enum_options.of_binary _m >>= fun options ->
      Bin'.decode_repeated_user_field 4 Enum_descriptor_proto.Enum_reserved_range.of_binary _m >>= fun reserved_range ->
      Bin'.decode_repeated_field 5 Field'.String_t _m >>= fun reserved_name ->
      Ok { name; value'; options; reserved_range; reserved_name }

  let rec to_text =
    fun { name; value'; options; reserved_range; reserved_name } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_repeated_user_field "value" Enum_value_descriptor_proto.to_text value' _o >>= fun () ->
      Text'.serialize_user_field "options" Enum_options.to_text options _o >>= fun () ->
      Text'.serialize_repeated_user_field "reserved_range" Enum_descriptor_proto.Enum_reserved_range.to_text reserved_range _o >>= fun () ->
      Text'.serialize_repeated_field "reserved_name" Field'.String_t reserved_name _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_repeated_user_field "value" Enum_value_descriptor_proto.of_text _m >>= fun value' ->
      Text'.decode_user_field "options" Enum_options.of_text _m >>= fun options ->
      Text'.decode_repeated_user_field "reserved_range" Enum_descriptor_proto.Enum_reserved_range.of_text _m >>= fun reserved_range ->
      Text'.decode_repeated_field "reserved_name" Field'.String_t _m >>= fun reserved_name ->
      Ok { name; value'; options; reserved_range; reserved_name }
end

and Enum_value_descriptor_proto : sig
  type t = {
    name : string option;
    number : int option;
    options : Enum_value_options.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string option;
    number : int option;
    options : Enum_value_options.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; number; options } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.Int32_t number _o >>= fun () ->
      Bin'.serialize_user_field 3 Enum_value_options.to_binary options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_optional_field 2 Field'.Int32_t _m >>= fun number ->
      Bin'.decode_user_field 3 Enum_value_options.of_binary _m >>= fun options ->
      Ok { name; number; options }

  let rec to_text =
    fun { name; number; options } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_optional_field "number" Field'.Int32_t number _o >>= fun () ->
      Text'.serialize_user_field "options" Enum_value_options.to_text options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_optional_field "number" Field'.Int32_t _m >>= fun number ->
      Text'.decode_user_field "options" Enum_value_options.of_text _m >>= fun options ->
      Ok { name; number; options }
end

and Service_descriptor_proto : sig
  type t = {
    name : string option;
    method' : Method_descriptor_proto.t list;
    options : Service_options.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string option;
    method' : Method_descriptor_proto.t list;
    options : Service_options.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; method'; options } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_repeated_user_field 2 Method_descriptor_proto.to_binary method' _o >>= fun () ->
      Bin'.serialize_user_field 3 Service_options.to_binary options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_repeated_user_field 2 Method_descriptor_proto.of_binary _m >>= fun method' ->
      Bin'.decode_user_field 3 Service_options.of_binary _m >>= fun options ->
      Ok { name; method'; options }

  let rec to_text =
    fun { name; method'; options } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_repeated_user_field "method" Method_descriptor_proto.to_text method' _o >>= fun () ->
      Text'.serialize_user_field "options" Service_options.to_text options _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_repeated_user_field "method" Method_descriptor_proto.of_text _m >>= fun method' ->
      Text'.decode_user_field "options" Service_options.of_text _m >>= fun options ->
      Ok { name; method'; options }
end

and Method_descriptor_proto : sig
  type t = {
    name : string option;
    input_type : string option;
    output_type : string option;
    options : Method_options.t option;
    client_streaming : bool option;
    server_streaming : bool option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string option;
    input_type : string option;
    output_type : string option;
    options : Method_options.t option;
    client_streaming : bool option;
    server_streaming : bool option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; input_type; output_type; options; client_streaming; server_streaming } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.String_t input_type _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.String_t output_type _o >>= fun () ->
      Bin'.serialize_user_field 4 Method_options.to_binary options _o >>= fun () ->
      Bin'.serialize_optional_field 5 Field'.Bool_t client_streaming _o >>= fun () ->
      Bin'.serialize_optional_field 6 Field'.Bool_t server_streaming _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_optional_field 2 Field'.String_t _m >>= fun input_type ->
      Bin'.decode_optional_field 3 Field'.String_t _m >>= fun output_type ->
      Bin'.decode_user_field 4 Method_options.of_binary _m >>= fun options ->
      Bin'.decode_optional_field 5 Field'.Bool_t _m >>= fun client_streaming ->
      Bin'.decode_optional_field 6 Field'.Bool_t _m >>= fun server_streaming ->
      Ok { name; input_type; output_type; options; client_streaming; server_streaming }

  let rec to_text =
    fun { name; input_type; output_type; options; client_streaming; server_streaming } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_optional_field "input_type" Field'.String_t input_type _o >>= fun () ->
      Text'.serialize_optional_field "output_type" Field'.String_t output_type _o >>= fun () ->
      Text'.serialize_user_field "options" Method_options.to_text options _o >>= fun () ->
      Text'.serialize_optional_field "client_streaming" Field'.Bool_t client_streaming _o >>= fun () ->
      Text'.serialize_optional_field "server_streaming" Field'.Bool_t server_streaming _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_optional_field "input_type" Field'.String_t _m >>= fun input_type ->
      Text'.decode_optional_field "output_type" Field'.String_t _m >>= fun output_type ->
      Text'.decode_user_field "options" Method_options.of_text _m >>= fun options ->
      Text'.decode_optional_field "client_streaming" Field'.Bool_t _m >>= fun client_streaming ->
      Text'.decode_optional_field "server_streaming" Field'.Bool_t _m >>= fun server_streaming ->
      Ok { name; input_type; output_type; options; client_streaming; server_streaming }
end

and File_options : sig
  module Optimize_mode : sig
    type t =
      | Speed
      | Code_size
      | Lite_runtime
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end

  type t = {
    java_package : string option;
    java_outer_classname : string option;
    java_multiple_files : bool option;
    java_generate_equals_and_hash : bool option;
    java_string_check_utf8 : bool option;
    optimize_for : File_options.Optimize_mode.t;
    go_package : string option;
    cc_generic_services : bool option;
    java_generic_services : bool option;
    py_generic_services : bool option;
    php_generic_services : bool option;
    deprecated : bool option;
    cc_enable_arenas : bool option;
    objc_class_prefix : string option;
    csharp_namespace : string option;
    swift_prefix : string option;
    php_class_prefix : string option;
    php_namespace : string option;
    php_metadata_namespace : string option;
    ruby_package : string option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module Optimize_mode : sig
    type t =
      | Speed
      | Code_size
      | Lite_runtime
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | Speed
      | Code_size
      | Lite_runtime
    [@@deriving eq, show]
  
    let default =
      fun () -> Speed
  
    let to_int =
      function
      | Speed -> 1
      | Code_size -> 2
      | Lite_runtime -> 3
  
    let of_int =
      function
      | 1 -> Some Speed
      | 2 -> Some Code_size
      | 3 -> Some Lite_runtime
      | _ -> None
  
    let to_string =
      function
      | Speed -> "SPEED"
      | Code_size -> "CODE_SIZE"
      | Lite_runtime -> "LITE_RUNTIME"
  
    let of_string =
      function
      | "SPEED" -> Some Speed
      | "CODE_SIZE" -> Some Code_size
      | "LITE_RUNTIME" -> Some Lite_runtime
      | _ -> None
  end

  type t = {
    java_package : string option;
    java_outer_classname : string option;
    java_multiple_files : bool option;
    java_generate_equals_and_hash : bool option;
    java_string_check_utf8 : bool option;
    optimize_for : File_options.Optimize_mode.t;
    go_package : string option;
    cc_generic_services : bool option;
    java_generic_services : bool option;
    py_generic_services : bool option;
    php_generic_services : bool option;
    deprecated : bool option;
    cc_enable_arenas : bool option;
    objc_class_prefix : string option;
    csharp_namespace : string option;
    swift_prefix : string option;
    php_class_prefix : string option;
    php_namespace : string option;
    php_metadata_namespace : string option;
    ruby_package : string option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { java_package; java_outer_classname; java_multiple_files; java_generate_equals_and_hash; java_string_check_utf8; optimize_for; go_package; cc_generic_services; java_generic_services; py_generic_services; php_generic_services; deprecated; cc_enable_arenas; objc_class_prefix; csharp_namespace; swift_prefix; php_class_prefix; php_namespace; php_metadata_namespace; ruby_package; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t java_package _o >>= fun () ->
      Bin'.serialize_optional_field 8 Field'.String_t java_outer_classname _o >>= fun () ->
      Bin'.serialize_optional_field 10 Field'.Bool_t java_multiple_files _o >>= fun () ->
      Bin'.serialize_optional_field 20 Field'.Bool_t java_generate_equals_and_hash _o >>= fun () ->
      Bin'.serialize_optional_field 27 Field'.Bool_t java_string_check_utf8 _o >>= fun () ->
      Bin'.serialize_enum_field 9 File_options.Optimize_mode.to_int optimize_for _o >>= fun () ->
      Bin'.serialize_optional_field 11 Field'.String_t go_package _o >>= fun () ->
      Bin'.serialize_optional_field 16 Field'.Bool_t cc_generic_services _o >>= fun () ->
      Bin'.serialize_optional_field 17 Field'.Bool_t java_generic_services _o >>= fun () ->
      Bin'.serialize_optional_field 18 Field'.Bool_t py_generic_services _o >>= fun () ->
      Bin'.serialize_optional_field 42 Field'.Bool_t php_generic_services _o >>= fun () ->
      Bin'.serialize_optional_field 23 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_optional_field 31 Field'.Bool_t cc_enable_arenas _o >>= fun () ->
      Bin'.serialize_optional_field 36 Field'.String_t objc_class_prefix _o >>= fun () ->
      Bin'.serialize_optional_field 37 Field'.String_t csharp_namespace _o >>= fun () ->
      Bin'.serialize_optional_field 39 Field'.String_t swift_prefix _o >>= fun () ->
      Bin'.serialize_optional_field 40 Field'.String_t php_class_prefix _o >>= fun () ->
      Bin'.serialize_optional_field 41 Field'.String_t php_namespace _o >>= fun () ->
      Bin'.serialize_optional_field 44 Field'.String_t php_metadata_namespace _o >>= fun () ->
      Bin'.serialize_optional_field 45 Field'.String_t ruby_package _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun java_package ->
      Bin'.decode_optional_field 8 Field'.String_t _m >>= fun java_outer_classname ->
      Bin'.decode_optional_field 10 Field'.Bool_t _m >>= fun java_multiple_files ->
      Bin'.decode_optional_field 20 Field'.Bool_t _m >>= fun java_generate_equals_and_hash ->
      Bin'.decode_optional_field 27 Field'.Bool_t _m >>= fun java_string_check_utf8 ->
      Bin'.decode_enum_field 9 File_options.Optimize_mode.of_int File_options.Optimize_mode.default _m >>= fun optimize_for ->
      Bin'.decode_optional_field 11 Field'.String_t _m >>= fun go_package ->
      Bin'.decode_optional_field 16 Field'.Bool_t _m >>= fun cc_generic_services ->
      Bin'.decode_optional_field 17 Field'.Bool_t _m >>= fun java_generic_services ->
      Bin'.decode_optional_field 18 Field'.Bool_t _m >>= fun py_generic_services ->
      Bin'.decode_optional_field 42 Field'.Bool_t _m >>= fun php_generic_services ->
      Bin'.decode_optional_field 23 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_optional_field 31 Field'.Bool_t _m >>= fun cc_enable_arenas ->
      Bin'.decode_optional_field 36 Field'.String_t _m >>= fun objc_class_prefix ->
      Bin'.decode_optional_field 37 Field'.String_t _m >>= fun csharp_namespace ->
      Bin'.decode_optional_field 39 Field'.String_t _m >>= fun swift_prefix ->
      Bin'.decode_optional_field 40 Field'.String_t _m >>= fun php_class_prefix ->
      Bin'.decode_optional_field 41 Field'.String_t _m >>= fun php_namespace ->
      Bin'.decode_optional_field 44 Field'.String_t _m >>= fun php_metadata_namespace ->
      Bin'.decode_optional_field 45 Field'.String_t _m >>= fun ruby_package ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { java_package; java_outer_classname; java_multiple_files; java_generate_equals_and_hash; java_string_check_utf8; optimize_for; go_package; cc_generic_services; java_generic_services; py_generic_services; php_generic_services; deprecated; cc_enable_arenas; objc_class_prefix; csharp_namespace; swift_prefix; php_class_prefix; php_namespace; php_metadata_namespace; ruby_package; uninterpreted_option }

  let rec to_text =
    fun { java_package; java_outer_classname; java_multiple_files; java_generate_equals_and_hash; java_string_check_utf8; optimize_for; go_package; cc_generic_services; java_generic_services; py_generic_services; php_generic_services; deprecated; cc_enable_arenas; objc_class_prefix; csharp_namespace; swift_prefix; php_class_prefix; php_namespace; php_metadata_namespace; ruby_package; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "java_package" Field'.String_t java_package _o >>= fun () ->
      Text'.serialize_optional_field "java_outer_classname" Field'.String_t java_outer_classname _o >>= fun () ->
      Text'.serialize_optional_field "java_multiple_files" Field'.Bool_t java_multiple_files _o >>= fun () ->
      Text'.serialize_optional_field "java_generate_equals_and_hash" Field'.Bool_t java_generate_equals_and_hash _o >>= fun () ->
      Text'.serialize_optional_field "java_string_check_utf8" Field'.Bool_t java_string_check_utf8 _o >>= fun () ->
      Text'.serialize_enum_field "optimize_for" File_options.Optimize_mode.to_string optimize_for _o >>= fun () ->
      Text'.serialize_optional_field "go_package" Field'.String_t go_package _o >>= fun () ->
      Text'.serialize_optional_field "cc_generic_services" Field'.Bool_t cc_generic_services _o >>= fun () ->
      Text'.serialize_optional_field "java_generic_services" Field'.Bool_t java_generic_services _o >>= fun () ->
      Text'.serialize_optional_field "py_generic_services" Field'.Bool_t py_generic_services _o >>= fun () ->
      Text'.serialize_optional_field "php_generic_services" Field'.Bool_t php_generic_services _o >>= fun () ->
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_optional_field "cc_enable_arenas" Field'.Bool_t cc_enable_arenas _o >>= fun () ->
      Text'.serialize_optional_field "objc_class_prefix" Field'.String_t objc_class_prefix _o >>= fun () ->
      Text'.serialize_optional_field "csharp_namespace" Field'.String_t csharp_namespace _o >>= fun () ->
      Text'.serialize_optional_field "swift_prefix" Field'.String_t swift_prefix _o >>= fun () ->
      Text'.serialize_optional_field "php_class_prefix" Field'.String_t php_class_prefix _o >>= fun () ->
      Text'.serialize_optional_field "php_namespace" Field'.String_t php_namespace _o >>= fun () ->
      Text'.serialize_optional_field "php_metadata_namespace" Field'.String_t php_metadata_namespace _o >>= fun () ->
      Text'.serialize_optional_field "ruby_package" Field'.String_t ruby_package _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "java_package" Field'.String_t _m >>= fun java_package ->
      Text'.decode_optional_field "java_outer_classname" Field'.String_t _m >>= fun java_outer_classname ->
      Text'.decode_optional_field "java_multiple_files" Field'.Bool_t _m >>= fun java_multiple_files ->
      Text'.decode_optional_field "java_generate_equals_and_hash" Field'.Bool_t _m >>= fun java_generate_equals_and_hash ->
      Text'.decode_optional_field "java_string_check_utf8" Field'.Bool_t _m >>= fun java_string_check_utf8 ->
      Text'.decode_enum_field "optimize_for" File_options.Optimize_mode.of_string File_options.Optimize_mode.default _m >>= fun optimize_for ->
      Text'.decode_optional_field "go_package" Field'.String_t _m >>= fun go_package ->
      Text'.decode_optional_field "cc_generic_services" Field'.Bool_t _m >>= fun cc_generic_services ->
      Text'.decode_optional_field "java_generic_services" Field'.Bool_t _m >>= fun java_generic_services ->
      Text'.decode_optional_field "py_generic_services" Field'.Bool_t _m >>= fun py_generic_services ->
      Text'.decode_optional_field "php_generic_services" Field'.Bool_t _m >>= fun php_generic_services ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_optional_field "cc_enable_arenas" Field'.Bool_t _m >>= fun cc_enable_arenas ->
      Text'.decode_optional_field "objc_class_prefix" Field'.String_t _m >>= fun objc_class_prefix ->
      Text'.decode_optional_field "csharp_namespace" Field'.String_t _m >>= fun csharp_namespace ->
      Text'.decode_optional_field "swift_prefix" Field'.String_t _m >>= fun swift_prefix ->
      Text'.decode_optional_field "php_class_prefix" Field'.String_t _m >>= fun php_class_prefix ->
      Text'.decode_optional_field "php_namespace" Field'.String_t _m >>= fun php_namespace ->
      Text'.decode_optional_field "php_metadata_namespace" Field'.String_t _m >>= fun php_metadata_namespace ->
      Text'.decode_optional_field "ruby_package" Field'.String_t _m >>= fun ruby_package ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { java_package; java_outer_classname; java_multiple_files; java_generate_equals_and_hash; java_string_check_utf8; optimize_for; go_package; cc_generic_services; java_generic_services; py_generic_services; php_generic_services; deprecated; cc_enable_arenas; objc_class_prefix; csharp_namespace; swift_prefix; php_class_prefix; php_namespace; php_metadata_namespace; ruby_package; uninterpreted_option }
end

and Message_options : sig
  type t = {
    message_set_wire_format : bool option;
    no_standard_descriptor_accessor : bool option;
    deprecated : bool option;
    map_entry : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    message_set_wire_format : bool option;
    no_standard_descriptor_accessor : bool option;
    deprecated : bool option;
    map_entry : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { message_set_wire_format; no_standard_descriptor_accessor; deprecated; map_entry; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.Bool_t message_set_wire_format _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.Bool_t no_standard_descriptor_accessor _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_optional_field 7 Field'.Bool_t map_entry _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.Bool_t _m >>= fun message_set_wire_format ->
      Bin'.decode_optional_field 2 Field'.Bool_t _m >>= fun no_standard_descriptor_accessor ->
      Bin'.decode_optional_field 3 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_optional_field 7 Field'.Bool_t _m >>= fun map_entry ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { message_set_wire_format; no_standard_descriptor_accessor; deprecated; map_entry; uninterpreted_option }

  let rec to_text =
    fun { message_set_wire_format; no_standard_descriptor_accessor; deprecated; map_entry; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "message_set_wire_format" Field'.Bool_t message_set_wire_format _o >>= fun () ->
      Text'.serialize_optional_field "no_standard_descriptor_accessor" Field'.Bool_t no_standard_descriptor_accessor _o >>= fun () ->
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_optional_field "map_entry" Field'.Bool_t map_entry _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "message_set_wire_format" Field'.Bool_t _m >>= fun message_set_wire_format ->
      Text'.decode_optional_field "no_standard_descriptor_accessor" Field'.Bool_t _m >>= fun no_standard_descriptor_accessor ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_optional_field "map_entry" Field'.Bool_t _m >>= fun map_entry ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { message_set_wire_format; no_standard_descriptor_accessor; deprecated; map_entry; uninterpreted_option }
end

and Field_options : sig
  module C_type : sig
    type t =
      | String
      | Cord
      | String_piece
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end
  
  module J_s_type : sig
    type t =
      | Js_normal
      | Js_string
      | Js_number
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end

  type t = {
    ctype : Field_options.C_type.t;
    packed : bool option;
    jstype : Field_options.J_s_type.t;
    lazy' : bool option;
    deprecated : bool option;
    weak : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module C_type : sig
    type t =
      | String
      | Cord
      | String_piece
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | String
      | Cord
      | String_piece
    [@@deriving eq, show]
  
    let default =
      fun () -> String
  
    let to_int =
      function
      | String -> 0
      | Cord -> 1
      | String_piece -> 2
  
    let of_int =
      function
      | 0 -> Some String
      | 1 -> Some Cord
      | 2 -> Some String_piece
      | _ -> None
  
    let to_string =
      function
      | String -> "STRING"
      | Cord -> "CORD"
      | String_piece -> "STRING_PIECE"
  
    let of_string =
      function
      | "STRING" -> Some String
      | "CORD" -> Some Cord
      | "STRING_PIECE" -> Some String_piece
      | _ -> None
  end
  
  module J_s_type : sig
    type t =
      | Js_normal
      | Js_string
      | Js_number
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | Js_normal
      | Js_string
      | Js_number
    [@@deriving eq, show]
  
    let default =
      fun () -> Js_normal
  
    let to_int =
      function
      | Js_normal -> 0
      | Js_string -> 1
      | Js_number -> 2
  
    let of_int =
      function
      | 0 -> Some Js_normal
      | 1 -> Some Js_string
      | 2 -> Some Js_number
      | _ -> None
  
    let to_string =
      function
      | Js_normal -> "JS_NORMAL"
      | Js_string -> "JS_STRING"
      | Js_number -> "JS_NUMBER"
  
    let of_string =
      function
      | "JS_NORMAL" -> Some Js_normal
      | "JS_STRING" -> Some Js_string
      | "JS_NUMBER" -> Some Js_number
      | _ -> None
  end

  type t = {
    ctype : Field_options.C_type.t;
    packed : bool option;
    jstype : Field_options.J_s_type.t;
    lazy' : bool option;
    deprecated : bool option;
    weak : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { ctype; packed; jstype; lazy'; deprecated; weak; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_enum_field 1 Field_options.C_type.to_int ctype _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.Bool_t packed _o >>= fun () ->
      Bin'.serialize_enum_field 6 Field_options.J_s_type.to_int jstype _o >>= fun () ->
      Bin'.serialize_optional_field 5 Field'.Bool_t lazy' _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_optional_field 10 Field'.Bool_t weak _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_enum_field 1 Field_options.C_type.of_int Field_options.C_type.default _m >>= fun ctype ->
      Bin'.decode_optional_field 2 Field'.Bool_t _m >>= fun packed ->
      Bin'.decode_enum_field 6 Field_options.J_s_type.of_int Field_options.J_s_type.default _m >>= fun jstype ->
      Bin'.decode_optional_field 5 Field'.Bool_t _m >>= fun lazy' ->
      Bin'.decode_optional_field 3 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_optional_field 10 Field'.Bool_t _m >>= fun weak ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { ctype; packed; jstype; lazy'; deprecated; weak; uninterpreted_option }

  let rec to_text =
    fun { ctype; packed; jstype; lazy'; deprecated; weak; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_enum_field "ctype" Field_options.C_type.to_string ctype _o >>= fun () ->
      Text'.serialize_optional_field "packed" Field'.Bool_t packed _o >>= fun () ->
      Text'.serialize_enum_field "jstype" Field_options.J_s_type.to_string jstype _o >>= fun () ->
      Text'.serialize_optional_field "lazy" Field'.Bool_t lazy' _o >>= fun () ->
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_optional_field "weak" Field'.Bool_t weak _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_enum_field "ctype" Field_options.C_type.of_string Field_options.C_type.default _m >>= fun ctype ->
      Text'.decode_optional_field "packed" Field'.Bool_t _m >>= fun packed ->
      Text'.decode_enum_field "jstype" Field_options.J_s_type.of_string Field_options.J_s_type.default _m >>= fun jstype ->
      Text'.decode_optional_field "lazy" Field'.Bool_t _m >>= fun lazy' ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_optional_field "weak" Field'.Bool_t _m >>= fun weak ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { ctype; packed; jstype; lazy'; deprecated; weak; uninterpreted_option }
end

and Oneof_options : sig
  type t = {
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { uninterpreted_option }

  let rec to_text =
    fun { uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { uninterpreted_option }
end

and Enum_options : sig
  type t = {
    allow_alias : bool option;
    deprecated : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    allow_alias : bool option;
    deprecated : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { allow_alias; deprecated; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 2 Field'.Bool_t allow_alias _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 2 Field'.Bool_t _m >>= fun allow_alias ->
      Bin'.decode_optional_field 3 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { allow_alias; deprecated; uninterpreted_option }

  let rec to_text =
    fun { allow_alias; deprecated; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "allow_alias" Field'.Bool_t allow_alias _o >>= fun () ->
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "allow_alias" Field'.Bool_t _m >>= fun allow_alias ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { allow_alias; deprecated; uninterpreted_option }
end

and Enum_value_options : sig
  type t = {
    deprecated : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    deprecated : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { deprecated; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { deprecated; uninterpreted_option }

  let rec to_text =
    fun { deprecated; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { deprecated; uninterpreted_option }
end

and Service_options : sig
  type t = {
    deprecated : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    deprecated : bool option;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { deprecated; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 33 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 33 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { deprecated; uninterpreted_option }

  let rec to_text =
    fun { deprecated; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { deprecated; uninterpreted_option }
end

and Method_options : sig
  module Idempotency_level : sig
    type t =
      | Idempotency_unknown
      | No_side_effects
      | Idempotent
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end

  type t = {
    deprecated : bool option;
    idempotency_level : Method_options.Idempotency_level.t;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module Idempotency_level : sig
    type t =
      | Idempotency_unknown
      | No_side_effects
      | Idempotent
    [@@deriving eq, show]
  
    val default : unit -> t
  
    val to_int : t -> int
  
    val of_int : int -> t option
  
    val to_string : t -> string
  
    val of_string : string -> t option
  end = struct
    type t =
      | Idempotency_unknown
      | No_side_effects
      | Idempotent
    [@@deriving eq, show]
  
    let default =
      fun () -> Idempotency_unknown
  
    let to_int =
      function
      | Idempotency_unknown -> 0
      | No_side_effects -> 1
      | Idempotent -> 2
  
    let of_int =
      function
      | 0 -> Some Idempotency_unknown
      | 1 -> Some No_side_effects
      | 2 -> Some Idempotent
      | _ -> None
  
    let to_string =
      function
      | Idempotency_unknown -> "IDEMPOTENCY_UNKNOWN"
      | No_side_effects -> "NO_SIDE_EFFECTS"
      | Idempotent -> "IDEMPOTENT"
  
    let of_string =
      function
      | "IDEMPOTENCY_UNKNOWN" -> Some Idempotency_unknown
      | "NO_SIDE_EFFECTS" -> Some No_side_effects
      | "IDEMPOTENT" -> Some Idempotent
      | _ -> None
  end

  type t = {
    deprecated : bool option;
    idempotency_level : Method_options.Idempotency_level.t;
    uninterpreted_option : Uninterpreted_option.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { deprecated; idempotency_level; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 33 Field'.Bool_t deprecated _o >>= fun () ->
      Bin'.serialize_enum_field 34 Method_options.Idempotency_level.to_int idempotency_level _o >>= fun () ->
      Bin'.serialize_repeated_user_field 999 Uninterpreted_option.to_binary uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 33 Field'.Bool_t _m >>= fun deprecated ->
      Bin'.decode_enum_field 34 Method_options.Idempotency_level.of_int Method_options.Idempotency_level.default _m >>= fun idempotency_level ->
      Bin'.decode_repeated_user_field 999 Uninterpreted_option.of_binary _m >>= fun uninterpreted_option ->
      Ok { deprecated; idempotency_level; uninterpreted_option }

  let rec to_text =
    fun { deprecated; idempotency_level; uninterpreted_option } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "deprecated" Field'.Bool_t deprecated _o >>= fun () ->
      Text'.serialize_enum_field "idempotency_level" Method_options.Idempotency_level.to_string idempotency_level _o >>= fun () ->
      Text'.serialize_repeated_user_field "uninterpreted_option" Uninterpreted_option.to_text uninterpreted_option _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "deprecated" Field'.Bool_t _m >>= fun deprecated ->
      Text'.decode_enum_field "idempotency_level" Method_options.Idempotency_level.of_string Method_options.Idempotency_level.default _m >>= fun idempotency_level ->
      Text'.decode_repeated_user_field "uninterpreted_option" Uninterpreted_option.of_text _m >>= fun uninterpreted_option ->
      Ok { deprecated; idempotency_level; uninterpreted_option }
end

and Uninterpreted_option : sig
  module rec Name_part : sig
    type t = {
      name_part : string option;
      is_extension : bool option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    name : Uninterpreted_option.Name_part.t list;
    identifier_value : string option;
    positive_int_value : int option;
    negative_int_value : int option;
    double_value : float option;
    string_value : string option;
    aggregate_value : string option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec Name_part : sig
    type t = {
      name_part : string option;
      is_extension : bool option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      name_part : string option;
      is_extension : bool option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { name_part; is_extension } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_optional_field 1 Field'.String_t name_part _o >>= fun () ->
        Bin'.serialize_optional_field 2 Field'.Bool_t is_extension _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name_part ->
        Bin'.decode_optional_field 2 Field'.Bool_t _m >>= fun is_extension ->
        Ok { name_part; is_extension }
  
    let rec to_text =
      fun { name_part; is_extension } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_optional_field "name_part" Field'.String_t name_part _o >>= fun () ->
        Text'.serialize_optional_field "is_extension" Field'.Bool_t is_extension _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_optional_field "name_part" Field'.String_t _m >>= fun name_part ->
        Text'.decode_optional_field "is_extension" Field'.Bool_t _m >>= fun is_extension ->
        Ok { name_part; is_extension }
  end

  type t = {
    name : Uninterpreted_option.Name_part.t list;
    identifier_value : string option;
    positive_int_value : int option;
    negative_int_value : int option;
    double_value : float option;
    string_value : string option;
    aggregate_value : string option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; identifier_value; positive_int_value; negative_int_value; double_value; string_value; aggregate_value } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 2 Uninterpreted_option.Name_part.to_binary name _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.String_t identifier_value _o >>= fun () ->
      Bin'.serialize_optional_field 4 Field'.Uint64_t positive_int_value _o >>= fun () ->
      Bin'.serialize_optional_field 5 Field'.Int64_t negative_int_value _o >>= fun () ->
      Bin'.serialize_optional_field 6 Field'.Double_t double_value _o >>= fun () ->
      Bin'.serialize_optional_field 7 Field'.Bytes_t string_value _o >>= fun () ->
      Bin'.serialize_optional_field 8 Field'.String_t aggregate_value _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 2 Uninterpreted_option.Name_part.of_binary _m >>= fun name ->
      Bin'.decode_optional_field 3 Field'.String_t _m >>= fun identifier_value ->
      Bin'.decode_optional_field 4 Field'.Uint64_t _m >>= fun positive_int_value ->
      Bin'.decode_optional_field 5 Field'.Int64_t _m >>= fun negative_int_value ->
      Bin'.decode_optional_field 6 Field'.Double_t _m >>= fun double_value ->
      Bin'.decode_optional_field 7 Field'.Bytes_t _m >>= fun string_value ->
      Bin'.decode_optional_field 8 Field'.String_t _m >>= fun aggregate_value ->
      Ok { name; identifier_value; positive_int_value; negative_int_value; double_value; string_value; aggregate_value }

  let rec to_text =
    fun { name; identifier_value; positive_int_value; negative_int_value; double_value; string_value; aggregate_value } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "name" Uninterpreted_option.Name_part.to_text name _o >>= fun () ->
      Text'.serialize_optional_field "identifier_value" Field'.String_t identifier_value _o >>= fun () ->
      Text'.serialize_optional_field "positive_int_value" Field'.Uint64_t positive_int_value _o >>= fun () ->
      Text'.serialize_optional_field "negative_int_value" Field'.Int64_t negative_int_value _o >>= fun () ->
      Text'.serialize_optional_field "double_value" Field'.Double_t double_value _o >>= fun () ->
      Text'.serialize_optional_field "string_value" Field'.Bytes_t string_value _o >>= fun () ->
      Text'.serialize_optional_field "aggregate_value" Field'.String_t aggregate_value _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "name" Uninterpreted_option.Name_part.of_text _m >>= fun name ->
      Text'.decode_optional_field "identifier_value" Field'.String_t _m >>= fun identifier_value ->
      Text'.decode_optional_field "positive_int_value" Field'.Uint64_t _m >>= fun positive_int_value ->
      Text'.decode_optional_field "negative_int_value" Field'.Int64_t _m >>= fun negative_int_value ->
      Text'.decode_optional_field "double_value" Field'.Double_t _m >>= fun double_value ->
      Text'.decode_optional_field "string_value" Field'.Bytes_t _m >>= fun string_value ->
      Text'.decode_optional_field "aggregate_value" Field'.String_t _m >>= fun aggregate_value ->
      Ok { name; identifier_value; positive_int_value; negative_int_value; double_value; string_value; aggregate_value }
end

and Source_code_info : sig
  module rec Location : sig
    type t = {
      path : int list;
      span : int list;
      leading_comments : string option;
      trailing_comments : string option;
      leading_detached_comments : string list;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    location : Source_code_info.Location.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec Location : sig
    type t = {
      path : int list;
      span : int list;
      leading_comments : string option;
      trailing_comments : string option;
      leading_detached_comments : string list;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      path : int list;
      span : int list;
      leading_comments : string option;
      trailing_comments : string option;
      leading_detached_comments : string list;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { path; span; leading_comments; trailing_comments; leading_detached_comments } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_repeated_field 1 Field'.Int32_t path _o >>= fun () ->
        Bin'.serialize_repeated_field 2 Field'.Int32_t span _o >>= fun () ->
        Bin'.serialize_optional_field 3 Field'.String_t leading_comments _o >>= fun () ->
        Bin'.serialize_optional_field 4 Field'.String_t trailing_comments _o >>= fun () ->
        Bin'.serialize_repeated_field 6 Field'.String_t leading_detached_comments _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_repeated_field 1 Field'.Int32_t _m >>= fun path ->
        Bin'.decode_repeated_field 2 Field'.Int32_t _m >>= fun span ->
        Bin'.decode_optional_field 3 Field'.String_t _m >>= fun leading_comments ->
        Bin'.decode_optional_field 4 Field'.String_t _m >>= fun trailing_comments ->
        Bin'.decode_repeated_field 6 Field'.String_t _m >>= fun leading_detached_comments ->
        Ok { path; span; leading_comments; trailing_comments; leading_detached_comments }
  
    let rec to_text =
      fun { path; span; leading_comments; trailing_comments; leading_detached_comments } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_repeated_field "path" Field'.Int32_t path _o >>= fun () ->
        Text'.serialize_repeated_field "span" Field'.Int32_t span _o >>= fun () ->
        Text'.serialize_optional_field "leading_comments" Field'.String_t leading_comments _o >>= fun () ->
        Text'.serialize_optional_field "trailing_comments" Field'.String_t trailing_comments _o >>= fun () ->
        Text'.serialize_repeated_field "leading_detached_comments" Field'.String_t leading_detached_comments _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_repeated_field "path" Field'.Int32_t _m >>= fun path ->
        Text'.decode_repeated_field "span" Field'.Int32_t _m >>= fun span ->
        Text'.decode_optional_field "leading_comments" Field'.String_t _m >>= fun leading_comments ->
        Text'.decode_optional_field "trailing_comments" Field'.String_t _m >>= fun trailing_comments ->
        Text'.decode_repeated_field "leading_detached_comments" Field'.String_t _m >>= fun leading_detached_comments ->
        Ok { path; span; leading_comments; trailing_comments; leading_detached_comments }
  end

  type t = {
    location : Source_code_info.Location.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { location } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 1 Source_code_info.Location.to_binary location _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 1 Source_code_info.Location.of_binary _m >>= fun location ->
      Ok { location }

  let rec to_text =
    fun { location } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "location" Source_code_info.Location.to_text location _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "location" Source_code_info.Location.of_text _m >>= fun location ->
      Ok { location }
end

and Generated_code_info : sig
  module rec Annotation : sig
    type t = {
      path : int list;
      source_file : string option;
      begin' : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    annotation : Generated_code_info.Annotation.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec Annotation : sig
    type t = {
      path : int list;
      source_file : string option;
      begin' : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      path : int list;
      source_file : string option;
      begin' : int option;
      end' : int option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { path; source_file; begin'; end' } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_repeated_field 1 Field'.Int32_t path _o >>= fun () ->
        Bin'.serialize_optional_field 2 Field'.String_t source_file _o >>= fun () ->
        Bin'.serialize_optional_field 3 Field'.Int32_t begin' _o >>= fun () ->
        Bin'.serialize_optional_field 4 Field'.Int32_t end' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_repeated_field 1 Field'.Int32_t _m >>= fun path ->
        Bin'.decode_optional_field 2 Field'.String_t _m >>= fun source_file ->
        Bin'.decode_optional_field 3 Field'.Int32_t _m >>= fun begin' ->
        Bin'.decode_optional_field 4 Field'.Int32_t _m >>= fun end' ->
        Ok { path; source_file; begin'; end' }
  
    let rec to_text =
      fun { path; source_file; begin'; end' } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_repeated_field "path" Field'.Int32_t path _o >>= fun () ->
        Text'.serialize_optional_field "source_file" Field'.String_t source_file _o >>= fun () ->
        Text'.serialize_optional_field "begin" Field'.Int32_t begin' _o >>= fun () ->
        Text'.serialize_optional_field "end" Field'.Int32_t end' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_repeated_field "path" Field'.Int32_t _m >>= fun path ->
        Text'.decode_optional_field "source_file" Field'.String_t _m >>= fun source_file ->
        Text'.decode_optional_field "begin" Field'.Int32_t _m >>= fun begin' ->
        Text'.decode_optional_field "end" Field'.Int32_t _m >>= fun end' ->
        Ok { path; source_file; begin'; end' }
  end

  type t = {
    annotation : Generated_code_info.Annotation.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { annotation } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 1 Generated_code_info.Annotation.to_binary annotation _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 1 Generated_code_info.Annotation.of_binary _m >>= fun annotation ->
      Ok { annotation }

  let rec to_text =
    fun { annotation } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "annotation" Generated_code_info.Annotation.to_text annotation _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "annotation" Generated_code_info.Annotation.of_text _m >>= fun annotation ->
      Ok { annotation }
end
