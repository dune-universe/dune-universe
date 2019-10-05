[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec Api : sig
  type t = {
    name : string;
    methods : Method'.t list;
    options : Google_protobuf_type_pc.Option.t list;
    version : string;
    source_context : Google_protobuf_source_context_pc.Source_context.t option;
    mixins : Mixin.t list;
    syntax : Google_protobuf_type_pc.Syntax.t;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    methods : Method'.t list;
    options : Google_protobuf_type_pc.Option.t list;
    version : string;
    source_context : Google_protobuf_source_context_pc.Source_context.t option;
    mixins : Mixin.t list;
    syntax : Google_protobuf_type_pc.Syntax.t;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; methods; options; version; source_context; mixins; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_repeated_user_field 2 Method'.to_binary methods _o >>= fun () ->
      Bin'.serialize_repeated_user_field 3 Google_protobuf_type_pc.Option.to_binary options _o >>= fun () ->
      Bin'.serialize_field 4 Field'.String_t version _o >>= fun () ->
      Bin'.serialize_user_field 5 Google_protobuf_source_context_pc.Source_context.to_binary source_context _o >>= fun () ->
      Bin'.serialize_repeated_user_field 6 Mixin.to_binary mixins _o >>= fun () ->
      Bin'.serialize_enum_field 7 Google_protobuf_type_pc.Syntax.to_int syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_repeated_user_field 2 Method'.of_binary _m >>= fun methods ->
      Bin'.decode_repeated_user_field 3 Google_protobuf_type_pc.Option.of_binary _m >>= fun options ->
      Bin'.decode_field 4 Field'.String_t _m >>= fun version ->
      Bin'.decode_user_field 5 Google_protobuf_source_context_pc.Source_context.of_binary _m >>= fun source_context ->
      Bin'.decode_repeated_user_field 6 Mixin.of_binary _m >>= fun mixins ->
      Bin'.decode_enum_field 7 Google_protobuf_type_pc.Syntax.of_int Google_protobuf_type_pc.Syntax.default _m >>= fun syntax ->
      Ok { name; methods; options; version; source_context; mixins; syntax }

  let rec to_text =
    fun { name; methods; options; version; source_context; mixins; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_repeated_user_field "methods" Method'.to_text methods _o >>= fun () ->
      Text'.serialize_repeated_user_field "options" Google_protobuf_type_pc.Option.to_text options _o >>= fun () ->
      Text'.serialize_field "version" Field'.String_t version _o >>= fun () ->
      Text'.serialize_user_field "source_context" Google_protobuf_source_context_pc.Source_context.to_text source_context _o >>= fun () ->
      Text'.serialize_repeated_user_field "mixins" Mixin.to_text mixins _o >>= fun () ->
      Text'.serialize_enum_field "syntax" Google_protobuf_type_pc.Syntax.to_string syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_repeated_user_field "methods" Method'.of_text _m >>= fun methods ->
      Text'.decode_repeated_user_field "options" Google_protobuf_type_pc.Option.of_text _m >>= fun options ->
      Text'.decode_field "version" Field'.String_t _m >>= fun version ->
      Text'.decode_user_field "source_context" Google_protobuf_source_context_pc.Source_context.of_text _m >>= fun source_context ->
      Text'.decode_repeated_user_field "mixins" Mixin.of_text _m >>= fun mixins ->
      Text'.decode_enum_field "syntax" Google_protobuf_type_pc.Syntax.of_string Google_protobuf_type_pc.Syntax.default _m >>= fun syntax ->
      Ok { name; methods; options; version; source_context; mixins; syntax }
end

and Method' : sig
  type t = {
    name : string;
    request_type_url : string;
    request_streaming : bool;
    response_type_url : string;
    response_streaming : bool;
    options : Google_protobuf_type_pc.Option.t list;
    syntax : Google_protobuf_type_pc.Syntax.t;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    request_type_url : string;
    request_streaming : bool;
    response_type_url : string;
    response_streaming : bool;
    options : Google_protobuf_type_pc.Option.t list;
    syntax : Google_protobuf_type_pc.Syntax.t;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; request_type_url; request_streaming; response_type_url; response_streaming; options; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_field 2 Field'.String_t request_type_url _o >>= fun () ->
      Bin'.serialize_field 3 Field'.Bool_t request_streaming _o >>= fun () ->
      Bin'.serialize_field 4 Field'.String_t response_type_url _o >>= fun () ->
      Bin'.serialize_field 5 Field'.Bool_t response_streaming _o >>= fun () ->
      Bin'.serialize_repeated_user_field 6 Google_protobuf_type_pc.Option.to_binary options _o >>= fun () ->
      Bin'.serialize_enum_field 7 Google_protobuf_type_pc.Syntax.to_int syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_field 2 Field'.String_t _m >>= fun request_type_url ->
      Bin'.decode_field 3 Field'.Bool_t _m >>= fun request_streaming ->
      Bin'.decode_field 4 Field'.String_t _m >>= fun response_type_url ->
      Bin'.decode_field 5 Field'.Bool_t _m >>= fun response_streaming ->
      Bin'.decode_repeated_user_field 6 Google_protobuf_type_pc.Option.of_binary _m >>= fun options ->
      Bin'.decode_enum_field 7 Google_protobuf_type_pc.Syntax.of_int Google_protobuf_type_pc.Syntax.default _m >>= fun syntax ->
      Ok { name; request_type_url; request_streaming; response_type_url; response_streaming; options; syntax }

  let rec to_text =
    fun { name; request_type_url; request_streaming; response_type_url; response_streaming; options; syntax } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_field "request_type_url" Field'.String_t request_type_url _o >>= fun () ->
      Text'.serialize_field "request_streaming" Field'.Bool_t request_streaming _o >>= fun () ->
      Text'.serialize_field "response_type_url" Field'.String_t response_type_url _o >>= fun () ->
      Text'.serialize_field "response_streaming" Field'.Bool_t response_streaming _o >>= fun () ->
      Text'.serialize_repeated_user_field "options" Google_protobuf_type_pc.Option.to_text options _o >>= fun () ->
      Text'.serialize_enum_field "syntax" Google_protobuf_type_pc.Syntax.to_string syntax _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_field "request_type_url" Field'.String_t _m >>= fun request_type_url ->
      Text'.decode_field "request_streaming" Field'.Bool_t _m >>= fun request_streaming ->
      Text'.decode_field "response_type_url" Field'.String_t _m >>= fun response_type_url ->
      Text'.decode_field "response_streaming" Field'.Bool_t _m >>= fun response_streaming ->
      Text'.decode_repeated_user_field "options" Google_protobuf_type_pc.Option.of_text _m >>= fun options ->
      Text'.decode_enum_field "syntax" Google_protobuf_type_pc.Syntax.of_string Google_protobuf_type_pc.Syntax.default _m >>= fun syntax ->
      Ok { name; request_type_url; request_streaming; response_type_url; response_streaming; options; syntax }
end

and Mixin : sig
  type t = {
    name : string;
    root : string;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    name : string;
    root : string;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { name; root } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t name _o >>= fun () ->
      Bin'.serialize_field 2 Field'.String_t root _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun name ->
      Bin'.decode_field 2 Field'.String_t _m >>= fun root ->
      Ok { name; root }

  let rec to_text =
    fun { name; root } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "name" Field'.String_t name _o >>= fun () ->
      Text'.serialize_field "root" Field'.String_t root _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "name" Field'.String_t _m >>= fun name ->
      Text'.decode_field "root" Field'.String_t _m >>= fun root ->
      Ok { name; root }
end
