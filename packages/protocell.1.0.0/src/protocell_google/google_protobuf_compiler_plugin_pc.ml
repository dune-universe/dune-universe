[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec Version : sig
  type t = {
    major : int option;
    minor : int option;
    patch : int option;
    suffix : string option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    major : int option;
    minor : int option;
    patch : int option;
    suffix : string option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { major; minor; patch; suffix } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.Int32_t major _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.Int32_t minor _o >>= fun () ->
      Bin'.serialize_optional_field 3 Field'.Int32_t patch _o >>= fun () ->
      Bin'.serialize_optional_field 4 Field'.String_t suffix _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.Int32_t _m >>= fun major ->
      Bin'.decode_optional_field 2 Field'.Int32_t _m >>= fun minor ->
      Bin'.decode_optional_field 3 Field'.Int32_t _m >>= fun patch ->
      Bin'.decode_optional_field 4 Field'.String_t _m >>= fun suffix ->
      Ok { major; minor; patch; suffix }

  let rec to_text =
    fun { major; minor; patch; suffix } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "major" Field'.Int32_t major _o >>= fun () ->
      Text'.serialize_optional_field "minor" Field'.Int32_t minor _o >>= fun () ->
      Text'.serialize_optional_field "patch" Field'.Int32_t patch _o >>= fun () ->
      Text'.serialize_optional_field "suffix" Field'.String_t suffix _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "major" Field'.Int32_t _m >>= fun major ->
      Text'.decode_optional_field "minor" Field'.Int32_t _m >>= fun minor ->
      Text'.decode_optional_field "patch" Field'.Int32_t _m >>= fun patch ->
      Text'.decode_optional_field "suffix" Field'.String_t _m >>= fun suffix ->
      Ok { major; minor; patch; suffix }
end

and Code_generator_request : sig
  type t = {
    file_to_generate : string list;
    parameter : string option;
    proto_file : Google_protobuf_descriptor_pc.File_descriptor_proto.t list;
    compiler_version : Version.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    file_to_generate : string list;
    parameter : string option;
    proto_file : Google_protobuf_descriptor_pc.File_descriptor_proto.t list;
    compiler_version : Version.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { file_to_generate; parameter; proto_file; compiler_version } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_field 1 Field'.String_t file_to_generate _o >>= fun () ->
      Bin'.serialize_optional_field 2 Field'.String_t parameter _o >>= fun () ->
      Bin'.serialize_repeated_user_field 15 Google_protobuf_descriptor_pc.File_descriptor_proto.to_binary proto_file _o >>= fun () ->
      Bin'.serialize_user_field 3 Version.to_binary compiler_version _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_field 1 Field'.String_t _m >>= fun file_to_generate ->
      Bin'.decode_optional_field 2 Field'.String_t _m >>= fun parameter ->
      Bin'.decode_repeated_user_field 15 Google_protobuf_descriptor_pc.File_descriptor_proto.of_binary _m >>= fun proto_file ->
      Bin'.decode_user_field 3 Version.of_binary _m >>= fun compiler_version ->
      Ok { file_to_generate; parameter; proto_file; compiler_version }

  let rec to_text =
    fun { file_to_generate; parameter; proto_file; compiler_version } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_field "file_to_generate" Field'.String_t file_to_generate _o >>= fun () ->
      Text'.serialize_optional_field "parameter" Field'.String_t parameter _o >>= fun () ->
      Text'.serialize_repeated_user_field "proto_file" Google_protobuf_descriptor_pc.File_descriptor_proto.to_text proto_file _o >>= fun () ->
      Text'.serialize_user_field "compiler_version" Version.to_text compiler_version _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_field "file_to_generate" Field'.String_t _m >>= fun file_to_generate ->
      Text'.decode_optional_field "parameter" Field'.String_t _m >>= fun parameter ->
      Text'.decode_repeated_user_field "proto_file" Google_protobuf_descriptor_pc.File_descriptor_proto.of_text _m >>= fun proto_file ->
      Text'.decode_user_field "compiler_version" Version.of_text _m >>= fun compiler_version ->
      Ok { file_to_generate; parameter; proto_file; compiler_version }
end

and Code_generator_response : sig
  module rec File : sig
    type t = {
      name : string option;
      insertion_point : string option;
      content : string option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    error : string option;
    file : Code_generator_response.File.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec File : sig
    type t = {
      name : string option;
      insertion_point : string option;
      content : string option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      name : string option;
      insertion_point : string option;
      content : string option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { name; insertion_point; content } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_optional_field 1 Field'.String_t name _o >>= fun () ->
        Bin'.serialize_optional_field 2 Field'.String_t insertion_point _o >>= fun () ->
        Bin'.serialize_optional_field 15 Field'.String_t content _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_optional_field 1 Field'.String_t _m >>= fun name ->
        Bin'.decode_optional_field 2 Field'.String_t _m >>= fun insertion_point ->
        Bin'.decode_optional_field 15 Field'.String_t _m >>= fun content ->
        Ok { name; insertion_point; content }
  
    let rec to_text =
      fun { name; insertion_point; content } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_optional_field "name" Field'.String_t name _o >>= fun () ->
        Text'.serialize_optional_field "insertion_point" Field'.String_t insertion_point _o >>= fun () ->
        Text'.serialize_optional_field "content" Field'.String_t content _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_optional_field "name" Field'.String_t _m >>= fun name ->
        Text'.decode_optional_field "insertion_point" Field'.String_t _m >>= fun insertion_point ->
        Text'.decode_optional_field "content" Field'.String_t _m >>= fun content ->
        Ok { name; insertion_point; content }
  end

  type t = {
    error : string option;
    file : Code_generator_response.File.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { error; file } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_optional_field 1 Field'.String_t error _o >>= fun () ->
      Bin'.serialize_repeated_user_field 15 Code_generator_response.File.to_binary file _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_optional_field 1 Field'.String_t _m >>= fun error ->
      Bin'.decode_repeated_user_field 15 Code_generator_response.File.of_binary _m >>= fun file ->
      Ok { error; file }

  let rec to_text =
    fun { error; file } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_optional_field "error" Field'.String_t error _o >>= fun () ->
      Text'.serialize_repeated_user_field "file" Code_generator_response.File.to_text file _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_optional_field "error" Field'.String_t _m >>= fun error ->
      Text'.decode_repeated_user_field "file" Code_generator_response.File.of_text _m >>= fun file ->
      Ok { error; file }
end
