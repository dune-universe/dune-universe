[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec Any : sig
  type t = {
    type_url : string;
    value' : string;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    type_url : string;
    value' : string;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { type_url; value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t type_url _o >>= fun () ->
      Bin'.serialize_field 2 Field'.Bytes_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun type_url ->
      Bin'.decode_field 2 Field'.Bytes_t _m >>= fun value' ->
      Ok { type_url; value' }

  let rec to_text =
    fun { type_url; value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "type_url" Field'.String_t type_url _o >>= fun () ->
      Text'.serialize_field "value" Field'.Bytes_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "type_url" Field'.String_t _m >>= fun type_url ->
      Text'.decode_field "value" Field'.Bytes_t _m >>= fun value' ->
      Ok { type_url; value' }
end
