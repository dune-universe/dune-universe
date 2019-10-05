[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec Field_mask : sig
  type t = {
    paths : string list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    paths : string list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { paths } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_field 1 Field'.String_t paths _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_field 1 Field'.String_t _m >>= fun paths ->
      Ok { paths }

  let rec to_text =
    fun { paths } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_field "paths" Field'.String_t paths _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_field "paths" Field'.String_t _m >>= fun paths ->
      Ok { paths }
end
