[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec Duration : sig
  type t = {
    seconds : int;
    nanos : int;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    seconds : int;
    nanos : int;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { seconds; nanos } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Int64_t seconds _o >>= fun () ->
      Bin'.serialize_field 2 Field'.Int32_t nanos _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Int64_t _m >>= fun seconds ->
      Bin'.decode_field 2 Field'.Int32_t _m >>= fun nanos ->
      Ok { seconds; nanos }

  let rec to_text =
    fun { seconds; nanos } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "seconds" Field'.Int64_t seconds _o >>= fun () ->
      Text'.serialize_field "nanos" Field'.Int32_t nanos _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "seconds" Field'.Int64_t _m >>= fun seconds ->
      Text'.decode_field "nanos" Field'.Int32_t _m >>= fun nanos ->
      Ok { seconds; nanos }
end
