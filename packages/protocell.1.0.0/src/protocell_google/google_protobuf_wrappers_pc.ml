[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module rec Double_value : sig
  type t = {
    value' : float;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : float;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Double_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Double_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Double_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Double_t _m >>= fun value' ->
      Ok { value' }
end

and Float_value : sig
  type t = {
    value' : float;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : float;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Float_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Float_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Float_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Float_t _m >>= fun value' ->
      Ok { value' }
end

and Int64_value : sig
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Int64_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Int64_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Int64_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Int64_t _m >>= fun value' ->
      Ok { value' }
end

and U_int64_value : sig
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Uint64_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Uint64_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Uint64_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Uint64_t _m >>= fun value' ->
      Ok { value' }
end

and Int32_value : sig
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Int32_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Int32_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Int32_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Int32_t _m >>= fun value' ->
      Ok { value' }
end

and U_int32_value : sig
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : int;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Uint32_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Uint32_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Uint32_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Uint32_t _m >>= fun value' ->
      Ok { value' }
end

and Bool_value : sig
  type t = {
    value' : bool;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : bool;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Bool_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Bool_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Bool_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Bool_t _m >>= fun value' ->
      Ok { value' }
end

and String_value : sig
  type t = {
    value' : string;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : string;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.String_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.String_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.String_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.String_t _m >>= fun value' ->
      Ok { value' }
end

and Bytes_value : sig
  type t = {
    value' : string;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    value' : string;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_field 1 Field'.Bytes_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_field 1 Field'.Bytes_t _m >>= fun value' ->
      Ok { value' }

  let rec to_text =
    fun { value' } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_field "value" Field'.Bytes_t value' _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_field "value" Field'.Bytes_t _m >>= fun value' ->
      Ok { value' }
end
