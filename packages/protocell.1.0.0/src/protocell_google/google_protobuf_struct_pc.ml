[@@@ocaml.warning "-39"]

let (>>=) = Runtime.Result.(>>=)

let (>>|) = Runtime.Result.(>>|)

module Field' = Runtime.Field_value

module Bin' = Runtime.Binary_format

module Text' = Runtime.Text_format

module Null_value : sig
  type t =
    | Null_value
  [@@deriving eq, show]

  val default : unit -> t

  val to_int : t -> int

  val of_int : int -> t option

  val to_string : t -> string

  val of_string : string -> t option
end = struct
  type t =
    | Null_value
  [@@deriving eq, show]

  let default =
    fun () -> Null_value

  let to_int =
    function
    | Null_value -> 0

  let of_int =
    function
    | 0 -> Some Null_value
    | _ -> None

  let to_string =
    function
    | Null_value -> "NULL_VALUE"

  let of_string =
    function
    | "NULL_VALUE" -> Some Null_value
    | _ -> None
end

module rec Struct' : sig
  module rec Fields_entry : sig
    type t = {
      key : string;
      value' : Value'.t option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end

  type t = {
    fields : Struct'.Fields_entry.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module rec Fields_entry : sig
    type t = {
      key : string;
      value' : Value'.t option;
    }
    [@@deriving eq, show]
  
    val to_binary : t -> (string, [> Bin'.serialization_error]) result
  
    val of_binary : string -> (t, [> Bin'.deserialization_error]) result
  
    val to_text : t -> (string, [> Text'.serialization_error]) result
  
    val of_text : string -> (t, [> Text'.deserialization_error]) result
  end = struct
    type t = {
      key : string;
      value' : Value'.t option;
    }
    [@@deriving eq, show]
  
    let rec to_binary =
      fun { key; value' } ->
        let _o = Runtime.Byte_output.create () in
        Bin'.serialize_field 1 Field'.String_t key _o >>= fun () ->
        Bin'.serialize_user_field 2 Value'.to_binary value' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_binary =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_field 1 Field'.String_t _m >>= fun key ->
        Bin'.decode_user_field 2 Value'.of_binary _m >>= fun value' ->
        Ok { key; value' }
  
    let rec to_text =
      fun { key; value' } ->
        let _o = Runtime.Byte_output.create () in
        Text'.serialize_field "key" Field'.String_t key _o >>= fun () ->
        Text'.serialize_user_field "value" Value'.to_text value' _o >>= fun () ->
        Ok (Runtime.Byte_output.contents _o)
  
    let rec of_text =
      fun input' ->
        Ok (Runtime.Byte_input.create input') >>=
        Text'.deserialize_message >>= fun _m ->
        Text'.decode_field "key" Field'.String_t _m >>= fun key ->
        Text'.decode_user_field "value" Value'.of_text _m >>= fun value' ->
        Ok { key; value' }
  end

  type t = {
    fields : Struct'.Fields_entry.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { fields } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 1 Struct'.Fields_entry.to_binary fields _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 1 Struct'.Fields_entry.of_binary _m >>= fun fields ->
      Ok { fields }

  let rec to_text =
    fun { fields } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "fields" Struct'.Fields_entry.to_text fields _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "fields" Struct'.Fields_entry.of_text _m >>= fun fields ->
      Ok { fields }
end

and Value' : sig
  module Kind : sig
    type t =
      | Null_value of Null_value.t
      | Number_value of float
      | String_value of string
      | Bool_value of bool
      | Struct_value of Struct'.t
      | List_value of List_value.t
    [@@deriving eq, show]
  
    val null_value : Null_value.t -> t
    val number_value : float -> t
    val string_value : string -> t
    val bool_value : bool -> t
    val struct_value : Struct'.t -> t
    val list_value : List_value.t -> t
  end

  type t = {
    kind : Kind.t option;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  module Kind : sig
    type t =
      | Null_value of Null_value.t
      | Number_value of float
      | String_value of string
      | Bool_value of bool
      | Struct_value of Struct'.t
      | List_value of List_value.t
    [@@deriving eq, show]
  
    val null_value : Null_value.t -> t
    val number_value : float -> t
    val string_value : string -> t
    val bool_value : bool -> t
    val struct_value : Struct'.t -> t
    val list_value : List_value.t -> t
  end = struct
    type t =
      | Null_value of Null_value.t
      | Number_value of float
      | String_value of string
      | Bool_value of bool
      | Struct_value of Struct'.t
      | List_value of List_value.t
    [@@deriving eq, show]
  
    let null_value value = Null_value value
    let number_value value = Number_value value
    let string_value value = String_value value
    let bool_value value = Bool_value value
    let struct_value value = Struct_value value
    let list_value value = List_value value
  end

  type t = {
    kind : Kind.t option;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { kind } ->
      let _o = Runtime.Byte_output.create () in
      (match kind with
       | None -> Ok ()
       | Some Null_value null_value -> Bin'.serialize_enum_field 1 Null_value.to_int null_value _o
       | Some Number_value number_value -> Bin'.serialize_field 2 Field'.Double_t number_value _o
       | Some String_value string_value -> Bin'.serialize_field 3 Field'.String_t string_value _o
       | Some Bool_value bool_value -> Bin'.serialize_field 4 Field'.Bool_t bool_value _o
       | Some Struct_value struct_value -> Bin'.serialize_user_oneof_field 5 Struct'.to_binary struct_value _o
       | Some List_value list_value -> Bin'.serialize_user_oneof_field 6 List_value.to_binary list_value _o) >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
        Bin'.decode_oneof_field [
          1, (fun _m -> Bin'.decode_enum_field 1 Null_value.of_int Null_value.default _m >>| Kind.null_value);
          2, (fun _m -> Bin'.decode_field 2 Field'.Double_t _m >>| Kind.number_value);
          3, (fun _m -> Bin'.decode_field 3 Field'.String_t _m >>| Kind.string_value);
          4, (fun _m -> Bin'.decode_field 4 Field'.Bool_t _m >>| Kind.bool_value);
          5, (fun _m -> Bin'.decode_user_oneof_field 5 Struct'.of_binary _m >>| Kind.struct_value);
          6, (fun _m -> Bin'.decode_user_oneof_field 6 List_value.of_binary _m >>| Kind.list_value);
        ] _m >>= fun kind ->
      Ok { kind }

  let rec to_text =
    fun { kind } ->
      let _o = Runtime.Byte_output.create () in
      (match kind with
       | None -> Ok ()
       | Some Null_value null_value -> Text'.serialize_enum_field "null_value" Null_value.to_string null_value _o
       | Some Number_value number_value -> Text'.serialize_field "number_value" Field'.Double_t number_value _o
       | Some String_value string_value -> Text'.serialize_field "string_value" Field'.String_t string_value _o
       | Some Bool_value bool_value -> Text'.serialize_field "bool_value" Field'.Bool_t bool_value _o
       | Some Struct_value struct_value -> Text'.serialize_user_oneof_field "struct_value" Struct'.to_text struct_value _o
       | Some List_value list_value -> Text'.serialize_user_oneof_field "list_value" List_value.to_text list_value _o) >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
        Text'.decode_oneof_field [
          "null_value", (fun _m -> Text'.decode_enum_field "null_value" Null_value.of_string Null_value.default _m >>| Kind.null_value);
          "number_value", (fun _m -> Text'.decode_field "number_value" Field'.Double_t _m >>| Kind.number_value);
          "string_value", (fun _m -> Text'.decode_field "string_value" Field'.String_t _m >>| Kind.string_value);
          "bool_value", (fun _m -> Text'.decode_field "bool_value" Field'.Bool_t _m >>| Kind.bool_value);
          "struct_value", (fun _m -> Text'.decode_user_oneof_field "struct_value" Struct'.of_text _m >>| Kind.struct_value);
          "list_value", (fun _m -> Text'.decode_user_oneof_field "list_value" List_value.of_text _m >>| Kind.list_value);
        ] _m >>= fun kind ->
      Ok { kind }
end

and List_value : sig
  type t = {
    values : Value'.t list;
  }
  [@@deriving eq, show]

  val to_binary : t -> (string, [> Bin'.serialization_error]) result

  val of_binary : string -> (t, [> Bin'.deserialization_error]) result

  val to_text : t -> (string, [> Text'.serialization_error]) result

  val of_text : string -> (t, [> Text'.deserialization_error]) result
end = struct
  type t = {
    values : Value'.t list;
  }
  [@@deriving eq, show]

  let rec to_binary =
    fun { values } ->
      let _o = Runtime.Byte_output.create () in
      Bin'.serialize_repeated_user_field 1 Value'.to_binary values _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_binary =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Bin'.deserialize_message >>= fun _m ->
      Bin'.decode_repeated_user_field 1 Value'.of_binary _m >>= fun values ->
      Ok { values }

  let rec to_text =
    fun { values } ->
      let _o = Runtime.Byte_output.create () in
      Text'.serialize_repeated_user_field "values" Value'.to_text values _o >>= fun () ->
      Ok (Runtime.Byte_output.contents _o)

  let rec of_text =
    fun input' ->
      Ok (Runtime.Byte_input.create input') >>=
      Text'.deserialize_message >>= fun _m ->
      Text'.decode_repeated_user_field "values" Value'.of_text _m >>= fun values ->
      Ok { values }
end
