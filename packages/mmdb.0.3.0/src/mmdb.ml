open Base

type t = Mmdb_types.Mmdb.t

exception Binding_integrity_error = Errors.Binding_integrity_error

module Common_error = Errors.Common_error
module Open_file_error = Errors.Open_file_error
module Fetch_ip_data_error = Errors.Fetch_ip_data_error
module Fetch_value_error = Errors.Fetch_value_error
module Fetch_error = Errors.Fetch_error
module Path = Types.Path

let library_version = Mmdb_ffi.Core.lib_version () |> Pointers.Char_ptr.to_string

let open_file path =
  let path = path |> Path.to_string |> Pointers.Char_ptr.of_string in
  let should_close = ref false in
  let finalise mmdb = if !should_close then Mmdb_ffi.Core.close mmdb in
  let mmdb = Pointers.Mmdb.allocate ~finalise () in
  let result = Mmdb_ffi.Core.open_file path Mmdb_types.Mmdb_mode.mmap mmdb in
  match Open_file_error.of_error_code result with
  | None ->
      should_close := true;
      Ok mmdb
  | Some error -> Error error

module Version_number = Types.Version_number

let binary_format_version mmdb =
  let major =
    Mmdb_ffi.Helpers.binary_format_major_version mmdb |> Unsigned.UInt16.to_int
  in
  let minor =
    Mmdb_ffi.Helpers.binary_format_major_version mmdb |> Unsigned.UInt16.to_int
  in
  Version_number.of_major_and_minor (major, minor)

module Language = Types.Language

let languages mmdb =
  let count = Mmdb_ffi.Helpers.language_count mmdb |> Unsigned.Size_t.to_int in
  let name_pointers = Mmdb_ffi.Helpers.language_names mmdb in
  Pointers.Char_ptr_ptr.to_string_list count name_pointers
  |> List.map ~f:Language.of_string

let language_by_code mmdb language_code =
  languages mmdb
  |> List.find ~f:(fun each -> String.equal language_code (Language.to_string each))

module Ip = Types.Ip

type ip_data = Mmdb_types.Lookup_result.t Ctypes.structure

let fetch_ip_data mmdb ip =
  let ip = ip |> Ip.to_string |> Pointers.Char_ptr.of_string in
  let address_error_code_ptr = Pointers.Int_ptr.allocate () in
  let mmdb_error_code_ptr = Pointers.Int_ptr.allocate () in
  let lookup_result =
    Mmdb_ffi.Core.lookup_string mmdb ip address_error_code_ptr mmdb_error_code_ptr
  in
  let address_error_code = Ctypes.(!@address_error_code_ptr) in
  let mmdb_error_code = Ctypes.(!@mmdb_error_code_ptr) in
  match Fetch_ip_data_error.of_error_code ~address_error_code mmdb_error_code with
  | Some e -> Error e
  | None -> Ok lookup_result

type any_value =
  | String of string
  | Float of float
  | Int of int
  | Bool of bool

module Supported_data_type = struct
  type t =
    | Utf8_String
    | Double
    | Uint16
    | Uint32
    | Int32
    | Uint64
    | Boolean
    | Float

  let of_data_type_code code =
    let data_type = Unsigned.UInt32.to_int code in
    let module T = Mmdb_types.Entry_data_type in
    match data_type with
    | data_type when data_type = T.utf8_string -> Some Utf8_String
    | data_type when data_type = T.double -> Some Double
    | data_type when data_type = T.uint16 -> Some Uint16
    | data_type when data_type = T.uint32 -> Some Uint32
    | data_type when data_type = T.int32 -> Some Int32
    | data_type when data_type = T.uint64 -> Some Uint64
    | data_type when data_type = T.boolean -> Some Boolean
    | data_type when data_type = T.float -> Some Float
    | _ -> None

  let convert_to_value data_type entry_data_ptr =
    let module H = Mmdb_ffi.Helpers in
    match data_type with
    | Utf8_String ->
        let length = H.get_entry_data_size entry_data_ptr |> Unsigned.UInt32.to_int in
        String
          ( entry_data_ptr
          |> H.get_entry_data_utf8_string_value
          |> Pointers.Char_ptr.to_string_of_length length )
    | Double -> Float (entry_data_ptr |> H.get_entry_data_double_value)
    | Uint16 ->
        Int (entry_data_ptr |> H.get_entry_data_uint16_value |> Unsigned.UInt16.to_int)
    | Uint32 ->
        Int (entry_data_ptr |> H.get_entry_data_uint32_value |> Unsigned.UInt32.to_int)
    | Int32 -> Int (entry_data_ptr |> H.get_entry_data_int32_value |> Signed.Int32.to_int)
    | Uint64 ->
        Int (entry_data_ptr |> H.get_entry_data_uint64_value |> Unsigned.UInt64.to_int)
    | Boolean -> Bool (entry_data_ptr |> H.get_entry_data_boolean_value)
    | Float -> Float (entry_data_ptr |> H.get_entry_data_float_value)

  let get_value entry_data_ptr =
    let data_type_code = Mmdb_ffi.Helpers.get_entry_data_type entry_data_ptr in
    match of_data_type_code data_type_code with
    | Some data_type -> Ok (Some (convert_to_value data_type entry_data_ptr))
    | None -> Error (`Unsupported_data_type (Unsigned.UInt32.to_string data_type_code))
end

let any_value_from_db lookup_result query : (any_value option, _) Result.t =
  match Ctypes.getf lookup_result Mmdb_types.Lookup_result.found_entry with
  | false -> Ok None
  | true -> (
      let entry =
        Ctypes.getf lookup_result Mmdb_types.Lookup_result.entry |> Ctypes.addr
      in
      let entry_data_ptr = Pointers.Entry_data.allocate () in
      let query = Pointers.Char_ptr_ptr.of_string_list query in
      let error_code = Mmdb_ffi.Core.aget_value entry entry_data_ptr query in
      match Fetch_value_error.is_ignorable_error_code error_code with
      | true -> Ok None
      | false -> (
        match Fetch_value_error.of_error_code error_code with
        | Some e -> Error e
        | None -> (
          match Mmdb_ffi.Helpers.get_entry_data_has_data entry_data_ptr with
          | false -> Ok None
          | true -> Supported_data_type.get_value entry_data_ptr ) ) )

let value_from_db value_from_ip_data mmdb ip query =
  let open Result.Let_syntax in
  let%bind ip_data = fetch_ip_data mmdb ip in
  value_from_ip_data ip_data query

module Query = struct
  type t = string list

  let of_string_list = Fn.id

  let to_string_list = Fn.id
end

module type VALUE_TYPE = sig
  module Query : sig
    type t

    val of_string_list : string list -> t

    val to_string_list : t -> string list
  end

  type answer

  val from_db : t -> Ip.t -> Query.t -> (answer option, Fetch_error.t) Result.t

  val from_ip_data : ip_data -> Query.t -> (answer option, Fetch_value_error.t) Result.t
end

module Any = struct
  module Query = Query

  type answer = any_value

  let from_ip_data = any_value_from_db

  let from_db = value_from_db from_ip_data
end

module String = struct
  module Query = Query

  type answer = string

  let from_ip_data lookup_result query =
    let open Result.Let_syntax in
    match%bind any_value_from_db lookup_result query with
    | None -> Ok None
    | Some (String s) -> Ok (Some s)
    | Some _ ->
        Error (`Unexpected_data_type "String query returned an unexpected value type")

  let from_db = value_from_db from_ip_data

  let country_code = Query.of_string_list ["country"; "iso_code"]

  let region_code = Query.of_string_list ["subdivisions"; "0"; "iso_code"]

  let city_name language =
    Query.of_string_list ["city"; "names"; Language.to_string language]

  let country_name language =
    Query.of_string_list ["country"; "names"; Language.to_string language]

  let continent_name language =
    Query.of_string_list ["continent"; "names"; Language.to_string language]
end

module Float = struct
  module Query = Query

  type answer = float

  let from_ip_data lookup_result query =
    let open Result.Let_syntax in
    match%bind any_value_from_db lookup_result query with
    | None -> Ok None
    | Some (Float f) -> Ok (Some f)
    | Some _ ->
        Error (`Unexpected_data_type "Float query returned an unexpected value type")

  let from_db = value_from_db from_ip_data
end

module Int = struct
  module Query = Query

  type answer = int

  let from_ip_data lookup_result query =
    let open Result.Let_syntax in
    match%bind any_value_from_db lookup_result query with
    | None -> Ok None
    | Some (Int n) -> Ok (Some n)
    | Some _ ->
        Error (`Unexpected_data_type "Int query returned an unexpected value type")

  let from_db = value_from_db from_ip_data
end

module Bool = struct
  module Query = Query

  type answer = bool

  let from_ip_data lookup_result query =
    let open Result.Let_syntax in
    match%bind any_value_from_db lookup_result query with
    | None -> Ok None
    | Some (Bool b) -> Ok (Some b)
    | Some _ ->
        Error (`Unexpected_data_type "Bool query returned an unexpected value type")

  let from_db = value_from_db from_ip_data
end

module Coordinates = struct
  include Coordinates

  type answer = Coordinates.t

  module Query = Query

  let from_ip_data ip_data query =
    let latitude_query =
      List.concat [Query.of_string_list query; ["latitude"]] |> Query.of_string_list
    in
    let longitude_query =
      List.concat [Query.of_string_list query; ["longitude"]] |> Query.of_string_list
    in
    let open Result.Let_syntax in
    let%bind latitude = Float.from_ip_data ip_data latitude_query in
    let%bind longitude = Float.from_ip_data ip_data longitude_query in
    Ok
      ( match Option.both latitude longitude with
      | None -> None
      | Some (latitude, longitude) -> Some Coordinates.{latitude; longitude} )

  let from_db = value_from_db from_ip_data

  let location = Query.of_string_list ["location"]
end
