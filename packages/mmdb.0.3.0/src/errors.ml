exception Binding_integrity_error of string

module Error_code = struct
  let to_message error_code =
    Mmdb_ffi.Core.strerror error_code |> Pointers.Char_ptr.to_string
end

module Common_error = struct
  type t =
    [ `Corrupt_search_tree of string
    | `Io_error of string
    | `Out_of_memory of string
    | `Invalid_data of string ]
  [@@deriving show]

  let of_error_code error_code =
    let open Base in
    let get_message () = Error_code.to_message error_code in
    let module E = Mmdb_types.Error_code in
    match error_code with
    | error_code when error_code = E.success -> None
    | error_code when error_code = E.corrupt_search_tree_error ->
        Some (`Corrupt_search_tree (get_message ()))
    | error_code when error_code = E.io_error -> Some (`Io_error (get_message ()))
    | error_code when error_code = E.out_of_memory_error ->
        Some (`Out_of_memory (get_message ()))
    | error_code when error_code = E.invalid_data_error ->
        Some (`Invalid_data (get_message ()))
    | _ ->
        let message = Printf.sprintf "Unrecognized error code: %d" error_code in
        Binding_integrity_error message |> raise
end

module Open_file_error = struct
  type t =
    [ `File_open_error of string
    | `Invalid_metadata of string
    | `Unknown_database_format of string
    | Common_error.t ]
  [@@deriving show]

  let of_error_code error_code =
    let get_message () = Error_code.to_message error_code in
    let module E = Mmdb_types.Error_code in
    match error_code with
    | error_code when error_code = E.file_open_error ->
        Some (`File_open_error (get_message ()))
    | error_code when error_code = E.invalid_metadata_error ->
        Some (`Invalid_metadata (get_message ()))
    | error_code when error_code = E.unknown_database_format_error ->
        Some (`Unknown_database_format (get_message ()))
    | _ -> Common_error.of_error_code error_code
end

module Fetch_ip_data_error = struct
  type t =
    [ `Invalid_address_info
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]

  let of_error_code ?(address_error_code = 0) error_code =
    if address_error_code != 0
    then Some `Invalid_address_info
    else
      let get_error_message () = Error_code.to_message error_code in
      let module E = Mmdb_types.Error_code in
      match error_code with
      | error_code when error_code = E.ipv6_lookup_in_ipv4_database_error ->
          Some (`Ipv6_lookup_in_ipv4_database (get_error_message ()))
      | _ -> Common_error.of_error_code error_code
end

module Fetch_value_error = struct
  type t =
    [ `Invalid_lookup_path of string
    | `Invalid_node_number of string
    | `Unsupported_data_type of string
    | `Unexpected_data_type of string
    | Common_error.t ]
  [@@deriving show]

  let of_error_code error_code =
    let get_error_message () = Error_code.to_message error_code in
    let module E = Mmdb_types.Error_code in
    match error_code with
    | error_code when error_code = E.invalid_lookup_path_error ->
        Some (`Invalid_lookup_path (get_error_message ()))
    | error_code when error_code = E.invalid_node_number_error ->
        Some (`Invalid_node_number (get_error_message ()))
    | _ -> Common_error.of_error_code error_code

  let is_ignorable_error_code error_code =
    error_code = Mmdb_types.Error_code.lookup_path_does_not_match_data_error
end

module Fetch_error = struct
  type t =
    [ Fetch_ip_data_error.t
    | Fetch_value_error.t ]
  [@@deriving show]
end
