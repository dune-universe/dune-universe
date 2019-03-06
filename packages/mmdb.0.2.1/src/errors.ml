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
    Mmdb_types.Error_code.(
      match error_code with
      | error_code when error_code = success -> None
      | error_code when error_code = corrupt_search_tree_error ->
          Some (`Corrupt_search_tree (get_message ()))
      | error_code when error_code = io_error ->
          Some (`Io_error (get_message ()))
      | error_code when error_code = out_of_memory_error ->
          Some (`Out_of_memory (get_message ()))
      | error_code when error_code = invalid_data_error ->
          Some (`Invalid_data (get_message ()))
      | _ ->
          let message =
            Printf.sprintf "Unrecognized error code: %d" error_code
          in
          Binding_integrity_error message |> raise)
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
    Mmdb_types.Error_code.(
      match error_code with
      | error_code when error_code = file_open_error ->
          Some (`File_open_error (get_message ()))
      | error_code when error_code = invalid_metadata_error ->
          Some (`Invalid_metadata (get_message ()))
      | error_code when error_code = unknown_database_format_error ->
          Some (`Unknown_database_format (get_message ()))
      | _ -> Common_error.of_error_code error_code)
end

module Lookup_ip_error = struct
  type t =
    [ `Invalid_address_info
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]

  let of_error_code ?(address_error_code = 0) error_code =
    if address_error_code != 0 then Some `Invalid_address_info
    else
      let get_error_message () = Error_code.to_message error_code in
      let fail error_name =
        let message =
          get_error_message ()
          |> Printf.sprintf "Bindings to libmaxminddb caused an '%s' error: %s"
               error_name
        in
        Binding_integrity_error message |> raise
      in
      Mmdb_types.Error_code.(
        match error_code with
        | error_code when error_code = invalid_lookup_path_error ->
            fail "invalid_lookup_path"
        | error_code when error_code = invalid_node_number_error ->
            fail "invalid_node_number"
        | error_code when error_code = ipv6_lookup_in_ipv4_database_error ->
            Some (`Ipv6_lookup_in_ipv4_database (get_error_message ()))
        | _ -> Common_error.of_error_code error_code)
end

module Lookup_error = struct
  type t = [`Unsupported_data_type of string | Lookup_ip_error.t]
  [@@deriving show]

  let is_ignorable_error_code error_code =
    error_code = Mmdb_types.Error_code.lookup_path_does_not_match_data_error
end
