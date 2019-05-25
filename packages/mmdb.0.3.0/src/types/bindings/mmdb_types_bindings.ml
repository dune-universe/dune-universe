module M (F : Ctypes.TYPE) = struct
  type 'a typ = 'a F.typ

  type 'a structure = 'a Ctypes.structure F.typ

  module Mmdb = struct
    type t = unit Ctypes_static.ptr

    let t : t typ = F.(ptr void)
  end

  module Entry = struct
    type t

    let t : t structure = F.structure "MMDB_entry_s"

    let mmdb = F.(field t "mmdb" Mmdb.t)

    let entry_offset = F.(field t "offset" uint32_t)

    let () = F.seal t
  end

  module Lookup_result = struct
    type t

    let t : t structure = F.structure "MMDB_lookup_result_s"

    let found_entry = F.(field t "found_entry" bool)

    let entry = F.(field t "entry" Entry.t)

    let netmask = F.(field t "netmask" uint16_t)

    let () = F.seal t
  end

  module Entry_data = struct
    type t = unit Ctypes_static.ptr

    let t : t typ = F.(ptr void)
  end

  module Entry_data_type = struct
    let extended = F.(constant "MMDB_DATA_TYPE_EXTENDED" int)

    let pointer = F.(constant "MMDB_DATA_TYPE_POINTER" int)

    let utf8_string = F.(constant "MMDB_DATA_TYPE_UTF8_STRING" int)

    let double = F.(constant "MMDB_DATA_TYPE_DOUBLE" int)

    let bytes = F.(constant "MMDB_DATA_TYPE_BYTES" int)

    let uint16 = F.(constant "MMDB_DATA_TYPE_UINT16" int)

    let uint32 = F.(constant "MMDB_DATA_TYPE_UINT32" int)

    let map = F.(constant "MMDB_DATA_TYPE_MAP" int)

    let int32 = F.(constant "MMDB_DATA_TYPE_INT32" int)

    let uint64 = F.(constant "MMDB_DATA_TYPE_UINT64" int)

    let uint128 = F.(constant "MMDB_DATA_TYPE_UINT128" int)

    let array = F.(constant "MMDB_DATA_TYPE_ARRAY" int)

    let container = F.(constant "MMDB_DATA_TYPE_CONTAINER" int)

    let end_marker = F.(constant "MMDB_DATA_TYPE_END_MARKER" int)

    let boolean = F.(constant "MMDB_DATA_TYPE_BOOLEAN" int)

    let float = F.(constant "MMDB_DATA_TYPE_FLOAT" int)
  end

  module Mmdb_mode = struct
    let mmap = F.(constant "MMDB_MODE_MMAP" int)

    let mask = F.(constant "MMDB_MODE_MASK" int)
  end

  module Error_code = struct
    let success = F.(constant "MMDB_SUCCESS" int)

    let file_open_error = F.(constant "MMDB_FILE_OPEN_ERROR" int)

    let corrupt_search_tree_error = F.(constant "MMDB_CORRUPT_SEARCH_TREE_ERROR" int)

    let invalid_metadata_error = F.(constant "MMDB_INVALID_METADATA_ERROR" int)

    let io_error = F.(constant "MMDB_IO_ERROR" int)

    let out_of_memory_error = F.(constant "MMDB_OUT_OF_MEMORY_ERROR" int)

    let unknown_database_format_error =
      F.(constant "MMDB_UNKNOWN_DATABASE_FORMAT_ERROR" int)

    let invalid_data_error = F.(constant "MMDB_INVALID_DATA_ERROR" int)

    let invalid_lookup_path_error = F.(constant "MMDB_INVALID_LOOKUP_PATH_ERROR" int)

    let lookup_path_does_not_match_data_error =
      F.(constant "MMDB_LOOKUP_PATH_DOES_NOT_MATCH_DATA_ERROR" int)

    let invalid_node_number_error = F.(constant "MMDB_INVALID_NODE_NUMBER_ERROR" int)

    let ipv6_lookup_in_ipv4_database_error =
      F.(constant "MMDB_IPV6_LOOKUP_IN_IPV4_DATABASE_ERROR" int)
  end
end
