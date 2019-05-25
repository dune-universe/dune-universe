module T = Mmdb_types

module M (F : Ctypes.FOREIGN) = struct
  module C = struct
    include Ctypes

    let ( @-> ) = F.( @-> )

    let returning = F.returning
  end

  module Core = struct
    let open_file =
      F.foreign "MMDB_open" C.(ptr char @-> int @-> T.Mmdb.t @-> returning int)

    let close = F.foreign "MMDB_close" C.(T.Mmdb.t @-> returning void)

    let lookup_string =
      F.foreign
        "MMDB_lookup_string"
        C.(T.Mmdb.t @-> ptr char @-> ptr int @-> ptr int @-> returning T.Lookup_result.t)

    let aget_value =
      F.foreign
        "MMDB_aget_value"
        C.(ptr T.Entry.t @-> T.Entry_data.t @-> ptr (ptr char) @-> returning int)

    let strerror = F.foreign "MMDB_strerror" C.(int @-> returning (ptr char))

    let lib_version = F.foreign "MMDB_lib_version" C.(void @-> returning (ptr char))
  end

  module Helpers = struct
    let size_of_mmdb_s = F.foreign "mmdb_ml_sizeof_mmdb_s" C.(void @-> returning size_t)

    let alignment_of_mmdb_s =
      F.foreign "mmdb_ml_alignof_mmdb_s" C.(void @-> returning size_t)

    let size_of_mmdb_entry_data_s =
      F.foreign "mmdb_ml_sizeof_mmdb_entry_data_s" C.(void @-> returning size_t)

    let alignment_of_mmdb_entry_data_s =
      F.foreign "mmdb_ml_alignof_mmdb_entry_data_s" C.(void @-> returning size_t)

    let language_count =
      F.foreign "mmdb_ml_language_count" C.(T.Mmdb.t @-> returning size_t)

    let language_names =
      F.foreign "mmdb_ml_language_names" C.(T.Mmdb.t @-> returning (ptr (ptr char)))

    let binary_format_major_version =
      F.foreign "mmdb_ml_binary_format_major_version" C.(T.Mmdb.t @-> returning uint16_t)

    let binary_format_minor_version =
      F.foreign "mmdb_ml_binary_format_minor_version" C.(T.Mmdb.t @-> returning uint16_t)

    let get_entry_data_has_data =
      F.foreign "mmdb_ml_get_entry_data_has_data" C.(T.Entry_data.t @-> returning bool)

    let get_entry_data_type =
      F.foreign "mmdb_ml_get_entry_data_type" C.(T.Entry_data.t @-> returning uint32_t)

    let get_entry_data_size =
      F.foreign "mmdb_ml_get_entry_data_size" C.(T.Entry_data.t @-> returning uint32_t)

    let get_entry_data_utf8_string_value =
      F.foreign
        "mmdb_ml_get_entry_data_utf8_string_value"
        C.(T.Entry_data.t @-> returning (ptr char))

    let get_entry_data_double_value =
      F.foreign
        "mmdb_ml_get_entry_data_double_value"
        C.(T.Entry_data.t @-> returning double)

    let get_entry_data_bytes_value =
      F.foreign
        "mmdb_ml_get_entry_data_bytes_value"
        C.(T.Entry_data.t @-> returning (ptr uint8_t))

    let get_entry_data_uint16_value =
      F.foreign
        "mmdb_ml_get_entry_data_uint16_value"
        C.(T.Entry_data.t @-> returning uint16_t)

    let get_entry_data_uint32_value =
      F.foreign
        "mmdb_ml_get_entry_data_uint32_value"
        C.(T.Entry_data.t @-> returning uint32_t)

    let get_entry_data_int32_value =
      F.foreign
        "mmdb_ml_get_entry_data_int32_value"
        C.(T.Entry_data.t @-> returning int32_t)

    let get_entry_data_uint64_value =
      F.foreign
        "mmdb_ml_get_entry_data_uint64_value"
        C.(T.Entry_data.t @-> returning uint64_t)

    let get_entry_data_boolean_value =
      F.foreign
        "mmdb_ml_get_entry_data_boolean_value"
        C.(T.Entry_data.t @-> returning bool)

    let get_entry_data_float_value =
      F.foreign
        "mmdb_ml_get_entry_data_float_value"
        C.(T.Entry_data.t @-> returning float)
  end
end
