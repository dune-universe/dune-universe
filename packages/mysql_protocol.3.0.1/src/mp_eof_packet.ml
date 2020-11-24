
exception Bad_EOF_packet of Bitstring.bitstring

type eof_packet = {
  eof_field_count : int;
  eof_warning_count : int;
  eof_status_flags : int
}

let eof_packet_empty = {
  eof_field_count = 0xfe;
  eof_warning_count = 0;
  eof_status_flags = 0
}

let eof_packet_to_string p =
  Printf.sprintf "eof_field_count : %u\neof_warning_count : %u\neof_status_flags : %u\n"
    p.eof_field_count p.eof_warning_count p.eof_status_flags

type flag_server = 
    Server_status_in_trans
  | Server_status_autocommit
  | Server_more_results_exists
  | Server_status_no_good_index_used
  | Server_status_no_index_used
  | Server_status_cursor_exists
  | Server_status_last_row_sent
  | Server_status_db_dropped
  | Server_status_no_backslash_escapes
  | Server_status_metadata_changed
  | Server_query_was_slow
  | Server_ps_out_params

let flag_server_to_int f = 
  match f with
    Server_status_in_trans -> 0x0001
  | Server_status_autocommit -> 0x0002
  | Server_more_results_exists -> 0x0008
  | Server_status_no_good_index_used -> 0x0010
  | Server_status_no_index_used -> 0x0020
  | Server_status_cursor_exists -> 0x0040
  | Server_status_last_row_sent -> 0x0080
  | Server_status_db_dropped -> 0x0100
  | Server_status_no_backslash_escapes -> 0x0200
  | Server_status_metadata_changed -> 0x0400
  | Server_query_was_slow -> 0x0800
  | Server_ps_out_params -> 0x1000

let status_has_flag status flag = 
  let code = flag_server_to_int flag in
  (status land code) <> 0

let eof_packet_bits bits =
  (* field_count is always 0xfe *)
  let length = Bitstring.bitstring_length bits in
  if (length = 0) then
    (* the first byte 0xfe has already been eat *)
    { eof_field_count = 0xfe; eof_warning_count = 0; eof_status_flags = 0 }
  else if (length = 8) then (
    (* we only have the first byte 0xfe *)
    match%bitstring bits with
    | {| 0xfe : 1*8 : int, unsigned, littleendian |} ->
        { eof_field_count = 0xfe; eof_warning_count = 0; eof_status_flags = 0 }
  )
  else if (length = 32) then (
    (* complete EOF packet but the first byte 0xfe has already been eat *)
    match%bitstring bits with
    | {| warning_count : 2*8 : int, unsigned, littleendian;
        status_flags : 2*8 : int, unsigned, littleendian |} ->
      { eof_field_count = 0xfe; eof_warning_count = warning_count; eof_status_flags = status_flags }
  )
  else if (length = 40) then (
    (* complete EOF packet including the first byte 0xfe *)
    match%bitstring bits with
    | {| 0xfe : 1*8 : int, unsigned, littleendian;
        warning_count : 2*8 : int, unsigned, littleendian;
        status_flags : 2*8 : int, unsigned, littleendian |} ->
      { eof_field_count = 0xfe; eof_warning_count = warning_count; eof_status_flags = status_flags }
  )
  else (
    raise (Bad_EOF_packet bits)
  )

let eof_packet_chan ic oc =
  let (_, _, bits) = Mp_packet.extract_packet ic oc in
  eof_packet_bits bits
