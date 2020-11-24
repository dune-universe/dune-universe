
type handshake = {
  packet_length : int;
  packet_number : int;
  protocol_version : int;
  server_version : string;
  thread_id : Int64.t;
  scramble_buff_1 : Bitstring.t;
  server_capabilities : Mp_capabilities.capabilities list;
  server_language : Mp_charset.charset;
  server_status : int;
  length_scramble : int;
  scramble_buff_2 : Bitstring.t
}

let handshake_to_string handshake =
  let fmt = format_of_string "packet_length : %u\n"
    ^^ format_of_string "packet_number : %u\n"
    ^^ format_of_string "protocol_version : %u\n"
    ^^ format_of_string "server_version : %s\n"
    ^^ format_of_string "thread_id : %Lu\n"
    ^^ format_of_string "scramble_buff_1 : %s\n"
    ^^ format_of_string "server_capabilities : %s\n"
    ^^ format_of_string "server_language : %s\n"
    ^^ format_of_string "server_status : %u\n"
    ^^ format_of_string "length_scramble : %u\n"
    ^^ format_of_string "scramble_buff_2 : %s\n"
  in
  Printf.sprintf fmt handshake.packet_length
    handshake.packet_number
    handshake.protocol_version
    handshake.server_version
    handshake.thread_id
    (Bitstring.string_of_bitstring handshake.scramble_buff_1)
    (Mp_capabilities.capabilities_to_string handshake.server_capabilities)
    (Mp_charset.charset_to_string handshake.server_language)
    handshake.server_status
    handshake.length_scramble
    (Bitstring.string_of_bitstring handshake.scramble_buff_2)

let handshake_initialisation ic oc =
  let (packet_length, packet_number, bits) = Mp_packet.extract_packet ic oc in
  let length_bits = (Bitstring.bitstring_length bits) - 8 in
  match%bitstring bits with
  | {| protocol_version : 1*8 : int, unsigned, bigendian; (* always = 10 ?? (see send_server_handshake_packet function in sql_acl.cc) *)
      rest : length_bits : bitstring |} -> (
        let (rest, server_version) = Mp_string.null_terminated_string rest "" in
        let length_rest = (Bitstring.bitstring_length rest) - (31*8) in
        match%bitstring rest with
        | {| thread_id : 4*8 : int, unsigned, littleendian;
            scramble_buff_1 : 8*8 : bitstring;
            0x00 : 1*8 : int, unsigned, bigendian;
            server_capabilities : 2*8 : bitstring;
            server_language : 1*8 : int, unsigned, bigendian;
            server_status : 2*8 : int, unsigned, bigendian; 
            server_capabilities_upper : 2 * 8 : bitstring; (* server capabilities (two upper bytes) *)
            length_scramble : 1 * 8 : int, unsigned, bigendian; (* length of the scramble *)
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            _ : 1 * 8 : int, unsigned, bigendian;
            rest : length_rest : bitstring |} ->
              (* thread_id is a 4 bytes unsigned integer (AND NOT a length coded binary) *)
              let thread_id = Int64.of_int32 thread_id in
              let server_language = Mp_charset.number_charset server_language in
              let server_capabilities = Mp_capabilities.decode_server_capabilities 
                  (Bitstring.concat [server_capabilities; server_capabilities_upper]) in
                  (* scramble_buff_2 = rest of the plugin provided data (at least 12 bytes)
                     \0 byte, terminating the second part of a scramble *)
              let (_, scramble_buff_2) = Mp_string.null_terminated_string rest "" in
              let scramble_buff_2 = Bitstring.bitstring_of_string scramble_buff_2 in
              let handshake =
                { packet_length = packet_length;
                  packet_number = packet_number;
                  protocol_version = protocol_version;
                  server_version = server_version;
                  thread_id = thread_id;
                  scramble_buff_1 = scramble_buff_1;
                  server_capabilities = server_capabilities;
                  server_language = server_language;
                  server_status = server_status;
                  length_scramble = length_scramble;
                  scramble_buff_2 = scramble_buff_2;
                }
              in
              handshake
        )
