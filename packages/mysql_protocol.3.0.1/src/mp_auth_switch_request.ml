
type auth_switch_request_packet = {
  plugin_name : string;
  plugin_data : Bitstring.t
}

let auth_switch_request_packet_to_string p =
  Printf.sprintf "plugin_name : %s\nplugin_data : %s\n"
    p.plugin_name (Bitstring.string_of_bitstring p.plugin_data)

let auth_switch_request_packet_bits_without_0xFE_prefix bits =
  let length = Bitstring.bitstring_length bits in
  match%bitstring bits with
  | {|  "mysql_native_password" : 21*8 : string;
        0x00 : 8 : int;
        plugin_data : length - ((21+1+1)*8) : bitstring;
        0x00 : 8 : int |} ->
    { plugin_name = "mysql_native_password"; plugin_data }
