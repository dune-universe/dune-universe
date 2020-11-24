
let build_change_user ~handshake ~user ~password ~databasename ~charset_number ~auth_plugin_name = 
  let user = Mp_string.make_null_terminated_string user in
  let length_user = String.length user in

  let scramble_buff = Bitstring.concat [handshake.Mp_handshake.scramble_buff_1; handshake.Mp_handshake.scramble_buff_2] in
  let scramble_buff = Bitstring.string_of_bitstring scramble_buff in
  let credential =
    if (String.length password > 0) then (
      (* /!\ : length coded binary <= 250 bytes ?? *)
      let encoded_password = Mp_authentication.encode_client_password scramble_buff password in
      let length_encoded_password = String.length encoded_password in
      let c = Char.chr length_encoded_password in
      (String.make 1 c) ^ encoded_password
    )
    else (
      String.make 1 '\x00'
    )
  in
  let length_credential = String.length credential in
  let db = Bitstring.bitstring_of_string (Mp_string.make_null_terminated_string databasename) in
  let length_db = Bitstring.bitstring_length db in
  let plugin = Mp_string.make_null_terminated_string auth_plugin_name in
  let length_plugin = String.length plugin in
  let%bitstring bits = {|
      user : length_user*8 : string;
      credential : length_credential*8 : string;
      db : length_db : bitstring;
      charset_number : 2*8 : int, unsigned, bigendian;
      plugin : length_plugin*8 : string
    |} 
  in
  bits
