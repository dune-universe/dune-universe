let xor_string s1 s2 = 
  (* s1 and s2 must have the same length *)
  let s = ref "" in
  let i = ref 0 in
  let f c1 = 
    let c2 = String.get s2 !i in
    let c = (Char.code c1) lxor (Char.code c2) in
    let c = Char.chr c in 
    s := !s ^ (String.make 1 c);
    i := !i + 1
  in
  let () = String.iter f s1 in
  !s

let encode_client_password scramble password = 
  let hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1()) s in
  let hash_stage1 = hash password in
  let hash_stage2 = hash hash_stage1 in
  let hash_stage3 = hash (scramble ^ hash_stage2) in
  let reply = xor_string hash_stage1 hash_stage3 in
  reply

(* 
   /!\ : bad capabilities can prevent right authentication
   This list works : 
   Client_long_password; Client_long_flag; Client_protocol_41; 
   Client_transactions; Client_secure_connection; 
 *)
let client_authentication_packet ~handshake ~capabilities ~max_packet_size ~charset_number ~user ~password ~databasename ~auth_plugin_name =
  let client_flags = Int64.of_int (Mp_capabilities.encode_client_capabilities capabilities) in
  let filler = Bitstring.make_bitstring (23*8) '\x00' in
  let user = Mp_string.make_null_terminated_string user in
  let length_user = String.length user in

  let scramble_buff = Bitstring.concat [handshake.Mp_handshake.scramble_buff_1; handshake.Mp_handshake.scramble_buff_2] in
  let scramble_buff = Bitstring.string_of_bitstring scramble_buff in
  let credential =
    if (String.length password > 0) then (
      (* /!\ : length coded binary <= 250 bytes ?? *)
      let encoded_password = encode_client_password scramble_buff password in
      let length_encoded_password = String.length encoded_password in
      let c = Char.chr length_encoded_password in
      (String.make 1 c) ^ encoded_password
    )
    else (
      String.make 1 '\x00'
    )
  in
  let length_credential = String.length credential in

  (* TODO (?): check CLIENT_CONNECT_WITH_DB is set in flags *)
  let db = Bitstring.bitstring_of_string (Mp_string.make_null_terminated_string databasename) in
  let length_db = Bitstring.bitstring_length db in

  (* TODO (?): check CLIENT_PLUGIN_AUTH is set in flags *)
  let plugin = Mp_string.make_null_terminated_string auth_plugin_name in
  let length_plugin = String.length plugin in

  let%bitstring bits = {|
      client_flags : Mp_bitstring.compute32 : int, unsigned, littleendian;
      max_packet_size : Mp_bitstring.compute32 : int, unsigned, bigendian;
      charset_number : 1*8 : int, unsigned, bigendian;
      filler : 23*8 : bitstring;
      user : length_user*8 : string;
      credential : length_credential*8 : string;
      db : length_db : bitstring;
      plugin : length_plugin*8 : string
    |}
  in 
  let bits = Bitstring.concat [bits; db] in
  let bits = Mp_packet.make_packet handshake.Mp_handshake.packet_number bits in
  bits
