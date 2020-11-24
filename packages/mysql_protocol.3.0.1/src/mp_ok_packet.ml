
type ok_packet = {
  ok_affected_rows : Int64.t;
  ok_insert_id : (Int64.t * Big_int.big_int);
  ok_server_status : int;
  ok_warning_count : int;
  ok_message : string;
}

let ok_packet_to_string p =
  let (insert_id_int64, insert_id_big_int) = p.ok_insert_id in
  let fmt = format_of_string "ok_affected_rows : %Lu\n"
    ^^ format_of_string "ok_insert_id : (%Lu, %s)\n"
    ^^ format_of_string "ok_server_status : %u\n"
    ^^ format_of_string "ok_warning_count : %u\n"
    ^^ format_of_string "ok_message : %s\n"
  in
  Printf.sprintf fmt p.ok_affected_rows
    insert_id_int64 (Big_int.string_of_big_int insert_id_big_int)
    p.ok_server_status
    p.ok_warning_count
    p.ok_message

let ok_packet bits = 
  let (affected_rows, rest) = Mp_binary.length_coded_binary bits in
  let (insert_id, rest) = Mp_binary.length_coded_binary rest in
  (* insert_id can be negative in two cases:
     - the auto_increment value is indeed negative (SIGNED field).
     - the returned value overflows Int64 maximum (BIGINT UNSIGNED field).
     Unfortunately, the protocol gives no way to differentiate these two
     cases (see https://bugs.mysql.com/bug.php?id=69228).
     So we return two values:
     - the first one is a Int64 and must be used when the
       auto_increment is _not_ a BIGINT UNSIGNED field.
     - the second one is a Big_int and must be used when the
       auto_increment is a BIGINT UNSIGNED field.*)
  let (insert_id_int64, insert_id_big_int) =
    if (Int64.compare insert_id Int64.zero >= 0) then
      (insert_id, Big_int.big_int_of_int64 insert_id)
    else
      let bi = Big_int.add_big_int (Big_int.power_int_positive_int 2 64)
        (Big_int.big_int_of_int64 insert_id)
      in
      (insert_id, bi)
  in
  if (Bitstring.bitstring_length rest > 0) then (
    let length_msg = (Bitstring.bitstring_length rest) - (4*8) in
    match%bitstring rest with
    | {| server_status : 2*8 : int, unsigned, bigendian;
        warning_count : 2*8 : int, unsigned, bigendian;
        message : length_msg : string |} -> (
          {
            ok_affected_rows = affected_rows;
            ok_insert_id = (insert_id_int64, insert_id_big_int);
            ok_server_status = server_status;
            ok_warning_count = warning_count;
            ok_message = message;
          }
      )
  )
  else (
    {
      ok_affected_rows = affected_rows;
      ok_insert_id = (insert_id_int64, insert_id_big_int);
      ok_server_status = 0;
      ok_warning_count = 0;
      ok_message = "";
    }
  )
