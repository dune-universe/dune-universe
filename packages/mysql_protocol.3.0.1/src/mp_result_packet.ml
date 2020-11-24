
type result_packet = 
    Result_packet_ok of Mp_ok_packet.ok_packet
  | Result_packet_prepare_ok of Mp_ok_prepare_packet.ok_prepare_packet
  | Result_packet_result_set of Mp_result_set_packet.result_select
  | Result_packet_eof of Mp_eof_packet.eof_packet
  | Result_packet_error of Mp_error_packet.error_packet

let result_packet_to_string p = 
  match p with 
  | Result_packet_ok r -> Mp_ok_packet.ok_packet_to_string r 
  | Result_packet_prepare_ok r -> Mp_ok_prepare_packet.ok_prepare_packet_to_string r 
  | Result_packet_result_set r -> Mp_result_set_packet.result_select_to_string r
  | Result_packet_eof r -> Mp_eof_packet.eof_packet_to_string r
  | Result_packet_error r -> Mp_error_packet.error_packet_to_string r

let rec result_packet ic oc filter iter return_all_raw_mysql_data type_sent fields acc =
  let (_, packet_number, bits) = Mp_packet.extract_packet ic oc in
  let length_rest = (Bitstring.bitstring_length bits) - 8 in
  match%bitstring bits with
  | {| type_packet : 1*8 : int, unsigned, bigendian;
      rest : length_rest : bitstring |} ->
        if (type_packet = 0x00) then (
          match type_sent with
          | Mp_com.Prepare -> (
              let p = Mp_ok_prepare_packet.ok_prepare_packet rest ic oc in
              (Result_packet_prepare_ok p, packet_number) :: acc
            )
          | Mp_com.Fetch -> (
              let (_, p) = Mp_result_set_packet.result_set_packet (Int64.of_int type_packet)
                  rest ic oc filter iter return_all_raw_mysql_data type_sent fields in
              (Result_packet_result_set p, packet_number) :: acc
            )
          | _ -> (
              let p = Mp_ok_packet.ok_packet rest in
              (Result_packet_ok p, packet_number) :: acc
            )
        )
        else if (type_packet >= 1 && type_packet <= 250 ) then
          let (server_more_results_exists, p) =
            Mp_result_set_packet.result_set_packet (Int64.of_int type_packet)
              rest ic oc filter iter return_all_raw_mysql_data type_sent fields in
          if (server_more_results_exists) then (
            result_packet ic oc filter iter return_all_raw_mysql_data type_sent fields
              ((Result_packet_result_set p, packet_number) :: acc)
          ) else (
            (Result_packet_result_set p, packet_number) :: acc
          )
        else if type_packet = 0xfe then
          let p = Mp_eof_packet.eof_packet_bits rest in
          (Result_packet_eof p, packet_number) :: acc
        else if type_packet = 0xff then
          let p = Mp_error_packet.error_packet rest in
          (Result_packet_error p, packet_number) :: acc
        else
          failwith "Bad result packet"
