
type result_set_packet = {
  result_set_field_count : Int64.t;
  result_set_extra : Int64.t;
  result_set_field_packets : Mp_field_packet.field_packet list;
  result_set_row_data_packets : Mp_raw_data.row_data list list;
}

type result_select = { 
  rows : (Mp_field.field_name list * Mp_data.t list list);
  mysql_data : result_set_packet option;
}

let rows_to_string result_select = 
  let (field_names, data) = result_select in
  let count_records = ref 1 in
  let count_fields = ref 0 in
  let row acc v = 
    let s =
      match (Mp_data.to_string v) with
      | None -> "NULL"
      | Some x -> x
    in
    let (f, _) = List.nth field_names !count_fields in
    let () = incr count_fields in
    let s = f ^ ": " ^ s ^ "\n" in
    acc ^ s
  in
  let rows acc record = 
    let () = count_fields := 0 in
    let s = List.fold_left row "" record in
    let s = "\n ---- Record " ^ (string_of_int !count_records) ^ " ---- \n" ^ s ^ " ---- " in
    let () = incr count_records in
    acc ^ s
  in
  List.fold_left rows "" data

let result_set_packet_to_string p =
  let field_packets = 
    let f acc e = 
      acc ^ "---- field packet ----\n" ^ (Mp_field_packet.field_packet_to_string e) ^ "---- ----\n"
    in
    List.fold_left f "" p.result_set_field_packets
  in
  let row_data_packets = 
    let f1 acc e = 
      acc ^ "---- data packet ----\n" ^ (Mp_raw_data.row_data_to_string e) ^ "---- ----\n"
    in
    let f acc e = 
      acc ^ (List.fold_left f1 "" e)
    in
    List.fold_left f "" p.result_set_row_data_packets
  in
  let fmt = format_of_string "result_set_field_count : %Lu\n"
    ^^ format_of_string "result_set_extra : %Lu\n"
    ^^ format_of_string "result_set_field_packets : \n%s\n"
    ^^ format_of_string "result_set_row_data_packets : \n%s\n"
  in
  Printf.sprintf fmt p.result_set_field_count
    p.result_set_extra
    field_packets
    row_data_packets

let result_select_to_string result_select =  
  let s1 = rows_to_string result_select.rows in
  let s2 = 
    match result_select.mysql_data with
    | None -> "No MySQL data"
    | Some v -> result_set_packet_to_string v
  in
  s1 ^ "\n" ^ s2

let result_set_packet result_set_field_count bits ic oc filter iter return_all_raw_mysql_data type_sent fields =
  let count = Int64.to_int result_set_field_count in
  let extra = 
    if Bitstring.bitstring_length bits = 8 then
      let (n, _) = Mp_binary.length_coded_binary bits in
      n
    else
      Int64.zero
  in
  let list_field_packets = 
    match type_sent with
    | Mp_com.Fetch -> fields (* no fields packets are sent in fetch case *)
    | _ -> (
        let list_field_packets = ref [] in
        let () =
          if count > 0 then 
            for _ = 1 to count do
              Mp_field_packet.field_packet list_field_packets ic oc
            done
        in
        List.rev !list_field_packets 
      )
  in
  let list_field_names = Mp_field.real_field_names list_field_packets in
  let eof_packet = 
    match type_sent with
    | Mp_com.Fetch -> Mp_eof_packet.eof_packet_empty (* no eof packet to read in fetch case *)
    | _ -> Mp_eof_packet.eof_packet_chan ic oc
  in
  let list_raw_data_packets = ref [] in
  let data_packets_end = ref false in
  let server_more_results_exists = ref false in
  let list_sql_data = ref [] in
  let () =
    let status_cursor_exists = Mp_eof_packet.status_has_flag eof_packet.Mp_eof_packet.eof_status_flags Mp_eof_packet.Server_status_cursor_exists in
    let data_part_exist = 
      match type_sent with
      | Mp_com.Fetch -> true
      | _ -> if status_cursor_exists then false else true
    in
    if (data_part_exist) then
      let (_, _, bits) = 
        match type_sent with
        | Mp_com.Fetch -> (0, 0, bits) (* no extract packet for the first time in the fetch case *)
        | _ -> Mp_packet.extract_packet ic oc
      in
      let bits = ref bits in
      let count_rows = ref 0 in
      while (not !data_packets_end) do
        let first_byte = Bitstring.takebits 8 !bits in
        let () = 
          match%bitstring first_byte with
          | {| test_packets_end : 1*8 : int, unsigned, bigendian |} -> (
              if (test_packets_end = 0xfe) then (
                data_packets_end := true;
                let eof = Mp_eof_packet.eof_packet_bits !bits in
                if (Mp_eof_packet.status_has_flag 
                      eof.Mp_eof_packet.eof_status_flags 
                      Mp_eof_packet.Server_more_results_exists) then (
                  server_more_results_exists := true
                )
              )
            )
        in
        if (not !data_packets_end) then
          let l = Mp_raw_data.raw_data_packet list_field_packets type_sent !count_rows !bits in
          let () = 
            if return_all_raw_mysql_data then
              list_raw_data_packets := l :: !list_raw_data_packets 
          in
          let sql_data = Mp_data_process.data_row list_field_packets l in
          let filtered = ref false in
          let () = 
            match filter with 
            | None -> list_sql_data := sql_data :: !list_sql_data
            | Some f ->
              if (f list_field_names sql_data) then 
                list_sql_data := sql_data :: !list_sql_data
              else
                filtered := true
          in
          let () = 
            match iter with
            | None -> ()
            | Some f -> 
              if (not !filtered) then
                f list_field_names sql_data
          in
          let () = incr count_rows in
          let (_, _, next) = Mp_packet.extract_packet ic oc in
          bits := next
      done
  in
  let list_raw_data_packets = List.rev !list_raw_data_packets in
  let list_sql_data = List.rev !list_sql_data in
  (* Final EOF packet is read in the while loop below *)
  let mysql_data = 
    if return_all_raw_mysql_data then
      Some {
        result_set_field_count = result_set_field_count;
        result_set_extra = extra;
        result_set_field_packets = list_field_packets;
        result_set_row_data_packets = list_raw_data_packets
      } 
    else
      None
  in
  (!server_more_results_exists, 
   { 
     rows = (list_field_names, list_sql_data);
     mysql_data = mysql_data;
   })
