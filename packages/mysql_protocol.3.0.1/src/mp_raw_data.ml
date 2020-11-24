
type row_data = 
  | Row_data_data of string
  | Row_data_null
  | Row_data_binary of Bitstring.t

let row_data_to_string p = 
  let v = 
    match p with
    | Row_data_data d -> d
    | Row_data_null -> "NULL"
    | Row_data_binary b -> Bitstring.string_of_bitstring b
  in
  Printf.sprintf "row_data : %s\n" v

let binary data field_packet = 
  let field_type = field_packet.Mp_field_packet.field_type in
  let (length_bits, data) = 
    match field_type with
    | Mp_field_packet.Field_type_tiny -> (Int64.of_int (1 * 8), data)
    | Mp_field_packet.Field_type_short 
    | Mp_field_packet.Field_type_year -> (Int64.of_int (2 * 8), data)
    | Mp_field_packet.Field_type_float
    | Mp_field_packet.Field_type_long -> (Int64.of_int (4 * 8), data)
    | Mp_field_packet.Field_type_int24 -> (Int64.of_int (4 * 8), data) (* 4 bytes with 0x00 or 0xff for the last one *)
    | Mp_field_packet.Field_type_double
    | Mp_field_packet.Field_type_longlong -> (Int64.of_int (8 * 8), data)
    | Mp_field_packet.Field_type_newdecimal 
    | Mp_field_packet.Field_type_datetime
    | Mp_field_packet.Field_type_time
    | Mp_field_packet.Field_type_date
    | Mp_field_packet.Field_type_timestamp 
    | Mp_field_packet.Field_type_string
    | Mp_field_packet.Field_type_var_string 
    | Mp_field_packet.Field_type_blob
    | Mp_field_packet.Field_type_long_blob
    | Mp_field_packet.Field_type_medium_blob
    | Mp_field_packet.Field_type_tiny_blob
    | Mp_field_packet.Field_type_bit 
    | Mp_field_packet.Field_type_geometry -> (
        let (l, d) = Mp_binary.length_coded_binary data in
        (Int64.mul l (Int64.of_int 8), d)
      )
    | _ -> assert false
  in
  let length_rest = (Bitstring.bitstring_length data) - (Int64.to_int length_bits) in
  match%bitstring data with
  | {| value : (Int64.to_int length_bits) : bitstring;
    rest : length_rest : bitstring |} -> (value, rest)

let raw_data_packet_binary list_field_packet list_null_fields bits =
  let nb_columns = List.length list_field_packet in
  let count_columns = ref 0 in
  let data = ref bits in
  let l = ref [] in
  let () = 
    while (Bitstring.bitstring_length !data > 0 || !count_columns < nb_columns) do
      let (v, rest) = 
        if (List.length list_null_fields > 0) then 
          if (List.nth list_null_fields !count_columns) then
            let () = incr count_columns in
            (Row_data_null, !data)
          else
            let (v, rest) = binary !data (List.nth list_field_packet !count_columns) in
            let () = incr count_columns in
            (Row_data_binary v, rest)
        else
          let (v, rest) = binary !data (List.nth list_field_packet !count_columns) in
          let () = incr count_columns in
          (Row_data_binary v, rest)
      in
      let () = l := v :: !l in
      data := rest
    done
  in
  List.rev !l

let null_bytes bits = 
  let nb_bytes = (Bitstring.bitstring_length bits) / 8 in
  if (nb_bytes > 0) then (
    let byte b l = 
      match%bitstring b with
      | {| bit0 : 1 : int;
          bit1 : 1 : int;
          bit2 : 1 : int;
          bit3 : 1 : int;
          bit4 : 1 : int;
          bit5 : 1 : int;
          bit6 : 1 : int;
          bit7 : 1 : int |} -> (
            l := bit7 :: !l;
            l := bit6 :: !l;
            l := bit5 :: !l;
            l := bit4 :: !l;
            l := bit3 :: !l;
            l := bit2 :: !l;
            l := bit1 :: !l;
            l := bit0 :: !l;
          )
    in
    let l = ref [] in
    let () = 
      for i = 0 to (nb_bytes - 1) do
        byte (Bitstring.subbitstring bits (i * 8) 8) l 
      done
    in
    List.rev !l
  )
  else (
    []
  )

let raw_data_packet list_field_packet type_sent count_rows bits =
  let binary_encoding = ref false in
  let bits =
    match type_sent with
    | Mp_com.Fetch ->
      if (count_rows > 0) then
        if (Bitstring.bitstring_length bits > 0) then (
          let length_rest = (Bitstring.bitstring_length bits) - 8 in
          match%bitstring bits with
          | {| test : 1*8 : int, unsigned;
              rest : length_rest : bitstring |} -> (
                if (test = 0) then
                  let () = binary_encoding := true in
                  rest
                else
                  bits
              )
        ) else
          bits
      else
        let () = binary_encoding := true in
        bits
    | _ ->
      if (Bitstring.bitstring_length bits > 0) then (
        let length_rest = (Bitstring.bitstring_length bits) - 8 in
        match%bitstring bits with
        | {| test : 1*8 : int, unsigned;
            rest : length_rest : bitstring |} -> (
              if (test = 0) then
                let () = binary_encoding := true in
                rest
              else
                bits
            )
      ) else
        bits
  in
  if (!binary_encoding) then (
    let nb_null_bits = ((((List.length list_field_packet) + 7 + 2) / 8) * 8) in
    let length_rest = (Bitstring.bitstring_length bits) - nb_null_bits in
    match%bitstring bits with
    | {| null_bits : nb_null_bits : bitstring;
        rest : length_rest : bitstring |} -> (
          let list_null_fields = null_bytes null_bits in
          (* the first two bits are reserved *)
          let list_null_fields = 
            match list_null_fields with
            | [] -> []
            | _ :: _ :: l -> l
            | _ -> assert false
          in
          raw_data_packet_binary list_field_packet list_null_fields rest
        )
  ) else (
    let data = ref bits in
    let l = ref [] in
    let () =
      while (Bitstring.bitstring_length !data > 0) do
        let null_value = ref false in
        let () =
          if (Bitstring.bitstring_length !data >= 8) then (
            match%bitstring !data with
            | {| test : 1*8 : int, unsigned, bigendian |} -> (
                if (test = 251) then 
                  null_value := true
              )
          )
        in
        let (v, rest) =
          if !null_value then (
            let length_rest = (Bitstring.bitstring_length !data) - 8 in
            match%bitstring !data with
            | {| _ : 1*8 : int, unsigned, bigendian;
                rest : length_rest : bitstring |} -> (Row_data_null, rest)
          ) else (
            let (v, rest) = Mp_string.length_coded_string !data in
            (Row_data_data v, rest)
          )
        in
        let () = l := v :: !l in
          data := rest
      done
    in
    List.rev !l
  )
