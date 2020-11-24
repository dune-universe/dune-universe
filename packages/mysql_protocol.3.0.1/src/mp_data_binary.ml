let case_datetime_timestamp_date field_type v =
  let length = Bitstring.bitstring_length v in
  let parts =
    if (length = 0) then
      ((0, 0, 0), (0, 0, 0, Int64.zero))
    else
      match%bitstring v with
      | {| year : 2 * 8 : int, unsigned, littleendian;
            rest : length - (2*8) : bitstring |} -> (
          if (Bitstring.bitstring_length rest > 0) then
            let length_rest = (Bitstring.bitstring_length rest) - (2*8) in
            match%bitstring rest with
            | {| month : 1 * 8 : int, unsigned, littleendian;
                day : 1 * 8 : int, unsigned, littleendian;
                rest : length_rest : bitstring |} -> (
                  if (Bitstring.bitstring_length rest > 0) then
                    let length_rest = (Bitstring.bitstring_length rest) - (3*8) in
                    match%bitstring rest with
                    | {| hour : 1 * 8 : int, unsigned, littleendian;
                        min : 1 * 8 : int, unsigned, littleendian;
                        sec : 1 * 8 : int, unsigned, littleendian;
                        rest : length_rest : bitstring |} -> (
                          if (Bitstring.bitstring_length rest > 0) then
                            match%bitstring rest with
                            | {| subsecond : Mp_bitstring.compute32 : int, unsigned, littleendian |} -> (
                                ((year, month, day), (hour, min, sec, subsecond))
                              )
                          else
                            ((year, month, day), (hour, min, sec, Int64.zero))
                        )
                  else
                    ((year, month, day), (0, 0, 0, Int64.zero))
                )
          else
            ((year, 0, 0), (0, 0, 0, Int64.zero))
          )
  in
  let ((year, month, day), (hour, min, sec, subsec)) = parts in
  match field_type with
  | Mp_field_packet.Field_type_datetime -> Mp_data.data_datetime ((year, month, day), (hour, min, sec, subsec))
  | Mp_field_packet.Field_type_timestamp -> Mp_data.data_timestamp ((year, month, day), (hour, min, sec, subsec))
  | Mp_field_packet.Field_type_date -> Mp_data.data_date (year, month, day)
  | _ -> assert false

let data_value_to_sql_value_date_time_types field_type v =
  match field_type with
  | Mp_field_packet.Field_type_datetime -> case_datetime_timestamp_date field_type v
  | Mp_field_packet.Field_type_timestamp -> case_datetime_timestamp_date field_type v
  | Mp_field_packet.Field_type_date -> case_datetime_timestamp_date field_type v
  | Mp_field_packet.Field_type_time -> (
      let length = Bitstring.bitstring_length v in
      if (length = 0) then
        Mp_data.data_time (Mp_data.Positive, 0, 0, 0, Int64.zero)
      else
        match%bitstring v with
        | {| sign : 1 * 8 : int, unsigned, littleendian;
            rest : length - 8 : bitstring |} -> (
              let pos_or_neg = 
                match sign with 
                | 1 -> Mp_data.Negative
                | 0 -> Mp_data.Positive
                | _ -> assert false
              in
              if (Bitstring.bitstring_length rest > 0) then
                let length_rest = (Bitstring.bitstring_length rest) - Mp_bitstring.compute32 in
                match%bitstring rest with
                | {| day : Mp_bitstring.compute32 : int, unsigned, littleendian;
                    rest : length_rest : bitstring |} -> (
                      let hour_day = Int64.mul day (Int64.of_int 24) in
                      (* cast should be ok, documentation says : 
                         			     "TIME values may range from '-838:59:59' to '838:59:59'" *)
                      let hour_day = Int64.to_int hour_day in
                      if (Bitstring.bitstring_length rest > 0) then
                        let length_rest = (Bitstring.bitstring_length rest) - (3*8) in
                        match%bitstring rest with
                        | {| hour : 1 * 8 : int, unsigned, littleendian;
                            min : 1 * 8 : int, unsigned, littleendian;
                            sec : 1 * 8 : int, unsigned, littleendian;
                            rest : length_rest : bitstring |} -> (
                              if (Bitstring.bitstring_length rest > 0) then
                                match%bitstring rest with
                                | {| subsecond : Mp_bitstring.compute32 : int, unsigned, littleendian |} -> (
                                    Mp_data.data_time (pos_or_neg, hour + hour_day, min, sec, subsecond)
                                  )
                              else
                                Mp_data.data_time (pos_or_neg, hour + hour_day, min, sec, Int64.zero)
                            )
                      else
                        Mp_data.data_time (pos_or_neg, 0 + hour_day, 0, 0, Int64.zero)
                    )
              else
                Mp_data.data_time (pos_or_neg, 0, 0, 0, Int64.zero)
            )
    )
  | _ -> assert false

let data_value_to_sql_value v field =
  let field_type = field.Mp_field_packet.field_type in
  match field_type with
  (* /!\ : should not happen because null values are sent with the null bitfield *)
  | Mp_field_packet.Field_type_null -> Mp_data.data_null
  | Mp_field_packet.Field_type_longlong -> (
      match%bitstring v with
      | {| d : 8 * 8 : int, littleendian |} ->
        let field_flags = field.Mp_field_packet.field_flags in
        let bi = Big_int.big_int_of_int64 d in
        let bi = 
          if ( (List.mem Mp_field_packet.Field_flag_unsigned field_flags)
               && (Big_int.sign_big_int bi = -1) ) then
            Big_int.add_big_int (Big_int.power_int_positive_int 2 64) bi
          else
            bi
        in
        Mp_data.data_longlongint bi
    )
  | Mp_field_packet.Field_type_long -> (
      let field_flags = field.Mp_field_packet.field_flags in
      match%bitstring v with
      | {| d : Mp_bitstring.compute32 : int, littleendian |} ->
        if (List.mem Mp_field_packet.Field_flag_unsigned field_flags) then
          Mp_data.data_longint d
        else
          if (Int64.compare d (Int64.of_string "2147483647") > 0) then
            Mp_data.data_longint (Int64.sub d (Int64.of_string "4294967296"))
          else
            Mp_data.data_longint d
    )
  | Mp_field_packet.Field_type_short -> (
      let field_flags = field.Mp_field_packet.field_flags in
      match%bitstring v with
      | {| d : 2 * 8 : int, littleendian |} ->
        if (List.mem Mp_field_packet.Field_flag_unsigned field_flags) then
          Mp_data.data_smallint d
        else
          if (d > 32767) then
            Mp_data.data_smallint (d - 65536)
          else
            Mp_data.data_smallint d
    )
  | Mp_field_packet.Field_type_tiny -> (
      let field_flags = field.Mp_field_packet.field_flags in
      match%bitstring v with
      | {| d : 1 * 8 : int, littleendian |} ->
        if (List.mem Mp_field_packet.Field_flag_unsigned field_flags) then
          Mp_data.data_tinyint d
        else
          if (d > 127) then 
            Mp_data.data_tinyint (d - 256)
          else
            Mp_data.data_tinyint d
    )
  | Mp_field_packet.Field_type_float -> (
      match%bitstring v with
      | {| d : 4 * 8 : int, littleendian |} ->
        Mp_data.data_float (Int32.float_of_bits d)
    )
  | Mp_field_packet.Field_type_double -> (
      match%bitstring v with
      | {| d : 8 * 8 : int, littleendian |} ->
        Mp_data.data_double (Int64.float_of_bits d)
    )
  | Mp_field_packet.Field_type_int24 -> (
      let field_flags = field.Mp_field_packet.field_flags in
      (* 4 bytes with 0x00 or 0xff for the last one 
         	 so we only need the first 3 bytes 
      *)
      match%bitstring v with
      | {| d : 3 * 8 : int, littleendian |} ->
        if (List.mem Mp_field_packet.Field_flag_unsigned field_flags) then
          Mp_data.data_int24 d
        else
          if (d > 8388607) then 
            Mp_data.data_int24 (d - 16777216)
          else
            Mp_data.data_int24 d
    )
  | Mp_field_packet.Field_type_year -> (
      match%bitstring v with
      | {| d : 2 * 8 : int, littleendian |} ->
          Mp_data.data_year d
    )
  | Mp_field_packet.Field_type_newdecimal -> (
      let length = Bitstring.bitstring_length v in
      let nb_bytes = length / 8 in
      match%bitstring v with
      | {| d : length : string |} ->
          let decimals = field.Mp_field_packet.field_decimals in
          let part_i_s = String.sub d 0 (nb_bytes - 1 - decimals) in
          let part_d_s = String.sub d (nb_bytes - decimals) decimals in
          let i = part_i_s ^ part_d_s in
          let i = Big_int.big_int_of_string i in
          let i = Num.num_of_big_int i in
          let div = Big_int.power_int_positive_int 10 decimals in
          let div = Num.num_of_big_int div in
          Mp_data.data_decimal (Num.div_num i div)
    )
  | Mp_field_packet.Field_type_string -> (
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : string |} -> (
          let field_flags = field.Mp_field_packet.field_flags in
          if (List.mem Mp_field_packet.Field_flag_enum field_flags) then 
            Mp_data.data_enum d
          else if (List.mem Mp_field_packet.Field_flag_set field_flags) then
            Mp_data.data_set d
          else if (List.mem Mp_field_packet.Field_flag_binary field_flags) then
            let b = Buffer.create (String.length d) in
            let () = Buffer.add_string b d in
            Mp_data.data_binary b
          else
            Mp_data.data_string d
        )
    )
  (* /!\ : should not happen because set is sent as a string *)
  | Mp_field_packet.Field_type_set -> (
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : string |} -> 
          Mp_data.data_set d
    )
  (* /!\ : should not happen because enum is sent as a string *)
  | Mp_field_packet.Field_type_enum -> (
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : string |} ->
          Mp_data.data_enum d
    )
  | Mp_field_packet.Field_type_var_string -> (
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : string |} -> 
        let field_flags = field.Mp_field_packet.field_flags in
        if (List.mem Mp_field_packet.Field_flag_binary field_flags) then
          let b = Buffer.create (String.length d) in
          let () = Buffer.add_string b d in
          Mp_data.data_varbinary b
        else
          Mp_data.data_varstring d
    )
  | Mp_field_packet.Field_type_varchar -> (
      (* TODO: add varchar to tests *)
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : string |} -> Mp_data.data_varchar d
    )
  | Mp_field_packet.Field_type_bit -> (
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : bitstring |} -> Mp_data.data_bit d
    )
  | Mp_field_packet.Field_type_tiny_blob
  | Mp_field_packet.Field_type_medium_blob
  | Mp_field_packet.Field_type_long_blob
  | Mp_field_packet.Field_type_blob -> (
      let length = Bitstring.bitstring_length v in
      match%bitstring v with
      | {| d : length : string |} ->
        let b = Buffer.create length in
        let () = Buffer.add_string b d in
        Mp_data.data_blob b
    )
  | Mp_field_packet.Field_type_geometry -> (* opaque type *)
      Mp_data.data_geometry v
  | Mp_field_packet.Field_type_datetime 
  | Mp_field_packet.Field_type_timestamp 
  | Mp_field_packet.Field_type_date 
  | Mp_field_packet.Field_type_time ->
      data_value_to_sql_value_date_time_types field_type v
