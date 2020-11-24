let data_value_to_sql_value field data =
  match data with 
  | Mp_raw_data.Row_data_null -> Mp_data.data_null
  | Mp_raw_data.Row_data_data v -> Mp_data_simple.data_value_to_sql_value v field
  | Mp_raw_data.Row_data_binary v -> Mp_data_binary.data_value_to_sql_value v field

let data_row field_packets list_packets = 
  let num_field = ref 0 in
  let one_packet acc data = 
    let field = List.nth field_packets !num_field in
    let _ = incr num_field in
    let v = data_value_to_sql_value field data in
    v :: acc
  in
  let record = List.fold_left one_packet [] list_packets in
  List.rev record

let data_list field_packets row_data_packets = 
  let row acc list_packets = 
    let r = data_row field_packets list_packets in
    r :: acc
  in
  let records = List.fold_left row [] row_data_packets in
  let records = List.rev records in
  records

let to_type_number d =
  let bitstring_type_number numtype unsigned = 
    let%bitstring v = 
      {| numtype : 8 : int, unsigned, littleendian;
        unsigned : 1 : int;
        0x0 : 7 : int, littleendian |}
    in v
  in
  match d with
    Mp_data.Decimal v -> (* use MySQL Newdecimal type instead of Decimal = 0 *)
    let unsigned = if (Num.sign_num v >= 0) then true else false in
    bitstring_type_number 0xf6 unsigned
  | Mp_data.Tinyint v ->
    let unsigned = if (v > 127) then true else false in
    bitstring_type_number 0x01 unsigned
  | Mp_data.Smallint v ->
    let unsigned = if (v > 32767) then true else false in
    bitstring_type_number 0x02 unsigned
  | Mp_data.Int v -> (* same as Longint *)
    let v = Int64.of_int v in
    let unsigned = 
      Int64.compare v (Int64.of_string "2147483647") > 0
    in
    bitstring_type_number 0x03 unsigned
  | Mp_data.Longint v ->
    let unsigned = 
      Int64.compare v (Int64.of_string "2147483647") > 0
    in
    bitstring_type_number 0x03 unsigned
  | Mp_data.Float _ ->
    bitstring_type_number 0x04 false
  | Mp_data.Double _ ->
    bitstring_type_number 0x05 false
  | Mp_data.Null ->
    bitstring_type_number 0x06 true
  | Mp_data.Timestamp _ ->
    bitstring_type_number 0x07 false
  | Mp_data.Longlongint v ->
    let unsigned = 
      let max = Big_int.sub_big_int (Big_int.power_int_positive_int 2 63) Big_int.unit_big_int in
      Big_int.gt_big_int v max
    in
    bitstring_type_number 0x08 unsigned
  | Mp_data.Int24 v ->
    (* in the enum, the type number is 0x09 but binary encoded int24 is like longint *)
    let unsigned = if (v > 8388607) then true else false in
    bitstring_type_number 0x03 unsigned
  | Mp_data.Date _ ->
    bitstring_type_number 0x0a false
  | Mp_data.Time v ->
    let (sign, _, _, _, _) = v in
    let unsigned = 
      match sign with
      | Mp_data.Positive -> true
      | Mp_data.Negative -> false
    in
    bitstring_type_number 0x0b unsigned
  | Mp_data.Datetime _ ->
    bitstring_type_number 0x0c false
  | Mp_data.Year v ->
    (* in the enum, the type number is 0x0d but binary encoded year is like smallint *)
    let unsigned = if (v >= 0) then true else false in
    bitstring_type_number 2 unsigned
  (* 0x0e is for newdate MySQL type *)
  | Mp_data.Varchar _ -> (* TODO: add Varchar to tests *) 
    bitstring_type_number 0x0f false
  | Mp_data.Bit _ ->
    (* documentation says it should be 0x10 but this doesn't work ! 
       so we send the bitfield as a long long int *)
    bitstring_type_number 0x08 true
  (* 0xf6 is for newdecimal MySQL Type*)
  | Mp_data.Enum _ ->
    bitstring_type_number 0xf7 true
  | Mp_data.Set _ ->
    bitstring_type_number 0xf8 true
  | Mp_data.Blob v -> (
      let length = Buffer.length v in
      let numtype = 
        if (length < 256) then (
          0xf9 (* tiny blob *)
        )
        else if (length < 65536) then (
          0xfc (* blob *)
        )
        else if (length < 16777216) then (
          0xfa (* medium blob *)
        )
        else
          0xfb (* long blob *)
      in
      bitstring_type_number numtype true
    )
  | Mp_data.Varbinary _ ->
    bitstring_type_number 0xfd false
  | Mp_data.Varstring _ ->
    bitstring_type_number 0xfd false
  | Mp_data.Binary _ ->
    bitstring_type_number 0xfe false
  | Mp_data.String _ ->
    bitstring_type_number 0xfe false
  | Mp_data.Geometry _ -> (* opaque type *)
    bitstring_type_number 0xff false

let to_bitstring_longlongint v =
  (* function to convert positive integer to 64 bits *)
  let decimal_to_binary decimal =
    let bit = Bitstring.create_bitstring 64 in
    let pos = ref 63 in
    let d = ref decimal in
    let () =
      let two = Big_int.big_int_of_int 2 in
      while (not (Big_int.eq_big_int !d Big_int.zero_big_int)) do
        let remainder = Big_int.mod_big_int !d two in
        let () =
          if (Big_int.eq_big_int remainder Big_int.unit_big_int) then
            Bitstring.set bit !pos
        in
        let () = d := Big_int.div_big_int !d two in
        decr pos
      done
    in
    bit
  in
  try
    let v_int64 = Big_int.int64_of_big_int v in
    let max = Big_int.sub_big_int (Big_int.power_int_positive_int 2 63) Big_int.unit_big_int in
    if (Big_int.gt_big_int v max) then
      let%bitstring v = 
        {| v_int64 : 8*8 : int, unsigned, littleendian |}
      in v
    else
      let%bitstring v = 
        {| v_int64 : 8*8 : int, littleendian |}
      in v
  with
  | Failure _ -> ( (* the big integer is > 2^63 - 1*)
      let max = Big_int.sub_big_int (Big_int.power_int_positive_int 2 64) Big_int.unit_big_int in
      if (Big_int.gt_big_int v max) then
        failwith "Bigint overflow ( > 2^64 - 1)"
      else
        let v_int64 = decimal_to_binary v in
        let%bitstring v = 
          {| v_int64 : 8*8 : bitstring |}
        in v
    )

let to_bitstring d =
  let int_longint v =
    let v =
      if (Int64.compare v Int64.zero < 0) then
        Int64.add v (Int64.of_string "4294967296")
      else
        v
    in
    if (Int64.compare v (Int64.of_string "2147483647") > 0) then (
      let%bitstring v =
        {| v : Mp_bitstring.compute32 : int, unsigned, littleendian |}
      in v
    )
    else (
      let%bitstring v =
        {| v : Mp_bitstring.compute32 : int, littleendian |}
      in v
    )
  in
  let datetime_timestamp v =
    let ((year, month, day), (hour, min, sec, subsecond)) = v in
    (* first byte length is static = 2 + 1 + 1 + 1 + 1 + 1 + 4 *)
    let%bitstring v =
      {| 11 : 1 * 8 : int, littleendian;
        year : 2 * 8 : int, littleendian;
        month : 1 * 8 : int, littleendian;
        day : 1 * 8 : int, littleendian;
        hour : 1 * 8 : int, littleendian;
        min : 1 * 8 : int, littleendian;
        sec : 1 * 8 : int, littleendian;
        subsecond : Mp_bitstring.compute32 : int, littleendian
      |}
    in v
  in
  let varchar_varstring_string_enum_set v =
    let length = String.length v in
    let bitstring_length = Mp_binary.build_length_coded_binary length in
    if (length > 0) then (
      let bitstring_data =
        let%bitstring v =
          {| v : length * 8 : string |}
        in v
      in
      Bitstring.concat [bitstring_length; bitstring_data]
    ) else (
      Bitstring.concat [bitstring_length]
    )
  in
  let binary_varbinary_blob v =
    let length = Buffer.length v in
    let v = Buffer.contents v in
    let bitstring_length = Mp_binary.build_length_coded_binary length in
    if (length > 0) then (
      let bitstring_data =
        let%bitstring v =
          {| v : length * 8 : string |}
        in v
      in
      Bitstring.concat [bitstring_length; bitstring_data]
    ) else (
      Bitstring.concat [bitstring_length]
    )
  in
  match d with
    Mp_data.Null -> Bitstring.empty_bitstring
  | Mp_data.Tinyint v -> 
    let v = 
      if (v < 0) then
        v + 256
      else 
        v
    in
    if (v > 127) then (
      let%bitstring v = 
        {| v : 1*8 : int, unsigned |}
      in v
    )
    else (
      let%bitstring v = 
        {| v : 1*8 : int |}
      in v
    )
  | Mp_data.Smallint v ->
    let v = 
      if (v < 0) then
        v + 65536
      else 
        v
    in
    if (v > 32767) then (
      let%bitstring v = 
        {| v : 2*8 : int, unsigned, littleendian |}
      in v
    )
    else (
      let%bitstring v = 
        {| v : 2*8 : int, littleendian |}
      in v
    )
  | Mp_data.Int v ->
    let v = Int64.of_int v in
    int_longint v
  | Mp_data.Longint v -> 
    int_longint v
  | Mp_data.Longlongint v ->
    to_bitstring_longlongint v
  | Mp_data.Decimal v -> 
    let s = Num.approx_num_fix 20 v in (* /!\ : how to get the right required precision ? *)
    let length = String.length s in 
    let bitstring_length = Mp_binary.build_length_coded_binary length in
    if (length > 0) then (
      let bitstring_data = 
        let%bitstring v = 
          {| s : length * 8 : string |}
        in v
      in
      Bitstring.concat [bitstring_length; bitstring_data]
    ) else (
      Bitstring.concat [bitstring_length]
    )
  | Mp_data.Date v -> 
    let (year, month, day) = v in
    (* first byte length is static = 2 + 1 + 1 *)
    let%bitstring v = 
      {| 4 : 1 * 8 : int, unsigned, littleendian;
        year : 2 * 8 : int, unsigned, littleendian;
        month : 1 * 8 : int, unsigned, littleendian;
        day : 1 * 8 : int, unsigned, littleendian
      |}
    in v
  | Mp_data.Time v ->
    let (sign, hour, min, sec, subsecond) = v in
    let bitstring_sign = 
      match sign with 
      | Mp_data.Negative -> 1
      | Mp_data.Positive -> 0
    in
    let (day, hour) = 
      if (hour > 24) then
        (Int32.of_int (hour / 24), hour mod 24)
      else (Int32.zero, hour)
    in
    (* first byte length is static = 1 + 4 + 1 + 1 + 1 + 4 *)
    let%bitstring v =
      {| 12 : 1 * 8 : int, unsigned, littleendian;
        bitstring_sign : 1 * 8 : int, unsigned, littleendian;
        day : 4 * 8 : int, littleendian;
        hour : 1 * 8 : int, littleendian;
        min : 1 * 8 : int, littleendian;
        sec : 1 * 8 : int, littleendian;
        subsecond : Mp_bitstring.compute32 : int, littleendian
      |}
    in v
  | Mp_data.Datetime v -> datetime_timestamp v
  | Mp_data.Timestamp v -> datetime_timestamp v
  | Mp_data.Float v -> 
    let v = Int32.bits_of_float v in
    let%bitstring v =
      {| v : 4 * 8 : int, littleendian |}
    in v
  | Mp_data.Double v -> 
    let v = Int64.bits_of_float v in
    let%bitstring v =
      {| v : 8 * 8 : int, littleendian |}
    in v
  | Mp_data.Int24 v -> 
    (* byte padding is different for positive and negative integer *)
    let (v, padding) = 
      if (v < 0) then
        (v + 16777216, 0xff)
      else 
        (v, 0x00)
    in
    (* 4 bytes with 0x00 or 0xff for the last one *)
    if (v > 8388607) then (
      let%bitstring v = 
        {| v : 3*8 : int, unsigned, littleendian;
          padding : 1*8 : int, unsigned, littleendian |}
      in v
    )
    else (
      let%bitstring v = 
        {| v : 3*8 : int, littleendian;
          padding : 1*8 : int, littleendian |}
      in v
    )
  | Mp_data.Year v -> 
    if (v >= 0) then (
      let%bitstring v = 
        {| v : 2*8 : int, unsigned, littleendian |}
      in v
    )
    else (
      let%bitstring v = 
        {| v : 2*8 : int, littleendian |}
      in v
    )
  | Mp_data.Varchar v -> (* TODO: add Varchar to tests *)
    varchar_varstring_string_enum_set v
  | Mp_data.Varstring v ->
    varchar_varstring_string_enum_set v
  | Mp_data.String v ->
    varchar_varstring_string_enum_set v
  | Mp_data.Enum v ->
    varchar_varstring_string_enum_set v
  | Mp_data.Set v ->
    varchar_varstring_string_enum_set v
  | Mp_data.Binary v ->
    binary_varbinary_blob v
  | Mp_data.Varbinary v ->
    binary_varbinary_blob v
  | Mp_data.Blob v ->
    binary_varbinary_blob v
  | Mp_data.Bit v -> (* send an unsigned 64 bit long long integer ?? *)
    let bits = 
      match%bitstring v with
      | {| v_int64 : 8*8 : int, unsigned |} ->
          let%bitstring v =
            {| v_int64 : 8*8 : int, unsigned, littleendian |}
          in v
    in
    bits
  | Mp_data.Geometry v -> (* opaque type: send as is *)
    v
