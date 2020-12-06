module Date_time_extended = Date_time_extended.Date_time_extended
module Date_extended = Date_extended.Date_extended
module Uint8_extended = Uint8_extended.Uint8_extended
module Uint16_extended = Uint16_extended.Uint16_extended
(*Need to switch to use of stdint over uint library to support 24 bit type
module Uint24_extended = Uint24_extended.Uint24_extended*)
module Uint32_extended = Uint32_extended.Uint32_extended
module Uint64_extended = Uint64_extended.Uint64_extended
module Bignum_extended = Bignum_extended.Bignum_extended
module Mysql = Mysql
module Utilities = struct
  let print_n_flush s =
    Printf.printf "%s" s

  (*Client code makefile supplies credentials and uses this function; credentials in client
   projects are stored in credentials.ml; this file is copied with modifications
   to make of type () -> Mysql.dbd with credentials optional with default values.*)
  let getcon ?(host="127.0.0.1") ~database ~password ~user =
    let open Mysql in 
    quick_connect
      ~host ~database ~password ~user ();;

  let closecon c = Mysql.disconnect c;;
  let parse_list s =
    try
      match s with
      | Some sl ->
	 (try
	     (*let () = print_n_flush ("parse_list() from " ^ sl) in*)
	     let l = String.split_on_char ',' sl in
	     let len = List.length l in
	     if len > 0 then Some l else None
	   with
	   | err ->
	      (*let () = print_n_flush
			 "\nFailed parsing table name list..." in*)
	      raise err
	 )
      | None -> None
    with
    | _ -> None;;

  let is_suffix s suffix =
    let l = String.length s in
    let suffixl = String.length suffix in
    if suffixl >= l then
      false
    else
      if suffixl = 0 then
	false
      else
	let sample = String.sub s (l - suffixl) suffixl in
	String.equal sample suffix
	  
    
  let serialize_optional_field ~field ~conn =
    match field with
    | None -> "NULL"
    | Some s -> "'" ^ (Mysql.real_escape conn s) ^ "'";;
  let serialize_optional_field_with_default ~field ~conn ~default =
    match field with
    | None -> default
    | Some s -> "'" ^ (Mysql.real_escape conn s) ^ "'";;
  let serialize_boolean_field ~field =
    match field with
    | true -> "TRUE"
    | false -> "FALSE";;
  let serialize_optional_bool_field ~field =
    match field with
    | None -> "NULL"
    | Some b -> if b then "TRUE" else "FALSE";;
  let serialize_optional_float_field_as_int ~field =
    match field with
    | Some f -> string_of_int (int_of_float (f *. 100.0))
    | None -> "NULL";;
  let serialize_float_field_as_int ~field =
    string_of_int (int_of_float (field *. 100.0));;
  let serialize_optional_date_field ~field =
    match field with
    | Some d -> "'" ^ (Date_extended.to_string d) ^ "'"
    | None -> "NULL";;
  let serialize_optional_date_time_field ~field =
    match field with
    | Some dt -> "'" ^ (Date_time_extended.to_string dt) ^ "'"
    | None -> "NULL";;
  (*===========parsers=============*)
  let parse_boolean_field_exn ~field =
    match field with
    | "YES" -> true 
    | "NO" -> false 
    | _ -> raise (Failure "Utilities::parse_boolean_field unrecognized value")
(*    
  let parse_optional_boolean_field_exn ~field =
    match field with
    | None -> None
    | Some s ->
       let b = parse_boolean_field_exn ~field:s in
       Some b;;
    *)
  let is_digit c =
    let codepoint = Char.code c in 
    codepoint > 47 && codepoint < 58
    
  (*For use with String.map (fun c -> if is_whitespace_char c then '' else c) s *)
  let is_whitespace_char c =
    let codepoint = Char.code c in 
    not (codepoint > 32 && codepoint < 127)
    
  let extract_field_as_string_exn ~fieldname ~results ~arrayofstring =
      let s_opt = (Mysql.column results
				~key:fieldname ~row:arrayofstring) in
      match s_opt with
      | Some s -> String.trim s
      | None ->
	 let () = print_n_flush ("\nutilities.ml::extract_field_as_string_exn() failed. \
				  most likely bad field name:" ^ fieldname) in
	 raise (Failure "utilities.ml::extract_field_as_string_exn() failed. \
			 most likely bad field name");;

  let extract_optional_field ~fieldname ~results ~arrayofstring =
    Mysql.column results ~key:fieldname ~row:arrayofstring;;
    (*
  let parse_int_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    Int.of_string s;;
    
  let parse_optional_int_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Int.of_string s in Some i
    | None -> None;;
     *)

  let parse_int64_field_exn ~fieldname ~results ~arrayofstring =
    try
      let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
      Core.Int64.of_string s
    with err ->
	 let () = print_n_flush "\nutilities::parse_int64_field_exn() failed" in
	 raise err;;

  let parse_int32_field_exn ~fieldname ~results ~arrayofstring =
    try
      let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
      Core.Int32.of_string s
    with err ->
      let () = print_n_flush "\nutilities::parse_int32_field_exn() failed" in
      raise err;;
    
(*  let parse_optional_int_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Int.of_string s in Some i
    | None -> None;;*)
    
  let parse_optional_int64_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Core.Int64.of_string s in Some i
    | None -> None;;

  let parse_optional_int32_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Core.Int32.of_string s in Some i
    | None -> None;;
    
  let parse_uint8_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    Uint8_extended.of_string s;;

  let parse_optional_uint8_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Uint8_extended.of_string s in Some i
    | None -> None;;

  let parse_uint16_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    Uint16_extended.of_string s;;

  let parse_optional_uint16_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Uint16_extended.of_string s in Some i
    | None -> None;;
(*
  let parse_uint24_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    Uint24_extended.of_string s;;

  let parse_optional_uint24_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Uint24_extended.of_string s in Some i
    | None -> None;;
 *)
  let parse_uint32_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    Uint32_extended.of_string s;;

  let parse_optional_uint32_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Uint32_extended.of_string s in Some i
    | None -> None;;

  let parse_uint64_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    Uint64_extended.of_string s;;

  let parse_optional_uint64_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Uint64_extended.of_string s in Some i
    | None -> None;;

    
  let parse_bignum_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
    Bignum_extended.of_string s;;

  let parse_optional_bignum_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Bignum_extended.of_string s in Some i
    | None -> None;;
    
  (*-----booleans------*)
  let parse_bool_field_exn ~fieldname ~results ~arrayofstring = 
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    parse_boolean_field_exn ~field:s;;

  let parse_optional_bool_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let b = parse_boolean_field_exn ~field:s in Some b
    | None -> None;;
  (*----------------floats--------------*)
  let parse_float_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in 
    float_of_string s;;

  let parse_optional_float_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let f = float_of_string s in Some f
    | None -> None;;
  (*----------------date and time--------------*)
  let parse_date_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
    Date_extended.of_string_exn s;;

  let parse_optional_date_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let dt = Date_extended.of_string_exn s in Some dt
    | None -> None;;

  let parse_datetime_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
    Date_time_extended.of_string s;;

  let parse_optional_datetime_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let dt = Date_time_extended.of_string s in Some dt
    | None -> None;;

end
