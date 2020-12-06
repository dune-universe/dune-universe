module Uint8_extended = Uint8_extended.Uint8_extended
module Uint16_extended = Uint16_extended.Uint16_extended
module Uint32_extended = Uint32_extended.Uint32_extended
module Uint64_extended = Uint64_extended.Uint64_extended
module Bignum_extended = Bignum_extended.Bignum_extended
module Date_extended = Date_extended.Date_extended
module Date_time_extended = Date_time_extended.Date_time_extended
module Mysql = Mysql
open Core
module Utilities = struct

  let getcon ?(host="127.0.0.1") ~database ~password ~user =
    let open Mysql in 
    quick_connect
      ~host ~database ~password ~user ();;
    
  let getcon_defaults () =
    raise (Failure "Parameterless db connections no longer supported") 
    (*getcon ~host:"127.0.0.1" ~database:"test_model" ~password:"root" ~user:"root";;*)
    
  let closecon c = Mysql.disconnect c;;

  let oc = Core.Out_channel.stdout;;    
  let print_n_flush s =
    let open Core in 
    Out_channel.output_string oc s;
    Out_channel.flush oc;;
    
  let rec print_n_flush_alist ~sep l =
    match l with
    | [] -> ()
    | h :: t ->
       let () = print_n_flush (Core.String.concat [h;sep]) in
       print_n_flush_alist ~sep t;;

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
    let open Core in 
    match field with
    | Some f -> Int.to_string (Float.to_int (f *. 100.0))
    | None -> "NULL";;
  let serialize_float_field_as_int ~field =
    let open Core in 
    Int.to_string (Float.to_int (field *. 100.0));;
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
    (*  "1" -> let () = print_n_flush 
			"\nutilities::parse_boolean_field_exn() 1 returning true" in true
    | "0" -> let () = print_n_flush 
			"\nutilities::parse_boolean_field_exn() 0 returning false" in false*)
    | "YES" -> (*let () = print_n_flush 
			  "\nutilities::parse_boolean_field_exn() YES returning true" in *) true 
(*    | "yes" -> let () = print_n_flush 
			  "\nutilities::parse_boolean_field_exn() yes returning true" in true 
    | "true" -> let () = print_n_flush 
			   "\nutilities::parse_boolean_field_exn() true returning true" in true 
    | "TRUE" -> let () = print_n_flush 
			   "\nutilities::parse_boolean_field_exn() TRUE returning true" in true*)
    | "NO" -> (*let () = print_n_flush 
			 "\nutilities::parse_boolean_field_exn() NO returning false" in *) false 
(*    | "no" -> let () = print_n_flush 
               "\nutilities::parse_boolean_field_exn() no returning false" in false 
    | "false" -> let () = print_n_flush 
               "\nutilities::parse_boolean_field_exn() false returning false" in false
    | "FALSE" -> let () = print_n_flush 
                   "\nutilities::parse_boolean_field_exn() FALSE returning false" in false*)
    | _ -> raise (Failure "Utilities::parse_boolean_field unrecognized value")
    
(*  let parse_optional_boolean_field_exn ~field =
    match field with
    | None -> None
    | Some s ->
       let b = parse_boolean_field_exn ~field:s in
       Some b;;
 *)
(*		  
  let parse_64bit_int_field_exn ~field =
    Core.Int64.of_string field
 *)
  let extract_field_as_string_exn ~fieldname ~results ~arrayofstring =
    try
      String.strip
	~drop:Char.is_whitespace
	(Option.value_exn
	   ~message:("Failed to get col " ^ fieldname)
	   (Mysql.column results
			 ~key:fieldname ~row:arrayofstring))
    with
    | _ ->
       let () = print_n_flush ("\nutilities.ml::extract_field_as_string_exn() failed. \
				most likely bad field name:" ^ fieldname) in
       raise (Failure "utilities.ml::extract_field_as_string_exn() failed. \
		       most likely bad field name")

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
    | Some s -> let i = Int64.of_string s in Some i
    | None -> None;;

  let parse_optional_int32_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Int32.of_string s in Some i
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
    Core.Float.of_string s;;

  let parse_optional_float_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let f = Core.Float.of_string s in Some f
    | None -> None;;
  (*----------------date and time--------------*)
  let parse_date_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
    Core.Date.of_string s;;

  let parse_optional_date_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let dt = Core.Date.of_string s in Some dt
    | None -> None;;

  let parse_datetime_field_exn ~fieldname ~results ~arrayofstring =
    let s = extract_field_as_string_exn ~fieldname ~results ~arrayofstring in
    Core.Time.of_string s;;

  let parse_optional_datetime_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let dt = Core.Time.of_string s in Some dt
    | None -> None;;

end
