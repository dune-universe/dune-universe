module Bignum_extended = Bignum_extended.Bignum_extended
module Date_extended = Date_extended.Date_extended
module Date_time_extended = Date_time_extended.Date_time_extended
module Postgresql = Postgresql
open Core
module Utilities = struct

  let getcon ?(host="127.0.0.1") ~dbname ~password ~user =
    new Postgresql.connection ~host ~dbname ~password ~user ();;

  let getcon_defaults () =
    raise (Failure "Parameterless db connections no longer supported") 
    
  let closecon c = c#finish;;

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
  (*--try not to use these but instead use query parameters*)
  let serialize_optional_field ~field ~(conn:Postgresql.connection) =
    match field with
    | None -> "NULL"
    | Some s -> Core.String.concat ["'" ; (conn#escape_string s) ; "'"];;
  let serialize_optional_field_with_default ~field ~(conn:Postgresql.connection) ~default =
    match field with
    | None -> Core.String.concat ["'" ; (conn#escape_string default) ; "'"]
    | Some s -> Core.String.concat ["'" ; (conn#escape_string s) ; "'"];;
  
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
    | "YES"
      | "t" -> (*let () = print_n_flush 
		 "\nutilities::parse_boolean_field_exn() 1 returning true" in*) true
    | "NO"
      | "f" -> (*let () = print_n_flush 
		 "\nutilities::parse_boolean_field_exn() 0 returning false" in*) false
    | s -> raise (Failure (Core.String.concat["Utilities::parse_boolean_field unrecognized value:";s]))

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
  let extract_field_as_string_exn ~fieldname ~(qresult:Postgresql.result) ~tuple =
    try
      Core.String.strip
	~drop:Core.Char.is_whitespace
	(qresult#getvalue tuple (qresult#fnumber fieldname))
    with
    | _ ->
       let () = print_n_flush
                  (Core.String.concat
                     ["\nutilities.ml::extract_field_as_string_exn() failed, most likely bad field name:";fieldname]) in
       raise (Failure "utilities.ml::extract_field_as_string_exn() failed. Most likely bad field name")

  let extract_optional_field ~fieldname ~(qresult:Postgresql.result) ~tuple =
    if (qresult#getisnull tuple (qresult#fnumber fieldname)) then
      None
    else
      Some (extract_field_as_string_exn ~fieldname ~qresult ~tuple)

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

  let parse_int64_field_exn ~fieldname ~qresult ~tuple =
    try
      let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in
      Core.Int64.of_string s
    with err ->
	 let () = print_n_flush "\nutilities::parse_int64_field_exn() failed" in
	 raise err;;

  let parse_int32_field_exn ~fieldname ~qresult ~tuple =
    try
      let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in
      Core.Int32.of_string s
    with err ->
      let () = print_n_flush "\nutilities::parse_int32_field_exn() failed" in
      raise err;;
    
(*  let parse_optional_int_field_exn ~fieldname ~results ~arrayofstring =
    let s_opt = extract_optional_field ~fieldname ~results ~arrayofstring in
    match s_opt with
    | Some s -> let i = Int.of_string s in Some i
    | None -> None;;*)
    
  let parse_optional_int64_field_exn ~fieldname ~qresult ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let i = Int64.of_string s in Some i
    | None -> None;;

  let parse_optional_int32_field_exn ~fieldname ~qresult ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let i = Int32.of_string s in Some i
    | None -> None;;
    

  let parse_bignum_field_exn ~fieldname ~qresult ~tuple =
    let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in
    Bignum_extended.of_string s;;

  let parse_optional_bignum_field_exn ~fieldname ~(qresult:Postgresql.result) ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let i = Bignum_extended.of_string s in Some i
    | None -> None;;
    
  (*-----booleans------*)
  let parse_bool_field_exn ~fieldname ~qresult ~tuple = 
    let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in 
    parse_boolean_field_exn ~field:s;;

  let parse_optional_bool_field_exn ~fieldname ~qresult ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let b = parse_boolean_field_exn ~field:s in Some b
    | None -> None;;
  (*----------------floats--------------*)
  let parse_float_field_exn ~fieldname ~qresult ~tuple =
    let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in 
    Core.Float.of_string s;;

  let parse_optional_float_field_exn ~fieldname ~qresult ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let f = Core.Float.of_string s in Some f
    | None -> None;;
  (*----------------date and time--------------*)
  let parse_date_field_exn ~fieldname ~qresult ~tuple =
    let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in
    Core.Date.of_string s;;

  let parse_optional_date_field_exn ~fieldname ~qresult ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let dt = Core.Date.of_string s in Some dt
    | None -> None;;

  let parse_datetime_field_exn ~fieldname ~qresult ~tuple =
    let s = extract_field_as_string_exn ~fieldname ~qresult ~tuple in
    Core.Time.of_string s;;

  let parse_optional_datetime_field_exn ~fieldname ~qresult ~tuple =
    let s_opt = extract_optional_field ~fieldname ~qresult ~tuple in
    match s_opt with
    | Some s -> let dt = Core.Time.of_string s in Some dt
    | None -> None;;

end
