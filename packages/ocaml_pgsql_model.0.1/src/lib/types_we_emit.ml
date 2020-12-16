module Types_we_emit = struct
  type t =
    | Bignum
    | CoreInt32
    | CoreInt64
    | Float
    | Date
    | Time
    (*===TODO===for the really paranoid; introduce a type that extends string and is length aware, 
      and never permits truncation when storing to the db, although would have to handle runtime exceptions*)
    | String
    | Bool
	[@@deriving show]
(*Return a string we can use in writing a module that is a type.*)
  let to_string ~t ~is_nullable =
    if is_nullable then
      match t with
      | Bignum -> "Bignum_extended.t option"
      | CoreInt64 -> "CoreInt64_extended.t option"
      | CoreInt32 -> "CoreInt32_extended.t option"
      | Float -> "Core.Float.t option"
      | Date -> "Date_extended.t option"
      | Time -> "Date_time_extended.t option"
      | String -> "string option"
      | Bool -> "bool option"
    else 
      match t with
      | Bignum -> "Bignum_extended.t"
      | CoreInt64 -> "CoreInt64_extended.t"
      | CoreInt32 -> "CoreInt32_extended.t"
      | Float -> "Core.Float.t"
      | Date -> "Date_extended.t"
      | Time -> "Date_time_extended.t"
      | String -> "string"
      | Bool -> "bool";;
		
  (**
   is_optional - is the field, of whatever type, optional in the type t of the module and nullable in the db?
   t - the type of the field   *)
  let converter_of_string_of_type ~is_optional ~t ~fieldname =
    let open Core in 
    match is_optional, t with
      false, String ->
      String.concat ["Utilities.extract_field_as_string_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, String ->
       String.concat ["Utilities.extract_optional_field ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, Bool ->
       String.concat ["Utilities.parse_bool_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, Bool ->
       String.concat ["Utilities.parse_optional_bool_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, CoreInt32 ->
       String.concat ["Utilities.parse_int32_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, CoreInt32 ->
       String.concat ["Utilities.parse_optional_int32_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, CoreInt64 ->
       String.concat ["Utilities.parse_int64_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, CoreInt64 ->
       String.concat ["Utilities.parse_optional_int64_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, Bignum -> String.concat ["Utilities.parse_optional_bignum_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, Bignum -> String.concat ["Utilities.parse_bignum_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, Float -> String.concat ["Utilities.parse_float_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, Float -> String.concat ["Utilities.parse_optional_float_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, Date -> String.concat ["Utilities.parse_date_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, Date -> String.concat ["Utilities.parse_optional_date_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | false, Time -> String.concat ["Utilities.parse_datetime_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]
    | true, Time -> String.concat ["Utilities.parse_optional_datetime_field_exn ~fieldname:\"";fieldname;"\" ~qresult ~tuple:tuple_number"]

  (**  is_optional - is the field, of whatever type, optional in the type t of the module and nullable in the db?
   t - the type of the field   *)
  let converter_to_string_of_type ~is_optional ~t =
    let open Core in 
    match is_optional, t with
      false, String ->
      String.concat ["(conv (fun x -> \"'\" ^ (Mysql.real_escape conn x) ^ \"'\"))"]
    | true, String ->
       String.concat ["(conv (fun x -> Utilities.serialize_optional_field ~field:x ~conn))"]
    | false, Bool ->
       String.concat ["(conv (fun x -> if x then \"TRUE\" else \"FALSE\"))"]
    | true, Bool ->
       String.concat ["(conv (fun x -> Utilities.serialize_optional_bool_field ~field:x))"]
    | false, CoreInt32 ->
       String.concat ["(conv (fun x -> Core.Int32.to_string x))"]
    | true, CoreInt32 ->
       String.concat ["(conv (fun x -> match x with None -> \"NULL\" | Some i -> (Core.Int32.to_string i)))"]
    | false, CoreInt64 ->
       String.concat ["(conv (fun x -> Core.Int64.to_string x))"]
    | true, CoreInt64 ->
       String.concat ["(conv (fun x -> match x with None -> \"NULL\" | Some i -> (Core.Int64.to_string i)))"]
    | false, Bignum -> "(conv (fun x -> Bignum_extended.to_string_hum x))"
    | true, Bignum -> "(conv (fun x -> match x with None -> \"NULL\" | Some i -> (Bignum_extended.to_string_hum i)))"
    | false, Float -> "(conv (fun x -> Float.to_string_round_trippable x))"
    | true, Float -> "(conv (fun x -> match x with None -> \"NULL\" | Some i -> (Float.to_string_round_trippable i)))"
    | false, Date -> "(conv (fun x -> (\"'\" ^ (Date_extended.to_string x) ^ \"'\")))"
    | true, Date -> "(conv (fun x -> Utilities.serialize_optional_date_field ~field:x))"
    | false, Time -> "(conv (fun x -> (\"'\" ^ (Date_time_extended.to_string x) ^ \"'\")))"
    | true, Time -> "(conv (fun x -> Utilities.serialize_optional_date_time_field ~field:x))"
end
