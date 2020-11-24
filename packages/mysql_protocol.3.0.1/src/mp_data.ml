type sign = Positive | Negative

type sql =
  | Null
  | Tinyint of int
  | Smallint of int
  | Int of int
  | Longint of Int64.t
  | Longlongint of Big_int.big_int
  | Decimal of Num.num
  | Date of (int * int * int) (* year, month, day *)
  | Time of (sign * int * int * int * Int64.t) (* sign * hour, min, sec, microsec *)
  | Datetime of ((int * int * int) * (int * int * int * Int64.t)) (* (year, month, day), (hour, min, sec, microsec) *)
  | Timestamp of ((int * int * int) * (int * int * int * Int64.t)) (* (year, month, day), (hour, min, sec, microsec) *)
  | Float of float
  | Double of float
  | Int24 of int
  | Year of int
  | Varchar of string
  | String of string
  | Varstring of string
  | Blob of Buffer.t (* TODO : add a Text type ? *)
  | Binary of Buffer.t
  | Varbinary of Buffer.t
  | Enum of string
  | Set of string
  | Bit of Bitstring.t
  | Geometry of Bitstring.t

type t = sql

exception Wrong_type of string

let data_null = Null

let data_tinyint v = Tinyint v

let data_smallint v = Smallint v

let data_int v = Int v

let data_longint v = Longint v

let data_longlongint v = Longlongint v

let data_decimal v = Decimal v

let data_date v = Date v

let data_time v = Time v

let data_datetime v = Datetime v

let data_timestamp v = Timestamp v

let data_float v = Float v

let data_double v = Double v

let data_int24 v = Int24 v

let data_year v = Year v

let data_varchar v = Varchar v

let data_string v = String v

let data_varstring v = Varstring v

let data_blob v = Blob v

let data_binary v = Binary v

let data_varbinary v = Varbinary v

let data_enum v = Enum v

let data_set v = Set v

let data_bit v = Bit v

let data_geometry v = Geometry v

let sign_to_string s = 
  match s with
  | Positive -> "+"
  | Negative -> "-"

let type_to_string = function
  | Null -> "Null"
  | Tinyint _ -> "Tinyint"
  | Smallint _ -> "Smallint"
  | Int _ -> "Int"
  | Int24 _ -> "Int24"
  | Year _ -> "Year"
  | Longint _ -> "Longint"
  | Longlongint _ -> "Longlongint"
  | Decimal _ -> "Decimal"
  | Date _ -> "Date"
  | Time _ -> "Time"
  | Datetime _ -> "Datetime"
  | Timestamp _ -> "Timestamp"
  | Float _ -> "Float"
  | Double _ -> "Double"
  | Varchar _ -> "Varchar"
  | String _ -> "String"
  | Varstring _ -> "Varstring"
  | Enum _ -> "Enum"
  | Set _ -> "Set"
  | Blob _ -> "Blob"
  | Binary _ -> "Binary"
  | Varbinary _ -> "Varbinary"
  | Bit _ -> "Bit"
  | Geometry _ -> "Geometry"

let to_string = function
  | Null -> None
  | Tinyint v -> Some(string_of_int v)
  | Smallint v -> Some(string_of_int v)
  | Int v -> Some(string_of_int v)
  | Int24 v -> Some(string_of_int v)
  | Year v -> Some(string_of_int v)
  | Longint v -> Some(Int64.to_string v)
  | Longlongint v -> Some(Big_int.string_of_big_int v)
  | Decimal v -> Some(Num.string_of_num v)
  | Date v -> Some(
      let (year, month, day) = v in
      Printf.sprintf "%u-%u-%u" year month day
    )
  | Time v -> Some(
      let (sign, hour, min, sec, subsec) = v in
      Printf.sprintf "%s%u:%u:%u.%Lu" (sign_to_string sign) hour min sec subsec
    )
  | Datetime v -> Some(
      let ((year, month, day), (hour, min, sec, subsec)) = v in
      Printf.sprintf "%u-%u-%u %u:%u:%u.%Lu" year month day hour min sec subsec
    )
  | Timestamp v -> Some(
      let ((year, month, day), (hour, min, sec, subsec)) = v in
      Printf.sprintf "%u-%u-%u %u:%u:%u.%Lu" year month day hour min sec subsec
    )
  | Float v -> Some(string_of_float v)
  | Double v -> Some(string_of_float v)
  | Varchar v -> Some v
  | String v -> Some v
  | Varstring v -> Some v
  | Enum v -> Some v
  | Set v -> Some v
  | Blob v -> Some(Buffer.contents v)
  | Binary v -> Some(Buffer.contents v)
  | Varbinary v -> Some(Buffer.contents v)
  | Bit v -> Some(
      match%bitstring v with
      | {| b1  : 1; b2  : 1; b3  : 1; b4  : 1; b5  : 1; b6  : 1; b7  : 1; b8  : 1;
          b9  : 1; b10 : 1; b11 : 1; b12 : 1; b13 : 1; b14 : 1; b15 : 1; b16 : 1;
          b17 : 1; b18 : 1; b19 : 1; b20 : 1; b21 : 1; b22 : 1; b23 : 1; b24 : 1;
          b25 : 1; b26 : 1; b27 : 1; b28 : 1; b29 : 1; b30 : 1; b31 : 1; b32 : 1;
          b33 : 1; b34 : 1; b35 : 1; b36 : 1; b37 : 1; b38 : 1; b39 : 1; b40 : 1;
          b41 : 1; b42 : 1; b43 : 1; b44 : 1; b45 : 1; b46 : 1; b47 : 1; b48 : 1;
          b49 : 1; b50 : 1; b51 : 1; b52 : 1; b53 : 1; b54 : 1; b55 : 1; b56 : 1;
          b57 : 1; b58 : 1; b59 : 1; b60 : 1; b61 : 1; b62 : 1; b63 : 1; b64 : 1 |} ->
          (
            let f b =
              match b with
              | true -> "1"
              | false -> "0"
            in
            Printf.sprintf "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s"
              (f b1)  (f b2)  (f b3)  (f b4)  (f b5)  (f b6)  (f b7)  (f b8)
              (f b9)  (f b10) (f b11) (f b12) (f b13) (f b14) (f b15) (f b16)
              (f b17) (f b18) (f b19) (f b20) (f b21) (f b22) (f b23) (f b24)
              (f b25) (f b26) (f b27) (f b28) (f b29) (f b30) (f b31) (f b32)
              (f b33) (f b34) (f b35) (f b36) (f b37) (f b38) (f b39) (f b40)
              (f b41) (f b42) (f b43) (f b44) (f b45) (f b46) (f b47) (f b48)
              (f b49) (f b50) (f b51) (f b52) (f b53) (f b54) (f b55) (f b56)
              (f b57) (f b58) (f b59) (f b60) (f b61) (f b62) (f b63) (f b64)
          )
      | {| _ |} -> assert false
    )
  | Geometry v -> Some(Bitstring.string_of_bitstring v)

let raise_wrong_type v t =
  let advice =
    match v with
    | Null -> ""
    | Tinyint _ -> "to_ocaml_int"
    | Smallint _ -> "to_ocaml_int"
    | Int _ -> "to_ocaml_int"
    | Int24 _ -> "to_ocaml_int"
    | Year _ -> "to_ocaml_int"
    | Longint _ -> "to_ocaml_int64"
    | Longlongint _ -> "to_ocaml_big_int"
    | Decimal _ -> "to_ocaml_num"
    | Date _ -> "to_ocaml_date"
    | Time _ -> "to_ocaml_time"
    | Datetime _ -> "to_ocaml_dt_ts"
    | Timestamp _ -> "to_ocaml_dt_ts"
    | Float _ -> "to_ocaml_float"
    | Double _ -> "to_ocaml_float"
    | Varchar _ -> "to_ocaml_string"
    | String _ -> "to_ocaml_string"
    | Varstring _ -> "to_ocaml_string"
    | Enum _ -> "to_ocaml_string"
    | Set _ -> "to_ocaml_string"
    | Blob _ -> "to_ocaml_buffer"
    | Binary _ -> "to_ocaml_buffer"
    | Varbinary _ -> "to_ocaml_buffer"
    | Bit _ -> "to_ocaml_bitstring"
    | Geometry _ -> "to_ocaml_bitstring"
  in
  let s =
    match to_string v with
    | None -> "NULL"
    | Some x -> x
  in
  let s =
    if String.length s > 32 then
      (String.sub s 0 32) ^ "..."
    else s
  in
  let msg =
    let advice =
      if advice = "" then ""
      else Printf.sprintf " May be you should use the \"%s\" function." advice
    in
    Printf.sprintf "Unable to convert to %s, value \"%s\" is of type %s.%s"
    t s (type_to_string v) advice
  in
  raise (Wrong_type msg)

let to_ocaml_int v =
  let t = "int" in
  match v with
  | Null -> None
  | Tinyint v -> Some v
  | Smallint v -> Some v
  | Int v -> Some v
  | Int24 v -> Some v
  | Year v -> Some v
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_int64 v =
  let t = "Int64" in
  match v with
  | Null -> None
  | Longint v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _  -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_big_int v =
  let t = "Big_int" in
  match v with
  | Null -> None
  | Longlongint v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_num v =
  let t = "Num" in
  match v with
  | Null -> None
  | Decimal v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_date v =
  let t = "date" in
  match v with
  | Null -> None
  | Date v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_time v =
  let t = "time" in
  match v with
  | Null -> None
  | Time v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_dt_ts v =
  let t = "datetime/timestamp" in
  match v with
  | Null -> None
  | Datetime v -> Some v
  | Timestamp v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_float v =
  let t = "float" in
  match v with
  | Null -> None
  | Float v -> Some v
  | Double v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_string v =
  let t = "string" in
  match v with
  | Null -> None
  | Varchar v -> Some v
  | String v -> Some v
  | Varstring v -> Some v
  | Enum v -> Some v
  | Set v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_buffer v =
  let t = "Buffer" in
  match v with
  | Null -> None
  | Blob v -> Some v
  | Binary v -> Some v
  | Varbinary v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Bit _ -> raise_wrong_type v t
  | Geometry _ -> raise_wrong_type v t

let to_ocaml_bitstring v =
  let t = "Bitstring" in
  match v with
  | Null -> None
  | Bit v -> Some v
  | Geometry v -> Some v
  | Tinyint _ -> raise_wrong_type v t
  | Smallint _ -> raise_wrong_type v t
  | Int _ -> raise_wrong_type v t
  | Int24 _ -> raise_wrong_type v t
  | Year _ -> raise_wrong_type v t
  | Longint _ -> raise_wrong_type v t
  | Longlongint _ -> raise_wrong_type v t
  | Decimal _ -> raise_wrong_type v t
  | Date _ -> raise_wrong_type v t
  | Time _ -> raise_wrong_type v t
  | Datetime _ -> raise_wrong_type v t
  | Timestamp _ -> raise_wrong_type v t
  | Float _ -> raise_wrong_type v t
  | Double _ -> raise_wrong_type v t
  | Varchar _ -> raise_wrong_type v t
  | String _ -> raise_wrong_type v t
  | Varstring _ -> raise_wrong_type v t
  | Enum _ -> raise_wrong_type v t
  | Set _ -> raise_wrong_type v t
  | Blob _ -> raise_wrong_type v t
  | Binary _ -> raise_wrong_type v t
  | Varbinary _ -> raise_wrong_type v t

let eq : t -> t -> bool = fun d1 d2 ->
  match d1, d2 with
  | Null, Null -> true
  | Null, _ -> false
  | _, Null -> false
  | Tinyint v1, Tinyint v2 -> v1 = v2
  | Tinyint _, _ -> false
  | _, Tinyint _ -> false
  | Smallint v1, Smallint v2 -> v1 = v2
  | Smallint _, _ -> false
  | _, Smallint _ -> false
  | Int v1, Int v2 -> v1 = v2
  | Int _, _ -> false
  | _, Int _ -> false
  | Date v1, Date v2 -> v1 = v2
  | Date _, _ -> false
  | _, Date _ -> false
  | Time v1, Time v2 -> v1 = v2
  | Time _, _ -> false
  | _, Time _ -> false
  | Datetime v1, Datetime v2 -> v1 = v2
  | Datetime _, _ -> false
  | _, Datetime _ -> false
  | Timestamp v1, Timestamp v2 -> v1 = v2
  | Timestamp _, _ -> false
  | _, Timestamp _ -> false
  | Float v1, Float v2 -> v1 = v2
  | Float _, _ -> false
  | _, Float _ -> false
  | Double v1, Double v2 -> v1 = v2
  | Double _, _ -> false
  | _, Double _ -> false
  | Int24 v1, Int24 v2 -> v1 = v2
  | Int24 _, _ -> false
  | _, Int24 _ -> false
  | Year v1, Year v2 -> v1 = v2
  | Year _, _ -> false
  | _, Year _ -> false
  | Varchar v1, Varchar v2 -> v1 = v2
  | Varchar _, _ -> false
  | _, Varchar _ -> false
  | String v1, String v2 -> v1 = v2
  | String _, _ -> false
  | _, String _ -> false
  | Varstring v1, Varstring v2 -> v1 = v2
  | Varstring _, _ -> false
  | _, Varstring _ -> false
  | Enum v1, Enum v2 -> v1 = v2
  | Enum _, _ -> false
  | _, Enum _ -> false
  | Set v1, Set v2 -> v1 = v2
  | Set _, _ -> false
  | _, Set _ -> false
  | Longint v1, Longint v2 -> 
    if (Int64.compare v1 v2 = 0) then true else false
  | Longint _, _ -> false
  | _, Longint _ -> false
  | Longlongint v1, Longlongint v2 ->
    if (Big_int.compare_big_int v1 v2 = 0) then true else false
  | Longlongint _, _ -> false
  | _, Longlongint _ -> false
  | Decimal v1, Decimal v2 -> Num.eq_num v1 v2
  | Decimal _, _ -> false
  | _, Decimal _ -> false
  | Blob v1, Blob v2 -> Buffer.contents v1 = Buffer.contents v2
  | Blob _, _ -> false
  | _, Blob _ -> false
  | Binary v1, Binary v2 -> Buffer.contents v1 = Buffer.contents v2
  | Binary _, _ -> false
  | _, Binary _ -> false
  | Varbinary v1, Varbinary v2 -> Buffer.contents v1 = Buffer.contents v2
  | Varbinary _, _ -> false
  | _, Varbinary _ -> false
  | Bit v1, Bit v2 -> Bitstring.equals v1 v2
  | Bit _, _ -> false
  | _, Bit _ -> false
  | Geometry v1, Geometry v2 -> Bitstring.equals v1 v2
