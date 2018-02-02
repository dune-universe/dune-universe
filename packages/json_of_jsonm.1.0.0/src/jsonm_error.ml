type t =
  [ `Expected of
    [ `Aval of bool
    | `Comment
    | `Eoi
    | `Json
    | `Name
    | `Name_sep
    | `Omem of bool
    | `Value
    ]
  | `Illegal_BOM
  | `Illegal_bytes of string
  | `Illegal_escape of
    [ `Lone_hi_surrogate of int
    | `Lone_lo_surrogate of int
    | `Not_esc_uchar of Uchar.t
    | `Not_hex_uchar of Uchar.t
    | `Not_lo_surrogate of int
    ]
  | `Illegal_literal of string
  | `Illegal_number of string
  | `Illegal_string_uchar of Uchar.t
  | `Unclosed of [ `As | `Comment | `Os | `String ]
]

(* ensure the above type and the jsonm type agree *)
let _enforce_error_type : Jsonm.error -> t = fun x -> x

let fmt_cp u = Printf.sprintf "U+%04X" u

let fmt_uchar u =
  if Uchar.to_int u <= 0x1F then fmt_cp (Uchar.to_int u)
  else
    let b = Buffer.create 4 in
    Uutf.Buffer.add_utf_8 b u;
    Printf.sprintf "'%s' (%s)" (Buffer.contents b) (fmt_cp (Uchar.to_int u))

let to_string = function
  | `Illegal_BOM -> "illegal initial BOM in character stream"
  | `Illegal_escape r ->
      "illegal escape, " ^
      begin match r with
      | `Not_hex_uchar u -> Printf.sprintf "%s not a hex digit" (fmt_uchar u)
      | `Not_esc_uchar u -> Printf.sprintf "%s not an escaped character" (fmt_uchar u)
      | `Lone_lo_surrogate p -> Printf.sprintf "%s lone low surrogate" (fmt_cp p)
      | `Lone_hi_surrogate p -> Printf.sprintf "%s lone high surrogate" (fmt_cp p)
      | `Not_lo_surrogate p -> Printf.sprintf "%s not a low surrogate" (fmt_cp p)
      end
  | `Illegal_string_uchar u ->
      Printf.sprintf "illegal character in JSON string (%s)" (fmt_uchar u)
  | `Illegal_bytes bs ->
      let rec dump_bytes i e acc = 
        if i < e then
          dump_bytes (i + 1) e (acc ^ (Printf.sprintf "%02X" (Char.code bs.[i])))
        else acc
      in
      "illegal bytes in character stream (" ^
      (dump_bytes 0 (String.length bs) "") ^
      ")]"
  | `Illegal_number n -> Printf.sprintf "illegal number (%s)" n
  | `Illegal_literal l -> Printf.sprintf "illegal literal (%s)" l
  | `Unclosed r ->
      "unclosed " ^
      begin match r with
      | `As -> "list";
      | `Os -> "assoc";
      | `String -> "string";
      | `Comment -> "comment"
      end
  | `Expected r ->
      "expected " ^
      begin match r with
      | `Comment -> "JavaScript comment"
      | `Value -> "JSON value"
      | `Name -> "member name"
      | `Name_sep -> "name separator (':')"
      | `Aval true -> "value or array end (value or ']')"
      | `Aval false -> "value separator or array end (',' or ']')"
      | `Omem true -> "member name or object end ('\"' or '}')]"
      | `Omem false ->"value separator or object end (',' or '}')"
      | `Json -> "JSON text (JSON value)"
      | `Eoi -> "end of input"
      end
