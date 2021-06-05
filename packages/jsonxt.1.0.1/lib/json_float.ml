external format_float : string -> float -> string = "caml_format_float"

(* Determine the size of an integer, handles 30bit, 62bit and Jsoo using 32bit ints *)
let max_json_int =
  let rec log2 n = if n <= 1 then 0 else 1 + log2(n asr 1) in
  let bits n = log2 n + 1 in
  if bits max_int > 53 then (1 lsl 53) - 1 else max_int

let max_json_int_as_float = float_of_int max_json_int

let string_of_float_json f =
  let is_int = (float_of_int (int_of_float f)) = f in
  if is_int && abs_float f <= max_json_int_as_float then begin (* IEEE max int in a float when in 64bit int mode*)
    let int_value = int_of_float f in (string_of_int int_value) ^ ".0"
  end
  else begin
    (* %.17g often creates overly long output, attempt to convert at lower precession first *)
    let s = format_float "%.16g" f in
    let s = if float_of_string s = f then s else format_float "%.17g" f in
    if not (String.contains s '.' || String.contains s 'e') then s ^ ".0"
    else s
  end
