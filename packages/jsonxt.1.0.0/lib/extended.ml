module Compliance = struct
  type json = Json.json
  type json_stream = Json_stream.json

  open Tokens

  let lex_string s = Lexxer_utils.unescape_string s
  let lex_number token = token
  let lex_integer token = token

  let lex_largeint = function
  | LARGEINT s -> FLOAT (float_of_string s)
  | token -> token

  let lex_variant _ = true
  let lex_tuple _ = true

  let comment_check () = Error "comments are not supported in basic mode"

  let number_to_string f =
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> Json_float.string_of_float_json f
    | FP_infinite -> if f < 0. then "-Infinity" else "Infinity"
    | FP_nan -> "NaN"

  let largeint s = `Float (float_of_string s)
  let integer i = `Int i
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l
  let tuple l = `Tuple l
  let variant k v = `Variant (k, v)

  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    `Float (1.0 /. 0.0)
  | `Neginfinity -> `Float (-1.0 /. 0.0)
  | `Nan ->         `Float (0.0 /. 0.0)
  | `Floatlit _ ->  raise (Failure "floatlit not supported in basic mode")

  module Stream = struct
    let number = number
    let largeint = largeint
    let integer = integer
    let null = null
    let string = string
    let bool = bool

    let array_start () = `As
    let array_end () = `Ae
    let object_start () = `Os
    let object_end () = `Oe
    let tuple_start () = `Ts
    let tuple_end () = `Te
    let variant_start () = `Vs
    let variant_end () = `Ve
    let name s = `Name s
  end
end

module Lexxer = Compliant_lexxer.Make(Compliance)
module Parser = Parser.Make(Compliance)
include Reader_string_file.Make (Lexxer) (Parser)
type t = json

include Writer_string.Make(Compliance)
include Writer_file.Make(Compliance)
include Pretty.Make(Compliance)

module Process = Process.Extended
