module Compliance = struct
  type json = Json.Basic.json
  type json_stream = Json_stream.Basic.json

  open Tokens

  let lex_string s = Lexxer_utils.unescape_string s

  let lex_number = function
  | INFINITY -> COMPLIANCE_ERROR "inf not supported"
  | NEGINFINITY -> COMPLIANCE_ERROR "-inf not supported"
  | NAN -> COMPLIANCE_ERROR "nan not supported"
  | FLOAT _ as token -> token
  | token -> token

  let lex_integer token = token

  let lex_largeint = function
  | LARGEINT s -> FLOAT (float_of_string s)
  | token -> token

  let lex_variant _ = false
  let lex_tuple _ = false

  let comment_check () = Error "comments are not supported in basic mode"

  let number_to_string f =
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> Json_float.string_of_float_json f
    | FP_infinite -> raise (Failure "infinity not supported")
    | FP_nan -> raise (Failure "nan not supported")

  let largeint s = `Float (float_of_string s)
  let integer i = `Int i
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l
  let tuple _l = raise (Failure "tuples not supported in basic mode")
  let variant _l = raise (Failure "variants not supported in basic mode")

  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    raise (Failure "inf not supported in basic mode")
  | `Neginfinity -> raise (Failure "-inf not supported in basic mode")
  | `Nan ->         raise (Failure "nan not supported in basic mode")
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
    let tuple_start () = raise (Failure "tuples not supported in basic mode")
    let tuple_end () = raise (Failure "tuples not supported in basic mode")
    let variant_start () = raise (Failure "variants not supported in basic mode")
    let variant_end () = raise (Failure "variants not supported in basic mode")
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

module Process = Process.Basic
