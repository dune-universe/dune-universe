open Cps_toolbox

module AST = struct

type value =
  | JNull
  | JBool of bool
  | JNumber of number
  | JString of string
  | JObject of (string * value) list
  | JArray of value list
and number =
  | NInt of int
  | NFloat of float

end (* AST *)

module Layout = struct
open Typeset
open AST

let _no_sep x _y = x
let _comma_sep x y = x <&> ~$"," <!+> y

let rec _visit_value value return =
  match value with
  | JNull -> return ~$"null"
  | JBool value1 ->
    if value1
    then return ~$"true"
    else return ~$"false"
  | JNumber number ->
    _visit_number number return
  | JString content ->
    return (~$"\"" <!&> ~$content <!&> ~$"\"")
  | JObject members ->
    List.fold
      (fun return -> return _no_sep null)
      (fun (key, value1) visit_members return ->
        visit_members @@ fun join members1 ->
        _visit_value value1 @@ fun value2 ->
        return _comma_sep (join
          (grp (~$key <!+> ~$":" <+> (nest (grp value2))))
          members1))
      members @@ fun _join members2 ->
    return (seq (~$"{" <!+> members2 <!+> ~$"}"))
  | JArray elements ->
    List.fold
      (fun return -> return _no_sep null)
      (fun element visit_elements return ->
        visit_elements @@ fun join elements1 ->
        _visit_value element @@ fun element1 ->
        return _comma_sep (join
          (grp element1)
          elements1))
      elements @@ fun _join elements2 ->
    return (seq (~$"[" <!+> elements2 <!+> ~$"]"))
and _visit_number number return =
  match number with
  | NInt value -> return ~$(string_of_int value)
  | NFloat value -> return ~$(string_of_float value)

let value x r = _visit_value x r

end (* Layout *)

module Print = struct

let value value1 return =
  Layout.value value1 @@ fun layout ->
  Typeset.compile layout @@ fun document ->
  Typeset.render document 2 80 return

end (* Print *)
