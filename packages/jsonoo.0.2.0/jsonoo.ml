open Js_of_ocaml

type t = < > Js.t

exception Decode_error of string

let decode_error message = raise (Decode_error message)

let _JSON = Js.Unsafe.global##._JSON

let try_parse_opt s =
  try Some (_JSON##parse (Js.string s)) with
  | _ -> None

let try_parse_exn s =
  try _JSON##parse (Js.string s) with
  | _ -> decode_error ("Failed to parse JSON string \"" ^ s ^ "\"")

let stringify ?spaces (json : t) =
  let js_string =
    match spaces with
    | None   -> _JSON##stringify json
    | Some n ->
      let spaces = Js.number_of_float (float_of_int n) in
      _JSON##stringify json Js.undefined spaces
  in
  Js.to_string js_string

module Decode = struct
  type 'a decoder = t -> 'a

  let expected typ (json : t) =
    decode_error ("Expected " ^ typ ^ ", got " ^ stringify json)

  let expected_length length array =
    decode_error
      ( "Expected array of length "
      ^ string_of_int length
      ^ ", got array of length "
      ^ string_of_int (Array.length array) )

  let typeof value = Js.to_string (Js.typeof value)

  let is_array value =
    let array_constr = Js.Unsafe.global##._Array in
    Js.instanceof value array_constr

  let id (json : t) = json

  let null (json : t) =
    if Js.Opt.test (Js.Opt.return json) then
      expected "null" json
    else
      Js.null

  let bool (json : t) =
    if typeof json = "boolean" then
      Js.to_bool (Js.Unsafe.coerce json)
    else
      expected "boolean" json

  let float (json : t) =
    if typeof json = "number" then
      Js.float_of_number (Js.Unsafe.coerce json)
    else
      expected "number" json

  let int (json : t) =
    let f = float json in
    if Float.is_finite f && Float.floor f = f then
      int_of_float f
    else
      expected "integer" json

  let string (json : t) =
    if typeof json = "string" then
      Js.to_string (Js.Unsafe.coerce json)
    else
      expected "string" json

  let char (json : t) =
    let s = string json in
    if String.length s = 1 then
      s.[0]
    else
      expected "single-character string" json

  let nullable decode (json : t) =
    if Js.Opt.test (Js.Opt.return json) then
      Some (decode json)
    else
      None

  let array decode (json : t) =
    if is_array json then
      let array = Js.to_array (Js.Unsafe.coerce json) in
      let convert i (json : t) =
        try decode json with
        | Decode_error message ->
          decode_error (message ^ "\n\tin array at index " ^ string_of_int i)
      in
      Array.mapi convert array
    else
      expected "array" json

  let list decode (json : t) = Array.to_list (array decode json)

  let tuple_element decode array i =
    try decode array.(i) with
    | Decode_error message ->
      decode_error (message ^ "\n\tin array at index " ^ string_of_int i)

  let pair decode_a decode_b (json : t) =
    let array = array id json in
    if Array.length array = 2 then
      let a = tuple_element decode_a array 0 in
      let b = tuple_element decode_b array 1 in
      (a, b)
    else
      expected_length 2 array

  let tuple2 = pair

  let tuple3 decode_a decode_b decode_c (json : t) =
    let array = array id json in
    if Array.length array = 3 then
      let a = tuple_element decode_a array 0 in
      let b = tuple_element decode_b array 1 in
      let c = tuple_element decode_c array 2 in
      (a, b, c)
    else
      expected_length 3 array

  let tuple4 decode_a decode_b decode_c decode_d (json : t) =
    let array = array id json in
    if Array.length array = 4 then
      let a = tuple_element decode_a array 0 in
      let b = tuple_element decode_b array 1 in
      let c = tuple_element decode_c array 2 in
      let d = tuple_element decode_d array 3 in
      (a, b, c, d)
    else
      expected_length 4 array

  let object_field decode js_object key =
    try decode (Js.Unsafe.get js_object key) with
    | Decode_error message ->
      decode_error
        (message ^ "\n\tin object at field '" ^ Js.to_string key ^ "'")

  let dict decode (json : t) =
    if
      typeof json = "object"
      && (not (is_array json))
      && Js.Opt.(test (return json))
    then (
      let keys = Js.to_array (Js.object_keys json) in
      let table = Hashtbl.create (Array.length keys) in
      let set key =
        let value = object_field decode json key in
        Hashtbl.add table (Js.to_string key) value
      in
      Array.iter set keys;
      table
    ) else
      expected "object" json

  let field key decode (json : t) =
    if
      typeof json = "object"
      && (not (is_array json))
      && Js.Opt.(test (return json))
    then
      let js_key = Js.string key in
      if Js.Optdef.(test @@ return (Js.Unsafe.get json js_key)) then
        object_field decode json js_key
      else
        decode_error ("Expected field '" ^ key ^ "'")
    else
      expected "object" json

  let rec at key_path decoder =
    match key_path with
    | [ key ]       -> field key decoder
    | first :: rest -> field first (at rest decoder)
    | []            ->
      invalid_arg "Expected key_path to contain at least one element"

  let try_optional decode (json : t) =
    try Some (decode json) with
    | Decode_error _ -> None

  let try_default value decode (json : t) =
    try decode json with
    | Decode_error _ -> value

  let any decoders (json : t) =
    let rec inner errors = function
      | []             ->
        let rev_errors = List.rev errors in
        decode_error
          ( "Value was not able to be decoded with the given decoders. Errors: "
          ^ String.concat "\n" rev_errors )
      | decode :: rest -> (
        try decode json with
        | Decode_error e -> inner (e :: errors) rest )
    in
    inner [] decoders

  let either a b = any [ a; b ]

  let map f decode (json : t) = f (decode json)

  let bind b a (json : t) = b (a json) json
end

module Encode = struct
  type 'a encoder = 'a -> t

  let id (json : t) = json

  let null : t = Obj.magic Js.null

  let bool b : t = Js.Unsafe.coerce (Js.bool b)

  let float f : t = Js.Unsafe.coerce (Js.number_of_float f)

  let int i : t = Js.Unsafe.coerce (Js.number_of_float (float_of_int i))

  let string s : t = Js.Unsafe.coerce (Js.string s)

  let char c : t = string (String.make 1 c)

  let nullable encode = function
    | None   -> null
    | Some v -> encode v

  let array encode a : t =
    let encoded : t array = Array.map encode a in
    Js.Unsafe.coerce (Js.array encoded)

  let list encode l : t = array encode (Array.of_list l)

  let pair encode_a encode_b (a, b) : t =
    let encoded : t array = [| encode_a a; encode_b b |] in
    Js.Unsafe.coerce (Js.array encoded)

  let tuple2 = pair

  let tuple3 encode_a encode_b encode_c (a, b, c) : t =
    let encoded : t array = [| encode_a a; encode_b b; encode_c c |] in
    Js.Unsafe.coerce (Js.array encoded)

  let tuple4 encode_a encode_b encode_c encode_d (a, b, c, d) : t =
    let encoded : t array =
      [| encode_a a; encode_b b; encode_c c; encode_d d |]
    in
    Js.Unsafe.coerce (Js.array encoded)

  let dict encode table : t =
    let encode_pair (k, v) = (k, Js.Unsafe.coerce (encode v)) in
    table
    |> Hashtbl.to_seq
    |> Array.of_seq
    |> Array.map encode_pair
    |> Js.Unsafe.obj

  let object_ (props : (string * t) list) : t =
    let coerce (k, v) = (k, Js.Unsafe.coerce v) in
    Js.Unsafe.obj (Array.map coerce @@ Array.of_list props)
end

let t_of_js : Ojs.t -> t = Obj.magic

let t_to_js : t -> Ojs.t = Obj.magic
