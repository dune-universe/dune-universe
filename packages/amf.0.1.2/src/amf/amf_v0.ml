open Core
open Stdint
open Sexplib.Std

module Time = struct

  include Time

  let to_unix_float time =
    to_span_since_epoch time |> Span.to_sec

  let of_epoch float =
    Span.of_sec float |> of_span_since_epoch

end

exception UnsupportedType
exception UpgradeToAMFV3

type typed_object = {
  class_name : string;
  properties : (string * t) list;
} [@@deriving sexp]

and t =
  | Number        of float
  | Boolean       of bool
  | String        of string
  | Object        of (string * t) list
  | MovieClip
  | Null
  | Undefined
  | Reference     of int
  | ECMAArray     of (string * t) list
  | ObjectEnd
  | StrictArray   of t array
  | Date          of Time.t
  | LongString    of string
  | Unsupported
  | RecordSet
  | XMLDocument   of string
  | TypedObject   of typed_object
  | AVMPlusObject [@@deriving sexp]

let marker_buffer item =
  (match item with
   | Number _      -> 0x00
   | Boolean _     -> 0x01
   | String _      -> 0x02
   | Object _      -> 0x03
   | MovieClip     -> 0x04
   | Null          -> 0x05
   | Undefined     -> 0x06
   | Reference _   -> 0x07
   | ECMAArray _   -> 0x08
   | ObjectEnd     -> 0x09
   | StrictArray _ -> 0x0a
   | Date _        -> 0x0b
   | LongString _  -> 0x0c
   | Unsupported   -> 0x0d
   | RecordSet     -> 0x0e
   | XMLDocument _ -> 0x0f
   | TypedObject _ -> 0x11
   | AVMPlusObject -> 0x12)
  |> fun num ->
     let dst = Bytes.create 1 in
     Uint8.(to_bytes_big_endian (of_int num) dst 0) ;
     dst

let double_to_buffer src =
  let module Inttype = Int64 in
  let int_length = Inttype.bits / 8 in
  let dst = Bytes.create int_length in
  Inttype.(to_bytes_big_endian (Core.Int64.bits_of_float src) dst 0) ;
  dst

let peel_off_byte src =
  let module Inttype = Uint8 in
  let int_length = Inttype.bits / 8 in
  if String.length src < int_length
  then Result.Error (Error.of_string "Premature end of buffer")
  else Result.Ok
         (Inttype.(of_bytes_big_endian (Bytes.of_string src) 0 |> to_int),
          String.sub src ~pos:int_length ~len:(String.length src - int_length))

let peel_off_u16 src =
  let module Inttype = Uint16 in
  let int_length = Inttype.bits / 8 in
  if String.length src < int_length
  then Result.Error (Error.of_string "Premature end of buffer")
  else Result.Ok
         (Inttype.(of_bytes_big_endian (Bytes.of_string src) 0 |> to_int),
          String.sub src ~pos:int_length ~len:(String.length src - int_length))

let peel_off_u32 src =
  let module Inttype = Uint32 in
  let int_length = Inttype.bits / 8 in
  if String.length src < int_length
  then Result.Error (Error.of_string "Premature end of buffer")
  else Result.Ok
         (Inttype.(of_bytes_big_endian (Bytes.of_string src) 0 |> to_int),
          String.sub src ~pos:int_length ~len:(String.length src - int_length))

let peel_off_double src =
  let double_length = 8 in
  if String.length src < double_length
  then Result.Error (Error.of_string "Premature end of buffer")
  else Result.Ok
         (Int64.(of_bytes_big_endian (Bytes.of_string src) 0 |> Core.Int64.float_of_bits),
          String.sub src
                     ~pos:double_length
                     ~len:(String.length src - double_length))

let peel_off_utf8 src =
  let open Result in
  peel_off_u16 src >>= fun (len, src) ->
  if String.length src < len
  then Result.Error (Error.of_string "Premature end of buffer")
  else Result.Ok
         (String.sub src ~pos:0 ~len:len,
          String.sub src
                     ~pos:len
                     ~len:(String.length src - len))

let peel_off_long_utf8 src =
  let open Result in
  peel_off_u32 src >>= fun (len, src) ->
  if String.length src < len
  then Result.Error (Error.of_string "Premature end of buffer")
  else Result.Ok
         (String.sub src ~pos:0 ~len:len,
          String.sub src
                     ~pos:len
                     ~len:(String.length src - len))

let boolean_to_buffer = function
  | true -> "\001"
  | false -> "\000"

let u16_to_buffer src =
  let module Inttype = Uint16 in
  let dst = Bytes.create (Inttype.bits / 8) in
  Inttype.(to_bytes_big_endian (of_int src) dst 0) ;
  dst

let u32_to_buffer src =
  let module Inttype = Uint32 in
  let dst = Bytes.create (Inttype.bits / 8) in
  Inttype.(to_bytes_big_endian (of_int src) dst 0) ;
  dst

let utf8_to_buffer src =
  Bytes.to_string (u16_to_buffer (String.length src)) ^ src

let utf8_long_to_buffer src =
  Bytes.to_string (u32_to_buffer (String.length src)) ^ src

let rec object_property_to_buffer (name, item) =
  utf8_to_buffer name ^ to_buffer item

and to_buffer item =
  match item with
  | Number value ->
     Bytes.to_string (marker_buffer item) ^ Bytes.to_string (double_to_buffer value)
  | Boolean value ->
     Bytes.to_string (marker_buffer item) ^ boolean_to_buffer value
  | String value ->
     Bytes.to_string (marker_buffer item) ^ utf8_to_buffer value
  | Object value ->
     Bytes.to_string (marker_buffer item) ^
       List.fold ~init:""
                 ~f:(fun sum entry -> sum ^ object_property_to_buffer entry)
                 value ^
         to_buffer ObjectEnd
  | MovieClip -> raise UnsupportedType
  | Null ->
     Bytes.to_string (marker_buffer item)
  | Undefined ->
     Bytes.to_string (marker_buffer item)
  | Reference value ->
     Bytes.to_string (marker_buffer item) ^ Bytes.to_string (u16_to_buffer value)
  | ECMAArray value ->
     Bytes.to_string (marker_buffer item) ^ Bytes.to_string (u32_to_buffer (List.length value)) ^
       List.fold ~init:""
                 ~f:( ^ )
                 (List.map ~f:object_property_to_buffer value)
  | ObjectEnd ->
     utf8_to_buffer "" ^ Bytes.to_string (marker_buffer item)
  | StrictArray value ->
     (* amf standard omits the marker. mistake maybe? *)
     Bytes.to_string (marker_buffer item) ^ Bytes.to_string (u32_to_buffer (Array.length value)) ^
       Array.fold ~init:"" ~f:( ^ ) (Array.map ~f:to_buffer value)
  | Date value ->
     Bytes.to_string (marker_buffer item) ^ Bytes.to_string (Time.to_unix_float value *. 1000.0
                           |> double_to_buffer) ^
       Bytes.to_string (u16_to_buffer 0)
  | LongString value ->
     Bytes.to_string (marker_buffer item) ^ utf8_long_to_buffer value
  | Unsupported -> raise UnsupportedType
  | RecordSet -> raise UnsupportedType
  | XMLDocument value ->
     Bytes.to_string (marker_buffer item) ^ utf8_long_to_buffer value
  | TypedObject {
        class_name;
        properties;
      } ->
     Bytes.to_string (marker_buffer item) ^ utf8_to_buffer class_name ^
       List.fold ~init:""
                 ~f:(fun sum entry -> sum ^ object_property_to_buffer entry)
                 properties ^
         to_buffer ObjectEnd
  | AVMPlusObject -> raise UpgradeToAMFV3

let rec peel_off_object_property_opt src =
  let open Result in
  peel_off_utf8 src >>= fun (name, src) ->
  if name = ""
  then (peel_off_byte src >>= fun (byte, src) ->
        if Char.of_int_exn byte <> Char.of_string (Bytes.to_string (marker_buffer ObjectEnd))
        then Error (Error.of_string "Object didn't end with ObjectEnd")
        else Ok (None, src))
  else
    peel_off_buffer src >>= fun (item, src) ->
    Ok (Some (name, item), src)

and peel_off_object_property src =
  let open Result in
  peel_off_object_property_opt src >>= fun (value, src) ->
  match value with
  | None -> Error (Error.of_string "Expecting another property, came up empty")
  | Some value -> Ok (value, src)

and peel_off_buffer src =
  let open Result in
  peel_off_byte src >>= fun (marker, src) ->
  match marker with
  | 0x00 ->
     peel_off_double src >>= fun (value, src) ->
     Ok (Number value, src)
  | 0x01 ->
     peel_off_byte src >>= fun (value, src) ->
     Ok (Boolean (value <> 0), src)
  | 0x02 ->
     peel_off_utf8 src >>= fun (value, src) ->
     Ok (String value, src)
  | 0x03 ->
     let rec iter rev_props src =
       peel_off_object_property_opt src >>= fun (next, src) ->
       match next with
       | None      -> Ok (List.rev rev_props, src)
       | Some next ->
          iter (next :: rev_props) src
     in
     iter [] src >>= fun (properties, src) ->
     Ok (Object properties, src)
  | 0x04 ->
     Error (Error.of_string "Unsupported marker: MovieClip")
  | 0x05 ->
     Ok (Null, src)
  | 0x06 ->
     Ok (Undefined, src)
  | 0x07 ->
     peel_off_u16 src >>= fun (value, src) ->
     Ok (Reference value, src)
  | 0x08 ->
     peel_off_u32 src >>= fun (count, src) ->
     let rec iter count rev_props src =
       if count = 0
       then Ok (List.rev rev_props, src)
       else peel_off_object_property src >>= fun (value, src) ->
            iter (count - 1) (value :: rev_props) src
     in
     iter count [] src >>= fun (properties, src) ->
     Ok (ECMAArray properties, src)
  | 0x09 ->
     Ok (ObjectEnd, src)
  | 0x0a ->
     peel_off_u32 src >>= fun (count, src) ->
     let rec iter count rev_props src =
       if count = 0
       then Ok (List.rev rev_props |> Array.of_list, src)
       else peel_off_buffer src >>= fun (value, src) ->
            iter (count - 1) (value :: rev_props) src
     in
     iter count [] src >>= fun (properties, src) ->
     Ok (StrictArray properties, src)
  | 0x0b ->
     peel_off_double src >>= fun (ms, src) ->
     peel_off_u16 src >>= fun (should_be_zero, src) ->
     if should_be_zero <> 0
     then Error (Error.of_string "Timezone not empty")
     else Ok (Date (Time.of_epoch (ms /. 1000.0)), src)
  | 0x0c ->
     peel_off_long_utf8 src >>= fun (value, src) ->
     Ok (LongString value, src)
  | 0x0d ->
     Error (Error.of_string "Unsupported marker: Unsupported")
  | 0x0e ->
     Error (Error.of_string "Unsupported marker: RecordSet")
  | 0x0f ->
     peel_off_long_utf8 src >>= fun (value, src) ->
     Ok (XMLDocument value, src)
  | 0x11 ->
     peel_off_utf8 src >>= fun (class_name, src) ->
     let rec iter rev_props src =
       peel_off_object_property_opt src >>= fun (next, src) ->
       match next with
       | None      -> Ok (List.rev rev_props, src)
       | Some next ->
          iter (next :: rev_props) src
     in
     iter [] src >>= fun (properties, src) ->
     Ok (TypedObject { class_name; properties; }, src)
  | 0x12 ->
     raise UpgradeToAMFV3
  | tag ->
     Error (Error.of_string (sprintf "Unrecognized marker: 0x%x" tag))

let of_buffer src =
  let open Result in
  peel_off_buffer src >>= fun (item, res) ->
  if res <> ""
  then Error (Error.of_string "Not end of buffer")
  else Ok item

let list_of_buffer src =
  let open Result in
  let rec iter res src =
    if src = ""
    then Ok (List.rev res)
    else peel_off_buffer src >>= fun (next, src) ->
         iter (next :: res) src in
  iter [] src

let array_of_buffer src =
  let open Result in
  let ( >|= ) a b =
    a >>= (fun value -> Ok (b value)) in
  list_of_buffer src
  >|= Array.of_list
  >|= (fun array -> StrictArray array)

let buffer_of_list lst =
  List.map ~f:to_buffer lst
  |> List.fold ~init:"" ~f:( ^ )

let peel_off_list str =
  let rec iter thus_far str =
    if str = ""
    then
      let thus_far = List.rev thus_far in
      Result.Ok thus_far
    else
      match peel_off_buffer str with
      | Result.Error err            -> Result.Error (thus_far, str, err)
      | Result.Ok (item, remaining) -> iter (item :: thus_far) remaining
  in
  iter [] str
