let convert_json_serializer (type a) json_serializer =
  let open Serializer in
  let module Json_Serializer = (val json_serializer : Json_serializer with type t = a) in
  (module struct 
    type t = a
    let t_of_string = Json_Serializer.t_of_string
    let string_of_t = Json_Serializer.string_of_t ~len:1024
  end : Serializer.Generic_serializer with type t = a)

let convert_biniou_serializer (type a) biniou_serializer = 
  let open Serializer in
  let module Biniou_Serializer = (val biniou_serializer : Biniou_serializer with type t = a) in
  (module struct 
    type t = a
    let t_of_string = Biniou_Serializer.t_of_string ~pos:0
    let string_of_t = Biniou_Serializer.string_of_t ~len:1024
  end : Serializer.Generic_serializer with type t = a)