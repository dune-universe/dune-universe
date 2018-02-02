module Json_of_string = Json_of_jsonm_monad.Make(struct
    type 'a t = 'a

    let return a = a
    let (>>=) a f = f a
  end)

type json = Json_of_string.json

let json_of_string = Json_of_string.decode_string
let json_of_string_exn = Json_of_string.decode_string_exn
let json_to_string = Json_of_string.encode_string
let json_to_string_exn = Json_of_string.encode_string_exn
let json_to_string_hum = Json_of_string.encode_string_hum

type t = Json_of_string.json

let of_string t = json_of_string_exn t
let to_string t = json_to_string_exn t
