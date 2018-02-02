module Json_of_channel = Json_of_jsonm_monad.Make(struct
    type 'a t = 'a

    let return a = a
    let (>>=) a f = f a
  end)

type json = Json_of_channel.json

let json_of_channel in_channel =
  let reader buf size = input in_channel buf 0 size in
  Json_of_channel.decode ~reader

let json_of_channel_exn in_channel =
  let reader buf size = input in_channel buf 0 size in
  Json_of_channel.decode_exn ~reader

let json_to_channel out_channel =
  let writer buf size = output out_channel buf 0 size in
  Json_of_channel.encode ~writer

let json_to_channel_exn out_channel =
  let writer buf size = output out_channel buf 0 size in
  Json_of_channel.encode_exn ~writer

