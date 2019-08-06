module String = Core.String

let __split ~on payload =
  match String.split ~on payload with
  | [ metadata; iv; cipher; tag ] ->
      (metadata, iv, cipher, tag)
  | _ ->
      failwith "Failed to decode data!"
