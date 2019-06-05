module String = Core.String

let __split ~on commitment =
  match String.split ~on commitment with
  | [left; right] ->
      (left, right)
  | _ ->
      failwith "Failed to decode data!"
