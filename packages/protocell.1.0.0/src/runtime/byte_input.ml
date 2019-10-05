open Base

type t = {
  bytes : string;
  byte_count : int;
  position : int ref;
}

type error = [`Not_enough_bytes]

let create bytes = {bytes; byte_count = String.length bytes; position = ref 0}

let has_more_bytes {byte_count; position; _} = !position < byte_count

let read_byte ({bytes; position; _} as stream) =
  match has_more_bytes stream with
  | true ->
      let result = bytes.[!position] in
      Int.incr position; Ok result
  | false -> Error `Not_enough_bytes

let read_bytes {bytes; byte_count; position} count =
  match !position + count <= byte_count with
  | true ->
      let pos = !position in
      position := !position + count;
      Ok (String.sub bytes ~pos ~len:count)
  | false -> Error `Not_enough_bytes

let read_while ({position; byte_count; bytes; _} as stream) condition =
  match has_more_bytes stream with
  | true ->
      let rec find_limit from =
        match from < byte_count with
        | true -> (
          match condition bytes.[from] with
          | true -> find_limit (Int.succ from)
          | false -> from)
        | false -> from
      in
      let from_pos = !position in
      let to_pos = find_limit from_pos in
      position := to_pos;
      String.sub bytes ~pos:from_pos ~len:(to_pos - from_pos)
  | false -> ""
