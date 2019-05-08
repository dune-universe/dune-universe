open Core
open Async

let read_char reader =
  match%map Reader.read_char reader with
  | `Ok c -> Ok c
  | `Eof -> Error `Connection_closed

let read_string reader =
  match%map Reader.read_line reader with
  | `Eof -> Error `Connection_closed
  | `Ok s -> Ok s

let flush_line reader = read_string reader |> Deferred.Result.map ~f:ignore

let read_int reader = read_string reader |> Deferred.Result.map ~f:int_of_string

let read_bulk ~len reader =
  let s = Bytes.create len in
  match%map Reader.really_read reader s with
  | `Eof _ -> Error `Connection_closed
  | `Ok -> Ok (Bytes.to_string s)

let rec read_resp reader =
  let open Deferred.Result.Let_syntax in
  match%bind read_char reader with
  | '+' -> read_string reader |> Deferred.Result.map ~f:(fun s -> Resp.String s)
  | '-' -> read_string reader |> Deferred.Result.map ~f:(fun s -> Resp.Error s)
  | ':' -> read_int reader |> Deferred.Result.map ~f:(fun i -> Resp.Integer i)
  | '$' -> (
      match%bind read_int reader with
      | -1 ->
          (* No extra newline *)
          return Resp.Null
      | len ->
          let%bind data = read_bulk ~len reader in
          let%bind () = flush_line reader in
          return (Resp.Bulk data) )
  | '*' ->
      let%bind length = read_int reader in
      let rec inner acc = function
        | 0 -> return acc
        | n ->
            let%bind elem = read_resp reader in
            inner (elem :: acc) (n - 1)
      in
      let%bind elements = inner [] length in
      return (Resp.Array (List.rev elements))
  | unknown ->
      Log.Global.debug "Unparseable type tag %C" unknown;
      Deferred.return @@ Error `Unexpected
