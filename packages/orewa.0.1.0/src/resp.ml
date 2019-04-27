open Core
open Async

type redis_error = string [@@deriving show, eq]

let crlf = "\r\n"

type t =
  | String of string
  | Error of redis_error
  | Integer of int
  | Bulk of string
  | Array of t list
  | Null
[@@deriving show, eq]

let terminator = function
  | true -> crlf
  | false -> ""

let rec encode = function
  | String s -> Printf.sprintf "+%s%s" s crlf
  | Error e -> Printf.sprintf "-%s%s" e crlf
  | Integer n -> Printf.sprintf ":%d%s" n crlf
  | Bulk s ->
      let len = String.length s in
      Printf.sprintf "$%d%s%s%s" len crlf s crlf
  | Array xs ->
      let payload = xs |> List.map ~f:encode |> String.concat in
      let len = List.length xs in
      Printf.sprintf "*%d%s%s%s" len crlf payload crlf
  | Null -> Printf.sprintf "$-1%s" crlf
