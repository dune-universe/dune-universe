(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
*)

let to_hex (b:Bytes.t) =
  let n_chars = Bytes.length b * 3 in
  let buf = Buffer.create n_chars in
  let hex c = Printf.sprintf "%02x " (Char.code c) in
  Bytes.iter (fun c -> Buffer.add_string buf (hex c)) b;
  Buffer.sub buf 0 (n_chars - 1)

let trimmed x =
  let x', post =
    let len = Bytes.length x in
    if len < 20
    then x, "" else
      (Bytes.sub x 0 20), Printf.sprintf "... (%i bytes)" len
  in
  Printf.sprintf "0x%S%s" (to_hex x')  post


let unwrap_option msg = function
  | None -> failwith ("None " ^ msg)
  | Some x -> x

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let get_option default = function
  | None -> default
  | Some x -> x

let show_option x2s = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" (x2s x)

let show_pair   x2s y2s (x,y) = Printf.sprintf "(%s, %s)" (x2s x) (y2s y)
let show_tuple3 x2s y2s z2s (x,y,z) = Printf.sprintf "(%s, %s, %s)" (x2s x) (y2s y) (z2s z)

let bl2s keys =
  List.map Bytes.to_string keys
  |> String.concat ";"

let so2s   = show_option (fun x -> x)
let so2hs  = show_option (fun x -> Printf.sprintf "0x%s" (to_hex x))
let bo2s   = show_option (function | true -> "true" | false -> "false")
let i64o2s = show_option Int64.to_string

let (>>=?) = Lwt_result.bind

let section = Lwt_log.Section.make "kinetic"
let tracing = Lwt_log.Section.make "tracing"
