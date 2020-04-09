open Core
open Top
open Utility

let unlines xs =
  String.concat ~sep:"\n" xs

let format_wordset env ws =
  let ws = Wordset.to_list ws in
  match env.Env.op with
  | Anagram -> sort_by caps_in ws
  | Build -> sort_by String.length ws
  | _ -> ws

let format_error e =
  "Error: " ^ e

let format_exception e =
  "Exception: " ^ (Exn.to_string e)
