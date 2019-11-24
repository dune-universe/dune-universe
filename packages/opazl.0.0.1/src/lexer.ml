open Ast

let digit = [%sedlex.regexp? '0' .. '9']

let digit2 = [%sedlex.regexp? digit, digit]

let eol = [%sedlex.regexp? '\n']

let any_except_eol = [%sedlex.regexp? Star (Sub (any, '\n'))]

let any_except_eol_eon = [%sedlex.regexp? Star (Sub (Sub (any, '\n'), '>'))]

let lxm = Sedlexing.Utf8.lexeme

let extract_time s = Scanf.sscanf s "[%d:%d:%d]" (fun x y z -> (x, y, z))

let extract_sub useless start s =
  let len = String.length s - useless in
  if len < 1 then "" else String.sub s start len

let extract_user = extract_sub 4 2

let extract_notice = extract_sub 6 5

let extract_action = extract_sub 4 3

let extract_msg = extract_sub 1 0

let rec file buf =
  match%sedlex buf with
  | '[', digit2, ':', digit2, ':', digit2, ']' ->
      let time = extract_time (lxm buf) in
      let content = line_content buf in
      Some (time, content)
  | eof ->
      None
  | _ ->
      failwith "unexpected character (file)"

and line_content buf =
  match%sedlex buf with
  | " <", any_except_eol_eon, "> " ->
      let user = extract_user (lxm buf) in
      let msg = msg buf in
      Msg (user, msg)
  | " *** ", any_except_eol, eol ->
      Notice (extract_notice (lxm buf))
  | " * ", any_except_eol, eol ->
      Action (extract_action (lxm buf))
  | _ ->
      failwith "unexpected character (line_content)"

and msg buf =
  match%sedlex buf with
  | any_except_eol, eol ->
      extract_msg (lxm buf)
  | _ ->
      failwith "unexpected character (msg)"
