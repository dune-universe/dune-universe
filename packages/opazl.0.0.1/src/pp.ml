open Ast

let fprintf_content fmt = function
  | Msg (usr, msg) ->
      Format.fprintf fmt "<%s> %s" usr msg
  | Notice s ->
      Format.fprintf fmt "* %s" s
  | Action s ->
      Format.fprintf fmt "*** %s" s

let pad_int fmt i = Format.fprintf fmt (if i < 10 then "0%d" else "%d") i

let fprintf_time fmt (h, m, s) =
  Format.fprintf fmt "[%a:%a:%a]" pad_int h pad_int m pad_int s

let fprintf_line fmt (time, content) =
  Format.fprintf fmt "%a %a" fprintf_time time fprintf_content content

let print_lines lines =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
    fprintf_line lines

let fprintf_file fmt file = Format.fprintf fmt "%a@." print_lines file
