
let invalid_argf ?(prefix="") fmt =
  Printf.ksprintf invalid_arg ("%s" ^^ fmt) prefix

let string_of_list ?(show_empty=false) ~sep ~f = function
  | [] -> if show_empty then "[]" else ""
  | l  -> StringLabels.concat ~sep (ListLabels.map ~f l)
