let indent_single = "  "

let bprintf ?(indent_level : int = 0) buffer fmt =
  for _ = 0 to indent_level - 1 do
    Buffer.add_string buffer indent_single
  done;
  Printf.bprintf buffer fmt
