module String_map = Map.Make (String)

let print_exn_at_loc loc =
  Printf.sprintf "Exception raised at %s:%d" loc.Lexing.pos_fname loc.Lexing.pos_lnum
