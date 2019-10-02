let output_line_number out_channel fname lnum =
  Printf.fprintf out_channel "\n# %d \"%s\"\n" lnum fname

let output_position out_channel { Lexing.pos_fname; pos_lnum } =
  output_line_number out_channel pos_fname pos_lnum
