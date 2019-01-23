let output_line_number out_channel fname lnum =
  Printf.fprintf out_channel "\n# %d \"%s\"\n" lnum fname

let output_position out_channel { Lexing.pos_fname; pos_lnum } =
  output_line_number out_channel pos_fname pos_lnum

let output_channel_to_the_end ?(buffer_size = 1024) out_channel in_channel =
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match input in_channel buffer 0 buffer_size with
    | 0 -> ()
    | len ->
        output out_channel buffer 0 len;
        loop () in
  loop ()
