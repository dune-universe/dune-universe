open Core

let compile_dot ?(format="pdf") ?(engine="dot") ?(title=engine) data : string =
  let output_file = Filename.temp_file title ("." ^ format) in
  let cmd = sprintf "dot -T%s -o %s" format output_file in
  let dot_process = Unix.open_process_out cmd in
  Out_channel.output_string dot_process data;
  Out_channel.close dot_process;
  ignore (Unix.close_process_out dot_process);
  output_file
;;

In_channel.read_all "unix.dot"
|> compile_dot
|> Open.in_default_app
|> Format.printf "file opened successfully: %b\n"
;;
