
let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

(* get the first line output by given command *)
let get_command_output ?(debug = false) (cmd: string): string =
  if debug then
    Printf.printf "get_command_output: %s" cmd;
  let _stat, output = BatUnix.run_and_read cmd in
  match BatString.split_on_char '\n' output with
  | first_line :: _others -> first_line
  | [] ->
    begin
      Printf.eprintf "get_command_output: no output for: %s" cmd;
      exit 1
    end
