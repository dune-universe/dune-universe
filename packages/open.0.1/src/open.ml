type os =
  | MacOS
  | Linux

let detect_os () : os =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  close_in ic;
  match uname with
  | "Darwin" -> MacOS
  | "Linux" -> Linux
  | _ -> failwith "unknown operating system"

let open_cmd =
  match detect_os () with
  | MacOS -> "open"
  | Linux -> "xdg-open"

let in_default_app file =
  let silence = "&> /dev/null" in
  Format.sprintf "%s %s %s" open_cmd file silence
  |> Unix.system
  |> ignore
