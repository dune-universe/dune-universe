type os =
  | MacOS
  | Linux
  | Windows
  | Cygwin

let detect_os () : os =
  if Sys.win32 then Windows else
  if Sys.cygwin then Cygwin else
  let in_ch,_,_ as uname = Unix.open_process_full "uname" [||] in
  let os = input_line in_ch in
  ignore (Unix.close_process_full uname);
  match os with
  | "Darwin" -> MacOS
  | "Linux" -> Linux
  | _ -> failwith "unknown operating system"

let open_cmd os =
  match os with
  | MacOS -> "open"
  | Linux -> "xdg-open"
  | Windows -> "start"
  | Cygwin -> "cygstart"

let silence os =
  match os with
  | MacOS | Linux | Cygwin-> "> /dev/null 2>&1"
  | Windows -> "> nul 2>&1"

let in_default_app file : bool =
  let os = detect_os () in
  Format.sprintf "%s %S %s" (open_cmd os) file (silence os)
  |> Unix.system
  |> function
    | Unix.WEXITED 0 -> true
    | _ -> false
