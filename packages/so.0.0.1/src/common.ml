let error s = Format.eprintf "%s@." s ; exit 1

let check_file () =
  if Array.length Sys.argv <> 2 then
    error (Format.sprintf "usage: %s <file>" Sys.argv.(0)) ;
  let file = Sys.argv.(1) in
  if not (Sys.file_exists file) then
    error (Format.sprintf "file %s doesn't exist" file) ;
  file

let check_config_file () =
  let config_file = XDGBaseDir.default.config_home ^ "/so/config" in
  if not (Sys.file_exists config_file) then
    error (Format.sprintf "config file %s doesn't exist" config_file) ;
  config_file

let load_config_file config_file =
  let chan = open_in config_file in
  let tbl = Hashtbl.create 2048 in
  ( try
      while true do
        let line = input_line chan in
        match String.split_on_char ':' line with
        | [prog; exts] ->
            let prog = String.trim prog in
            let exts = String.split_on_char ' ' exts in
            List.iter
              (fun el -> if el <> "" then Hashtbl.add tbl ("." ^ el) prog)
              exts
        | _ ->
            error (Format.sprintf "parse error in config file %s" config_file)
      done
    with End_of_file -> close_in chan ) ;
  tbl

let find_cmd file tbl =
  let ext = Filename.extension file in
  match Hashtbl.find tbl ext with
  | exception Not_found ->
      "xdg-open"
  | prog ->
      prog
