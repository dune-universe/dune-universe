let _ =
  if Array.length Sys.argv < 2 then
    failwith (Format.sprintf "usage: %s <log file>" Sys.argv.(0)) ;
  let file = Sys.argv.(1) in
  let chan = open_in file in
  let msgs = Opazl.Parser.from_channel chan in
  close_in chan ;
  Opazl.Pp.fprintf_file Format.std_formatter msgs
