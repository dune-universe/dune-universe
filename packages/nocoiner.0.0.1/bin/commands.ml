open Helpers

let commit ?(force = false) commit_file open_file =
  let secret = read_all stdin in
  if (not force) && Sys.file_exists commit_file then cant_overwrite commit_file
  else if (not force) && Sys.file_exists open_file then
    cant_overwrite open_file
  else
    let c, o = Nocoiner.commit secret in
    write_to c commit_file ; write_to o open_file

let reveal commit_file open_file =
  let c = read_from commit_file in
  let o = read_from open_file in
  let secret = Nocoiner.reveal ~commitment:c ~opening:o in
  print_endline secret ; flush stdout
