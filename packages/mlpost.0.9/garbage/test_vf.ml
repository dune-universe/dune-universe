open Dvi

let () =
  let vf = read_vf_file Sys.argv.(1) in
  Format.printf "%a@." print_vf vf
