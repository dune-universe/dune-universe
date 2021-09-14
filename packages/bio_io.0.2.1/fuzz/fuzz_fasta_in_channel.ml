open! Base
open Bio_io

let abort ?(exit_code = 1) msg =
  let () = Stdio.eprintf "%s\n" msg in
  Caml.exit exit_code

let argv = Sys.get_argv ()

let fname =
  match Array.length argv with
  | 2 -> argv.(1)
  | _ -> abort "usage: fuzz_fasta_in_channel.exe seqs.fa"

let seqs = Fasta_in_channel.with_file_records fname
