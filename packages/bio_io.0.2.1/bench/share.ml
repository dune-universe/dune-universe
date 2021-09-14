open! Core
open Bio_io

let infile = Sys.getenv_exn "BENCH_FASTA_INFILE"

let get_record_list () = Fasta_in_channel.with_file_records_exn infile

let total_length () =
  let total_length =
    Fasta_in_channel.with_file_fold_records_exn infile ~init:0
      ~f:(fun length record ->
        let sequence = Fasta_record.seq record in
        length + String.length sequence)
  in
  prerr_endline @@ "total bases: " ^ Int.to_string total_length

let print_records () =
  Fasta_in_channel.with_file_iter_records_exn infile ~f:(fun record ->
      let open Fasta_record in
      eprintf "%s => %d\n" (id record) (String.length (seq record)))

let print_recordsi () =
  Fasta_in_channel.with_file_iteri_records_exn infile ~f:(fun index record ->
      let open Fasta_record in
      eprintf "%d: %s => %d\n" (index + 1) (id record)
        (String.length (seq record)))
