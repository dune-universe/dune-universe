open! Core
open! Core_bench
open Share

let () =
  let bench name f = Bench.Test.create ~name (fun () -> f ()) in
  Command.run
    (Bench.make_command
       [
         bench "get_record_list" get_record_list;
         bench "total_length" total_length;
         bench "print_records" print_records;
         bench "print_recordsi" print_recordsi;
       ])
