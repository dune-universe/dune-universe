open Reed_solomon_erasure
open Reed_solomon_erasure__.Ops
module Bigstring = Core_kernel.Bigstring

let make_random_shards_bytes per_shard size =
  Array.init
    size
    (fun _ ->
       Bytes.init
         per_shard
         (fun _ -> char_of_int (Random.int 256)))

let make_random_shards_bigstr per_shard size =
  Array.init
    size
    (fun _ ->
       Bigstring.init
         per_shard
         ~f:(fun _ -> char_of_int (Random.int 256)))

let fill_random slice =
  for i = 0 to (Bytes.length slice) - 1 do
    slice.%(i) <- char_of_int (Random.int 256)
  done

let benchmark_encode_bigstr
    (iterations    : int)
    (data_shards   : int)
    (parity_shards : int)
    (per_shard     : int)
  : unit =
  let shards = make_random_shards_bigstr per_shard (data_shards + parity_shards) in

  let r = ReedSolomon.make data_shards parity_shards in

  let time_start = Unix.gettimeofday () in

  for _ = 0 to (iterations) - 1 do
    ReedSolomon.encode_bigstr r shards
  done;

  let time_end   = Unix.gettimeofday () in
  let time_taken = (time_end -. time_start) in
  let byte_count = (iterations * per_shard *  data_shards) in
  Printf.printf "encode_bigstr :\n";
  Printf.printf "    shards           : %d / %d\n" data_shards parity_shards;
  Printf.printf "    shard length     : %d\n" per_shard;
  Printf.printf "    time taken       : %f\n" time_taken;
  Printf.printf "    byte count       : %d\n" byte_count;
  Printf.printf "    MB/s             : %f\n" ((float_of_int byte_count) /. 1_048_576.0 /. time_taken)

let benchmark_encode_sep_bigstr
    (iterations    : int)
    (data_shards   : int)
    (parity_shards : int)
    (per_shard     : int)
  : unit =
  let data   = make_random_shards_bigstr per_shard data_shards in
  let parity = make_random_shards_bigstr per_shard parity_shards in

  let r = ReedSolomon.make data_shards parity_shards in

  let time_start = Unix.gettimeofday () in

  for _ = 0 to (iterations) - 1 do
    ReedSolomon.encode_sep_bigstr r data parity
  done;

  let time_end   = Unix.gettimeofday () in
  let time_taken = (time_end -. time_start) in
  let byte_count = (iterations * per_shard *  data_shards) in
  Printf.printf "encode_sep_bigstr :\n";
  Printf.printf "    shards           : %d / %d\n" data_shards parity_shards;
  Printf.printf "    shard length     : %d\n" per_shard;
  Printf.printf "    time taken       : %f\n" time_taken;
  Printf.printf "    byte count       : %d\n" byte_count;
  Printf.printf "    MB/s             : %f\n" ((float_of_int byte_count) /. 1_048_576.0 /. time_taken)

let benchmark_encode_bytes
    (iterations    : int)
    (data_shards   : int)
    (parity_shards : int)
    (per_shard     : int)
  : unit =
  let shards = make_random_shards_bytes per_shard (data_shards + parity_shards) in

  let r = ReedSolomon.make data_shards parity_shards in

  let time_start = Unix.gettimeofday () in

  for _ = 0 to (iterations) - 1 do
    ReedSolomon.encode_bytes r shards
  done;

  let time_end   = Unix.gettimeofday () in
  let time_taken = (time_end -. time_start) in
  let byte_count = (iterations * per_shard *  data_shards) in
  Printf.printf "encode_bytes :\n";
  Printf.printf "    shards           : %d / %d\n" data_shards parity_shards;
  Printf.printf "    shard length     : %d\n" per_shard;
  Printf.printf "    time taken       : %f\n" time_taken;
  Printf.printf "    byte count       : %d\n" byte_count;
  Printf.printf "    MB/s             : %f\n" ((float_of_int byte_count) /. 1_048_576.0 /. time_taken)

let benchmark_encode_str
    (iterations    : int)
    (data_shards   : int)
    (parity_shards : int)
    (per_shard     : int)
  : unit =
  let shards = make_random_shards_bytes per_shard (data_shards + parity_shards) in

  let r = ReedSolomon.make data_shards parity_shards in

  let time_start = Unix.gettimeofday () in

  for _ = 0 to (iterations) - 1 do
    ReedSolomon.encode_bytes r shards
  done;

  let time_end   = Unix.gettimeofday () in
  let time_taken = (time_end -. time_start) in
  let byte_count = (iterations * per_shard *  data_shards) in
  Printf.printf "encode_str :\n";
  Printf.printf "    shards           : %d / %d\n" data_shards parity_shards;
  Printf.printf "    shard length     : %d\n" per_shard;
  Printf.printf "    time taken       : %f\n" time_taken;
  Printf.printf "    byte count       : %d\n" byte_count;
  Printf.printf "    MB/s             : %f\n" ((float_of_int byte_count) /. 1_048_576.0 /. time_taken)

let () =
  (* benchmark_encode_sep_bigstr 100 10 2 1_048_576; *)
  benchmark_encode_bigstr 100 10 2 1_048_576;
  (* benchmark_encode_bytes  100 10 2 1_048_576; *)
  (* benchmark_encode_str    100 10 2 1_048_576; *)
