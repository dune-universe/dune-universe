let hash_size = 32

let b = Bytes.of_string

let with_time f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  res, (t2 -. t1)

let load_file filename =
  let ch = open_in_bin filename in
  let size = in_channel_length ch in
  let body = really_input_string ch size in
  close_in ch;
  Bytes.of_string body

let hex_string bs =
  let list_of_bytes bs =
    let len = Bytes.length bs in
    List.init len (fun i -> Bytes.get bs i)
  in
  list_of_bytes bs
  |> List.map int_of_char
  |> List.map (Printf.sprintf "%02x")
  |> String.concat ""

(* expected result *)
let b3sum filename =
  let command = "b3sum " ^ filename in
  let ch = Unix.open_process_in command in
  let result = input_line ch in
  ignore (Unix.close_process_in ch);
  String.split_on_char ' ' result
  |> List.hd

let small_test () =
  let h = Blake3.hash hash_size @@ b"Hello\n" in
  let expected =
    (*
       $ echo Hello | b3sum -
       38d5445421bfd60d4d48ff2a7acb3ed412e43e68e66cdb2bb86f604ec6e6caa0  -
    *)
    "38d5445421bfd60d4d48ff2a7acb3ed412e43e68e66cdb2bb86f604ec6e6caa0"
  in
  assert (expected = hex_string h)

let blake2 hash_size inbuf =
  let open Hacl_star in
  let key = Bytes.create 0 in
  let outbuf = Bytes.create hash_size in
  Hacl.Blake2b_32.hash key inbuf outbuf;
  Bytes.unsafe_to_string outbuf

(* compare with b3sum *)
let file_test () =
  let open Unix in
  let fn = Filename.temp_file "random" "data" in
  let bytes = Bytes.create 1000000000 (* 1GiB *) in
  let fd = openfile fn [O_CREAT; O_RDWR] 0o666 in
  ignore @@ write fd bytes 0 1000000000;
  close fd;
  let bytes = Bytes.create 1000000000 (* 1GiB *) in
  let h1, time1 = with_time @@ fun () -> Blake3.hash_multicore 32 bytes in
  let hex1 = hex_string h1 in
  let hex2, time2 = with_time @@ fun () -> b3sum fn in
  if hex2 <> hex1 then begin
    Printf.eprintf "---\nexpected: %s\nactual:   %s\n" hex2 hex1;
    assert false
  end;
  Printf.printf "1GiB hash: blake3_multicore %f b3sum %f\n%!"
    time1 time2

let measure inputsz outputsz =
  let bytes = Bytes.create 4294967296 in
  let size = min 100000 (max 1 (1000000 * 60 / inputsz)) in
  let inputs = List.init size (fun i -> Bytes.sub bytes i inputsz) in
  let _, time_b3m = with_time @@ fun () -> List.iter (fun s -> ignore @@ Blake3.hash_multicore outputsz s) inputs in
  let _, time_b3s = with_time @@ fun () -> List.iter (fun s -> ignore @@ Blake3.hash outputsz s) inputs in
  let _, time_b2 = with_time @@ fun () -> List.iter (fun s -> ignore @@ blake2 outputsz s) inputs in
  Format.printf "%d, %d, %f, %f, %f, %d@."
    inputsz outputsz
    (time_b3m *. 1000000.0 /. float size)
    (time_b3s *. 1000000.0 /. float size)
    (time_b2  *. 1000000.0 /. float size)
    size

let () =
  small_test ();
  file_test ();
  Printf.printf "input-bytes, output-bytes, blake3multi-microsecs, blake3single-microsecs, blake2-microsecs, iteration\n%!";

  measure 60 28;
  measure 60 32;
  List.iter (fun inputsz -> measure inputsz 32)
    (List.init 24 (fun x -> 256 * (1 lsl x)))
