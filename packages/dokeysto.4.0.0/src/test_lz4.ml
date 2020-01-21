open Printf

let n = 1_000_000

(* RWDBZ *)
let test_rwdbz () =
  let module Rwdbz = Dokeysto_lz4.Db_lz4.RWZ in
  let start = Unix.gettimeofday () in
  let rwz = Rwdbz.create "rwdb_lz4" in
  for i = 1 to n do
    let s = string_of_int i in
    Rwdbz.add rwz s s
  done;
  let stop = Unix.gettimeofday () in
  printf "RWZ records creation rate: %.2f/s\n%!" (float n /. (stop -. start));
  Rwdbz.sync rwz;
  let start' = Unix.gettimeofday () in
  for i = 1 to n do
    let s = string_of_int i in
    assert(s = Rwdbz.find rwz s);
  done;
  let stop' = Unix.gettimeofday () in
  printf "RWZ records find rate: %.2f/s\n%!" (float n /. (stop' -. start'));
  let _ = Sys.command "ls -lh rwdb_lz4 rwdb_lz4.idx" in
  Rwdbz.close rwz

let test_rwdbz_gen () =
  let module Rwdbz_gen = Dokeysto_lz4.Db_lz4_gen.RWZ (Dokeysto.Gen_gen) in
  let start = Unix.gettimeofday () in
  let rwz = Rwdbz_gen.create "rwdb_lz4_gen" in
  for i = 1 to n do
    Rwdbz_gen.add rwz i i
  done;
  let stop = Unix.gettimeofday () in
  printf "RWZ_gen records creation rate: %.2f/s\n%!" (float n /. (stop -. start));
  Rwdbz_gen.sync rwz;
  let start' = Unix.gettimeofday () in
  for i = 1 to n do
    assert(i = Rwdbz_gen.find rwz i);
  done;
  let stop' = Unix.gettimeofday () in
  printf "RWZ_gen records find rate: %.2f/s\n%!" (float n /. (stop' -. start'));
  let _ = Sys.command "ls -lh rwdb_lz4_gen rwdb_lz4_gen.idx" in
  Rwdbz_gen.close rwz

(* RODBZ *)
let test_rodbz () =
  let module Rodbz = Dokeysto_lz4.Db_lz4.ROZ in
  let roz = Rodbz.open_existing "rwdb_lz4" in
  let start''' = Unix.gettimeofday () in
  for i = 1 to n do
    let s = string_of_int i in
    assert(s = Rodbz.find roz s);
  done;
  let stop''' = Unix.gettimeofday () in
  printf "ROZ records find rate: %.2f/s\n%!" (float n /. (stop''' -. start'''))

let test_rodbz_gen () =
  let module Rodbz_gen = Dokeysto_lz4.Db_lz4_gen.ROZ (Dokeysto.Gen_gen) in
  let roz = Rodbz_gen.open_existing "rwdb_lz4_gen" in
  let start''' = Unix.gettimeofday () in
  for i = 1 to n do
    assert(i = Rodbz_gen.find roz i);
  done;
  let stop''' = Unix.gettimeofday () in
  printf "ROZ_gen records find rate: %.2f/s\n%!" (float n /. (stop''' -. start'''))

let () =
  test_rwdbz ();
  test_rwdbz_gen ();
  test_rodbz ();
  test_rodbz_gen ()
