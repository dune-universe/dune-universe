
open Printf

module Rwdb = Dokeysto.Db.RW
module Rodb = Dokeysto.Db.RO

module Rwdb_gen = Dokeysto.Db_gen.RW (Dokeysto.Gen_gen)
module Rodb_gen = Dokeysto.Db_gen.RO (Dokeysto.Gen_gen)

let n = 1_000_000

(* RWDB *)
let test_rwdb () =
  let start = Unix.gettimeofday () in
  let rw = Rwdb.create "rwdb" in
  for i = 1 to n do
    let s = string_of_int i in
    Rwdb.add rw s s
  done;
  let stop = Unix.gettimeofday () in
  printf "RW records creation rate: %.2f/s\n%!" (float n /. (stop -. start));
  Rwdb.sync rw;
  let start' = Unix.gettimeofday () in
  for i = 1 to n do
    let s = string_of_int i in
    assert(s = Rwdb.find rw s);
  done;
  let stop' = Unix.gettimeofday () in
  printf "RW records find rate: %.2f/s\n%!" (float n /. (stop' -. start'));
  let _ = Sys.command "ls -lh rwdb rwdb.idx" in
  Rwdb.close rw

let test_rwdb_gen () =
  let start = Unix.gettimeofday () in
  let rw = Rwdb_gen.create "rwdb_gen" in
  for i = 1 to n do
    Rwdb_gen.add rw i i
  done;
  let stop = Unix.gettimeofday () in
  printf "RW_gen records creation rate: %.2f/s\n%!" (float n /. (stop -. start));
  Rwdb_gen.sync rw;
  let start' = Unix.gettimeofday () in
  for i = 1 to n do
    assert(i = Rwdb_gen.find rw i);
  done;
  let stop' = Unix.gettimeofday () in
  printf "RW_gen records find rate: %.2f/s\n%!" (float n /. (stop' -. start'));
  let _ = Sys.command "ls -lh rwdb_gen rwdb_gen.idx" in
  Rwdb_gen.close rw

(* RODB *)
let test_rodb () =
  let start = Unix.gettimeofday () in
  let ro = Rodb.open_existing "rwdb" in
  for i = 1 to n do
    let s = string_of_int i in
    assert(s = Rodb.find ro s);
  done;
  let stop = Unix.gettimeofday () in
  printf "RO records find rate: %.2f/s\n%!" (float n /. (stop -. start));
  Rodb.close ro

let test_rodb_gen () =
  let start = Unix.gettimeofday () in
  let ro = Rodb_gen.open_existing "rwdb_gen" in
  for i = 1 to n do
    assert(i = Rodb_gen.find ro i);
  done;
  let stop = Unix.gettimeofday () in
  printf "RO_gen records find rate: %.2f/s\n%!" (float n /. (stop -. start));
  Rodb_gen.close ro

let () =
  test_rwdb ();
  test_rwdb_gen ();
  test_rodb ();
  test_rodb_gen ()
