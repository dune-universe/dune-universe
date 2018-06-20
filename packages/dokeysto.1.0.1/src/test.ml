open Printf

module Rwdb = Dokeysto.Db.RW
module Rwdbz = Dokeysto.Db.RWZ
module Rodb = Dokeysto.Db.RO
module Rodbz = Dokeysto.Db.ROZ

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

(* RWDBZ *)
let test_rwdbz () =
  let start = Unix.gettimeofday () in
  let rwz = Rwdbz.create "rwdbz" in
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
  let _ = Sys.command "ls -lh rwdbz rwdbz.idx" in
  Rwdbz.close rwz

(* RODBZ *)
let test_rodbz () =
  let roz = Rodbz.open_existing "rwdbz" in
  let start''' = Unix.gettimeofday () in
  for i = 1 to n do
    let s = string_of_int i in
    assert(s = Rodbz.find roz s);
  done;
  let stop''' = Unix.gettimeofday () in
  printf "ROZ records find rate: %.2f/s\n%!" (float n /. (stop''' -. start'''))

let () =
  test_rwdb ();
  test_rodb ();
  test_rwdbz ();
  test_rodbz ()
