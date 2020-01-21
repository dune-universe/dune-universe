open Printf

let n = 1_000_000

let test_rwdb () =
  let module Rwdb = Dokeysto_camltc.Db_camltc.RW in
  let start = Unix.gettimeofday () in
  let rw = Rwdb.create "rwdb_camltc" in
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
  (* test iter *)
  let count = ref 0 in
  Rwdb.iter (fun k v ->
      assert(k = v);
      incr count
    ) rw;
  assert(!count = n);
  (* test fold *)
  let ref_acc = ref [] in
  for i = 1 to n do
    let s = string_of_int i in
    ref_acc := (s, s) :: !ref_acc
  done;
  let acc = Rwdb.fold (fun k v acc -> (k, v) :: acc) rw [] in
  assert(List.length acc = n);
  assert(List.sort compare !ref_acc = List.sort compare acc);
  let _ = Sys.command "ls -lh rwdb_camltc" in
  Rwdb.close rw

let test_rwdb_gen () =
  let module Rwdb_gen = Dokeysto_camltc.Db_camltc_gen.RW (Dokeysto.Gen_gen) in
  let start = Unix.gettimeofday () in
  let rw = Rwdb_gen.create "rwdb_camltc_gen" in
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
  (* test iter *)
  let count = ref 0 in
  Rwdb_gen.iter (fun k v ->
      assert(k = v);
      incr count
    ) rw;
  assert(!count = n);
  (* test fold *)
  let ref_acc = ref [] in
  for i = 1 to n do
    ref_acc := (i, i) :: !ref_acc
  done;
  let acc = Rwdb_gen.fold (fun k v acc -> (k, v) :: acc) rw [] in
  assert(List.length acc = n);
  assert(List.sort compare !ref_acc = List.sort compare acc);
  let _ = Sys.command "ls -lh rwdb_camltc_gen" in
  Rwdb_gen.close rw

let test_rodb () =
  let module Rodb = Dokeysto_camltc.Db_camltc.RO in
  let ro = Rodb.open_existing "rwdb_camltc" in
  let start''' = Unix.gettimeofday () in
  let count = ref 0 in
  for i = 1 to n do
    let s = string_of_int i in
    incr count;
    assert(s = Rodb.find ro s);
  done;
  let stop''' = Unix.gettimeofday () in
  assert(!count = n);
  printf "RO records find rate: %.2f/s\n%!" (float n /. (stop''' -. start'''));
  Rodb.close ro

let test_rodb_gen () =
  let module Rodb_gen = Dokeysto_camltc.Db_camltc_gen.RO (Dokeysto.Gen_gen) in
  let ro = Rodb_gen.open_existing "rwdb_camltc_gen" in
  let start''' = Unix.gettimeofday () in
  let count = ref 0 in
  for i = 1 to n do
    incr count;
    assert(i = Rodb_gen.find ro i);
  done;
  let stop''' = Unix.gettimeofday () in
  assert(!count = n);
  printf "RO_gen records find rate: %.2f/s\n%!" (float n /. (stop''' -. start'''));
  Rodb_gen.close ro

let () =
  let module Rwdb = Dokeysto_camltc.Db_camltc.RW in
  let module Rodb = Dokeysto_camltc.Db_camltc.RO in
  test_rwdb ();
  test_rwdb_gen ();
  test_rodb ();
  test_rodb_gen ();
  let rw = Rwdb.open_existing "rwdb_camltc" in
  Rwdb.destroy rw;
  assert(not (Sys.file_exists "rwdb_camltc"));
  let dummy_rw = Rwdb.dummy () in
  let _dummy_ro = Rodb.dummy () in
  Rwdb.destroy dummy_rw
