open Printf

module Bdb = Camltc.Bdb

let nuke db fn =
  Bdb.close db;
  Bdb.delete db;
  Sys.remove fn

let () =
  let fn = "/tmp/test.tc" in
  let db = Bdb.create fn [] in
  Bdb.put db "key" "value";
  Bdb.put db "key0" "value0";
  Bdb.put db "key1" "value1";
  Bdb.put db "key2" "value2";
  assert(Bdb.get db "key" = "value");
  let cur = Bdb.get_cursor db in
  Bdb.last db cur;
  assert(Bdb.key db cur = "key2");
  assert(Bdb.value db cur = "value2");
  let n = Int64.to_int (Bdb.get_key_count db) in
  Bdb.first db cur;
  for i = 1 to n do
    let k = Bdb.key db cur in
    let v = Bdb.value db cur in
    printf "%s:%s\n" k v;
    if i < n then
      Bdb.next db cur
  done;
  nuke db fn;
  assert(not (Sys.file_exists fn));
  printf "OK\n"
