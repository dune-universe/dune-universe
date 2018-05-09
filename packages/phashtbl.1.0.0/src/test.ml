open Printf

module GKGV = Phashtbl.GenKeyToGenVal

let test_0 () =
  let module Ht = GKGV in
  let key, value = 1, 2 in
  let tmp_fn = Filename.temp_file "" "" in
  let ht = Ht.open_new tmp_fn in
  assert(not (Ht.mem ht key));
  Ht.add ht key value;
  assert(Ht.mem ht key);
  assert(Ht.find ht key = value);
  Ht.iter (fun k v -> printf "%d -> %d\n%!" k v) ht;
  let acc = Ht.fold (fun k v acc -> (k, v) :: acc) ht [] in
  assert(acc = [(key,value)]);
  Ht.close ht;
  let ht1 = Ht.open_existing tmp_fn in
  assert(Ht.find ht1 key = value);
  Ht.remove ht1 key;
  assert(not (Ht.mem ht1 key));
  Ht.close ht1;
  ()

module SKGV = Phashtbl.StrKeyToGenVal

let test_1 () =
  let module Ht = SKGV in
  let key, value = "1", 2 in
  let tmp_fn = Filename.temp_file "" "" in
  let ht = Ht.open_new tmp_fn in
  assert(not (Ht.mem ht key));
  Ht.add ht key value;
  assert(Ht.mem ht key);
  assert(Ht.find ht key = value);
  Ht.iter (fun k v -> printf "%s -> %d\n%!" k v) ht;
  let acc = Ht.fold (fun k v acc -> (k, v) :: acc) ht [] in
  assert(acc = [(key,value)]);
  Ht.close ht;
  let ht1 = Ht.open_existing tmp_fn in
  assert(Ht.find ht1 key = value);
  Ht.remove ht1 key;
  assert(not (Ht.mem ht1 key));
  Ht.close ht1;
  ()

module GKSV = Phashtbl.GenKeyToStrVal

let test_2 () =
  let module Ht = GKSV in
  let key, value = 1, "2" in
  let tmp_fn = Filename.temp_file "" "" in
  let ht = Ht.open_new tmp_fn in
  assert(not (Ht.mem ht key));
  Ht.add ht key value;
  assert(Ht.mem ht key);
  assert(Ht.find ht key = value);
  Ht.iter (fun k v -> printf "%d -> %s\n%!" k v) ht;
  let acc = Ht.fold (fun k v acc -> (k, v) :: acc) ht [] in
  assert(acc = [(key,value)]);
  Ht.close ht;
  let ht1 = Ht.open_existing tmp_fn in
  assert(Ht.find ht1 key = value);
  Ht.remove ht1 key;
  assert(not (Ht.mem ht1 key));
  Ht.close ht1;
  ()

module SKSV = Phashtbl.StrKeyToStrVal

let test_3 () =
  let module Ht = SKSV in
  let key, value = "1", "2" in
  let tmp_fn = Filename.temp_file "" "" in
  let ht = Ht.open_new tmp_fn in
  assert(not (Ht.mem ht key));
  Ht.add ht key value;
  assert(Ht.mem ht key);
  assert(Ht.find ht key = value);
  Ht.iter (fun k v -> printf "%s -> %s\n%!" k v) ht;
  let acc = Ht.fold (fun k v acc -> (k, v) :: acc) ht [] in
  assert(acc = [(key,value)]);
  Ht.close ht;
  let ht1 = Ht.open_existing tmp_fn in
  assert(Ht.find ht1 key = value);
  Ht.remove ht1 key;
  assert(not (Ht.mem ht1 key));
  Ht.close ht1;
  ()

let main () =
  test_0 ();
  test_1 ();
  test_2 ();
  test_3 ()

let () = main ()
