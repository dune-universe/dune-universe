(*
Copyright (2010-2014) INCUBAID BVBA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

open OUnit
open Otc
open Logging
open Extra

let db_fn suff = Printf.sprintf "/tmp/db%s.tc" suff

let setup_tc suff _ =
  let db = Bdb._make () in
  let _ = Bdb._dbopen db (db_fn suff) (Bdb.owriter lor Bdb.ocreat lor Bdb.otrunc) in
    db

let teardown_tc suff db =
  let _ = Bdb._dbclose db in
  let _ = Bdb._delete db in
  let _ = Unix.unlink (db_fn suff) in
    ()

let test_make () =
  let db = Bdb._make () in
  let _ = Bdb._delete db in
    ()

let test_open db = ()

let test_basic db =
  let _ = Bdb.put db "hello" "world" in
  let v = Bdb.get db "hello" in
  let _ = Extra.eq_string "put/get" "world" v in
    ()

let test_cursor db =
  let _ = Bdb.put db "key1" "value1" in
  let _ = Bdb.put db "key2" "value2" in
  let _ = Bdb.put db "key3" "value3" in
  let cur = Bdb._cur_make db in
  let _ = Bdb.first db cur in
  let _ = eq_string "key1" "key1" (Bdb.key db cur) in
  let _ = eq_string "value1" "value1" (Bdb.value db cur) in
  let key,value = Bdb.record db cur in
  let _ = eq_string "key1" "key1" key in
  let _ = eq_string "value1" "value1" value in
  let _ = Bdb.next db cur in
  let _ = eq_string "key2" "key2" (Bdb.key db cur) in
  let _ = eq_string "value2" "value2" (Bdb.value db cur) in
  let _ = Bdb.next db cur in
  let _ = eq_string "key3" "key3" (Bdb.key db cur) in
  let _ = eq_string "value3" "value3" (Bdb.value db cur) in
  let _ = try Bdb.next db cur; assert_failure "expecting failure" with _ -> ()
  in
  let _ = Bdb._cur_delete cur in
  ()

let load db kvs = List.iter (fun (k,v) -> Bdb.put db k v) kvs

let test_range db =
  let () = load db [
    "kex1","value1";
    "key2","value2";
    "key3","value3";
    "kez3","value4";
  ]
  in
  let a = Bdb.range db (Some "key") true (Some "kez") false (-1) in
  let () = eq_int "num==2" 2 (Array.length a) in
  let () = eq_string "key2" "key2" a.(0) in
  let () = eq_string "key3" "key3" a.(1) in
  ()

let test_range2 db =
  let () = load db ["xey001","XEY001"] in
  let a = Bdb.range db None false None false (-1) in
  let () = eq_int "length should be 1" 1 (Array.length a) in
  let a' = Bdb.range db None false None false 0 in
  let () = eq_int "length should be 0" 0 (Array.length a') in
  ()

    
let test_range_entries db =
  let () = load db
    [ "@kex1", "value1";
      "@key2", "value2";
      "@key3", "value3";
      "@kez3", "value4"]
  in
  let a = Bdb.range_entries "@" db (Some "key") true (Some "kez") false (-1)
  in
  let () = eq_int "num==2" 2( Array.length a) in
  let key i = fst (a.(i)) in
  let () = eq_string "key2" "key2" (key 0) in
  let () = eq_string "key3" "key3" (key 1) in
  ()

let test_range_entries2 db =
 let () = load db
    [ "@kex1", "value1";
      "@key2", "value2";
      "@key3", "value3";
      "@kez3", "value4"]
  in
  let a = Bdb.range_entries "@" db (Some "key") true (Some "key3") false (-1)
  in
  let () = eq_int "num==1" 1 ( Array.length a) in
  let key i = fst (a.(i)) in
  let () = eq_string "key2" "key2" (key 0) in
  (* None at end *)
  let b = Bdb.range_entries "@" db (Some "key") true None false (-1) in
  let () = OUnit.assert_equal ~printer:string_of_int 3 (Array.length b) in
  let c = Bdb.range_entries "@" db None true None false (-1) in
  let () = OUnit.assert_equal ~printer:string_of_int 4 (Array.length c) in
  let d = Bdb.range_entries "@" db (Some "p") true None false (-1) in
  let () = OUnit.assert_equal ~printer:string_of_int 0 (Array.length d) in
  let e = Bdb.range_entries "f" db None false None false (-1) in
  let () = OUnit.assert_equal ~printer:string_of_int 0 (Array.length e) in
  ()

let test_range_entries3 db =
  let p = "O\001\000\000\000\000\000@\000\128\000\000\000\000\000\000\000" in
  let () =
    load db
         [ ("@" ^ p ^ "hi3.txt", "hi3");
           ("@" ^ p ^ "sub1/hi.txt", "sub1/hi.txt");
           ("@" ^ p ^ "sub1/hi2.txt","sub1/hi2.txt");
           ("@" ^ p ^ "sub2/hi2.txt","sub2/hi2.txt");
]
  in
  let k = "O\001\000\000\000\000\000@\000\128\000\000\000\000\000\000\000sub2/" in
  let a = Bdb.range_entries
    "@" db
    (Some k) true
    (Some "O\002") false (-1)
  in
  (*Array.iter (fun (k,v) -> Printf.printf "(%S,%S)\n" k v) a; *)
  OUnit.assert_equal 1 (Array.length a);
  OUnit.assert_equal (p ^ "sub2/hi2.txt", "sub2/hi2.txt") a.(0);
  ()

let test_range_entries4 db =
  let dump a =
    let () = Printf.printf "a:[\n" in
    Array.iter (fun (k,v) -> Printf.printf "%S : %S\n" k v) a;
    Printf.printf "]\n%!"
  in
  let prefix = "@" in
  let () = load db [prefix ^ "xey001","XEY001"] in
  let a2 = Bdb.range_entries prefix db None false None false 0 in
  let () = dump a2 in
  let () = eq_int "length should be 0" 0 (Array.length a2) in
  ()

let test_get3_generic db =
  let () = load db [("x","X");
                    ("xx", "XX");
                    ("xxx","XXX");
                   ] in
  let key0 = "12345xyz" in
  let v = Bdb.get3_generic db key0 5 1 in
  OUnit.assert_equal v "X";
  try
    let v = Bdb.get3_generic db key0 5 2 in
    OUnit.assert_equal v "v"
  with Not_found -> ()

let test_unknown db =
  let _ = try Bdb.get db "hello" with
    | Not_found -> ""
    | _ -> assert_failure "unexpected exception"
  in ()

let test_prefix_keys db =
  let () = load db [
    "kex1", "value1";
    "key2", "value2";
    "key3", "value3";
    "kez3", "value4"]
  in
  let a = Bdb.prefix_keys db "key" (-1) in
  let () = eq_int "num==2" 2 (Array.length a) in
  let () = eq_string "key2" "key2" a.(0) in
  let () = eq_string "key3" "key3" a.(1) in
    ()

let test_null db =
  let str = String.make 5 (char_of_int 0) in
  let () = Bdb.put db "key1" str in
  let str2 = Bdb.get db "key1" in
  let () = eq_int "length==5" 5 (String.length str2) in
  let () = eq_int "char0" 0 (int_of_char str2.[0]) in
  let () = eq_int "char0" 0 (int_of_char str2.[1]) in
  let () = eq_int "char0" 0 (int_of_char str2.[2]) in
  let () = eq_int "char0" 0 (int_of_char str2.[3]) in
  let () = eq_int "char0" 0 (int_of_char str2.[4]) in
    ()

let test_flags db =
  match Bdb.flags db with
    | [Bdb.BDBFOPEN] -> ()
    | _ -> assert_failure "Unexpected flags set"

let _test_copy_from_cursor db1 db2 max expected =
  Bdb.with_cursor db1 (fun sdb cur ->
    try
      Bdb.first sdb cur;
    with Not_found -> (* Empty DB *)
      ();
    let cnt = Bdb.copy_from_cursor sdb cur db2 max in
    OUnit.assert_equal ~printer:string_of_int expected cnt)

let test_copy_from_cursor_0 db1 db2 =
  _test_copy_from_cursor db1 db2 None 0

let _fill db =
  let rec loop = function
    | 0 -> ()
    | n -> Bdb.put db (Printf.sprintf "key%d" n) "value"; loop (n - 1)
  in
  loop

let test_copy_from_cursor_1 db1 db2 =
  _fill db1 1;
  _test_copy_from_cursor db1 db2 None 1

let test_copy_from_cursor_2 db1 db2 =
  _fill db1 2;
  _test_copy_from_cursor db1 db2 None 2

let test_copy_from_cursor_3 db1 db2 =
  _fill db1 10;
  _test_copy_from_cursor db1 db2 (Some 5) 5

let test_copy_from_cursor_4 db1 db2 =
  _fill db1 10;
  _test_copy_from_cursor db1 db2 (Some 20) 10

let test_copy_from_cursor_5 db1 db2 =
  _fill db1 100;

  let test sdb cur tdb max =
    let rec loop i c =
      let cnt = Bdb.copy_from_cursor sdb cur tdb max in
      match cnt with
        | 0 -> (i, c)
        | n -> loop (i + 1) (c + n)
    in
    loop 0 0
  in
  Bdb.with_cursor db1 (fun sdb cur ->
    Bdb.first sdb cur;
    let (iters, cnt) = test sdb cur db2 (Some 11) in
    OUnit.assert_equal ~printer:string_of_int 10 iters;
    OUnit.assert_equal ~printer:string_of_int 100 cnt)

let test_copy_from_cursor_6 db1 db2 =
  _fill db1 100;

  let cnt = Bdb.with_cursor db1 (fun sdb cur ->
    Bdb.first sdb cur;
    Bdb.copy_from_cursor sdb cur db2 None) in
  OUnit.assert_equal ~printer:string_of_int 100 cnt;

  let r1 = Bdb.range_entries "k" db1 None true None true (-1) in
  let r2 = Bdb.range_entries "k" db2 None true None true (-1) in
  OUnit.assert_equal ~printer:string_of_int 100 (Array.length r1);
  OUnit.assert_equal r1 r2

let test_copy_from_cursor_7 db1 db2 =
  _fill db1 10;
  _test_copy_from_cursor db1 db2 (Some 10) 10

let suite =
  let wrap f = bracket (setup_tc "0") f (teardown_tc "0") in
  let wrap2 f = bracket
                  (setup_tc "0")
                  (fun db0 ->
                    bracket
                      (setup_tc "1")
                      (f db0)
                      (teardown_tc "1")
                      ())
                  (teardown_tc "0")
  in
  "Otc" >:::
    [
      "make" >:: test_make;
      "open" >:: wrap test_open;
      "basic" >:: wrap test_basic;
      "cursor" >:: wrap test_cursor;
      "range" >:: wrap test_range;
      "range2" >:: wrap test_range2;
      "unknown" >:: wrap test_unknown;
      "prefix_keys" >:: wrap test_prefix_keys;
      "null" >:: wrap test_null;
      "flags" >:: wrap test_flags;
      "range_entries" >:: wrap test_range_entries;
      "range_entries2" >:: wrap test_range_entries2;
      "range_entries3" >:: wrap test_range_entries3;
      "range_entries4" >:: wrap test_range_entries4;
      "copy_from_cursor" >::: [
        "0" >:: wrap2 test_copy_from_cursor_0;
        "1" >:: wrap2 test_copy_from_cursor_1;
        "2" >:: wrap2 test_copy_from_cursor_2;
        "3" >:: wrap2 test_copy_from_cursor_3;
        "4" >:: wrap2 test_copy_from_cursor_4;
        "5" >:: wrap2 test_copy_from_cursor_5;
        "6" >:: wrap2 test_copy_from_cursor_6;
        "7" >:: wrap2 test_copy_from_cursor_7;
      ];
      "get3_generic" >:: wrap test_get3_generic
    ]
