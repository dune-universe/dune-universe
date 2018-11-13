let compare_t = let module M = Mirage_fs_mem.Pure in (module M: Alcotest.TESTABLE with type t = Mirage_fs_mem.Pure.t)

let we =
  let module M = struct
    type t = Mirage_fs_mem.write_error
    let pp = Mirage_fs_mem.pp_write_error
    let equal a b = compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_write_res = Alcotest.result compare_t we

let e =
  let module M = struct
    type t = Mirage_fs.error
    let pp = Mirage_fs.pp_error
    let equal a b = compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let cs =
  let module M = struct
    type t = Cstruct.t
    let pp = Cstruct.hexdump_pp
    let equal = Cstruct.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_read_res = Alcotest.result (Alcotest.list cs) e

let stat =
  let open Mirage_fs in
  let module M = struct
    type t = stat
    let pp fmt stat = 
      Fmt.pf fmt "{ name: %s ; read_only: %b ; directory: %b ; size: %Lu }" stat.filename stat.read_only stat.directory stat.size
    let equal a b = 
      String.equal a.filename b.filename && a.read_only = b.read_only && a.directory = b.directory && a.size = b.size 
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_stat_res = Alcotest.result stat e

let bc = Cstruct.of_string "bc"
let neu = Cstruct.of_string "NEU"
let add k v m = match Mirage_fs_mem.Pure.write m k 0 v with 
 | Error _ -> assert false
 | Ok m -> m
let empty_m = Mirage_fs_mem.Pure.empty ()

let map = add "a" bc empty_m

let empty () =
  let expected = empty_m in
  Alcotest.check compare_t "hello" expected (Mirage_fs_mem.Pure.empty ()) 

let read () =
  let expected = Ok [ bc ] in
  Alcotest.check compare_read_res "hello" expected (Mirage_fs_mem.Pure.read map "a" 0 2) 

let readWithLength () =
  let expected = Ok [ bc ] in
  Alcotest.check compare_read_res "hello" expected (Mirage_fs_mem.Pure.read map "a" 0 50) 

let size () =
  let expected = Ok 2L in 
  Alcotest.check (Alcotest.result Alcotest.int64 e) "hello" expected (Mirage_fs_mem.Pure.size map "a")

let create () =
  let expected = Ok (add "leer" Cstruct.empty empty_m) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.create empty_m "leer")

let mkdir () =
  let expected = Ok { Mirage_fs.filename = "leer" ; read_only = false ; directory = true ; size = 0L } in
  match Mirage_fs_mem.Pure.mkdir empty_m "leer" with
   | Ok m -> Alcotest.check compare_stat_res "hello" expected (Mirage_fs_mem.Pure.stat m "leer")
   | Error _ -> assert false

let mkdirWithSlash () =
  let failOnError = function
   | Error _ -> assert false
   | Ok m -> m in
  let expected_subdir = Ok { Mirage_fs.filename = "__uids__/10000000-0000-0000-0000-000000000001/" ; read_only = false ; directory = true ; size = 0L } in
  let expected_dir = Ok { Mirage_fs.filename = "__uids__/" ; read_only = false ; directory = true ; size = 0L } in
  let m = failOnError @@ Mirage_fs_mem.Pure.mkdir empty_m "" in
  let m = failOnError @@ Mirage_fs_mem.Pure.write m ".prop.xml" 0 (Cstruct.create 22) in
  let m = failOnError @@ Mirage_fs_mem.Pure.mkdir m "__uids__" in
  let m = failOnError @@ Mirage_fs_mem.Pure.write m "__uids__/.prop.xml" 0 (Cstruct.create 22) in
  let m = failOnError @@ Mirage_fs_mem.Pure.mkdir m "__uids__/10000000-0000-0000-0000-000000000001" in
  let m = failOnError @@ Mirage_fs_mem.Pure.write m "__uids__/10000000-0000-0000-0000-000000000001/.prop.xml" 0 (Cstruct.create 22) in
  Alcotest.check compare_stat_res "hello" expected_subdir (Mirage_fs_mem.Pure.stat m "__uids__/10000000-0000-0000-0000-000000000001/");
  Alcotest.check compare_stat_res "hello" expected_dir (Mirage_fs_mem.Pure.stat m "__uids__/")

let destroy () =
  let expected = empty_m in
  Alcotest.check compare_write_res "hello" (Ok expected)
    (Mirage_fs_mem.Pure.destroy map "a")

let stat () =
  let expected = Ok { Mirage_fs.filename = "a" ; read_only = false ; directory = false ; size = 2L } in
  Alcotest.check compare_stat_res "hello" expected (Mirage_fs_mem.Pure.stat map "a")
  
let listdir () =
  let map_of_three = add "b" Cstruct.empty (add "c" Cstruct.empty map) in
  let expected = Ok [ "a" ; "b" ; "c" ] in 
  Alcotest.check
    Alcotest.(result (slist string String.compare) e) "hello"
    expected (Mirage_fs_mem.Pure.listdir map_of_three "")

let write () =
  let expected = Ok (add "a" bc empty_m) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write empty_m "a" 0 bc)

let write_multiple () =
  let expected = Ok (add "a" bc (add "b" bc empty_m)) in
  match Mirage_fs_mem.Pure.write empty_m "b" 0 bc with
  | Ok m -> Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write m "a" 0 bc)
  | Error _ -> Alcotest.fail "Unexpected map write result"

let writeOffset () =
  let expected = Error `No_space
  in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write map "a" 10 bc)

let overwrite () =
  let map = add "a" (Cstruct.of_string "gutentag1234") empty_m
  and expected = Ok (add "a" (Cstruct.of_string "foo") empty_m)
  in
  Alcotest.check compare_write_res "hello" expected
    (Mirage_fs_mem.Pure.write map "a" 0 (Cstruct.of_string "foo"))

let create_multiple_directories () =
  match Mirage_fs_mem.Pure.mkdir empty_m "b" with
  | Error _ -> Alcotest.fail "Unexpected mkdir result"
  | Ok m ->
    match Mirage_fs_mem.Pure.mkdir m "b/c" with
    | Error _ -> Alcotest.fail "Unexpected mkdir result"
    | Ok m' ->
      match Mirage_fs_mem.Pure.mkdir m' "b/d" with
      | Error _ -> Alcotest.fail "Unexpected mkdir result"
      | Ok m'' ->
        match Mirage_fs_mem.Pure.mkdir m'' "b/e" with
        | Error _ -> Alcotest.fail "Unexpected mkdir result"
        | Ok m''' ->
          let expected = Ok [ "c" ; "d" ; "e" ] in
          Alcotest.check
            Alcotest.(result (slist string String.compare) e) __LOC__
            expected (Mirage_fs_mem.Pure.listdir m''' "b")

let create_multiple_directories_and_root () =
  match Mirage_fs_mem.Pure.mkdir empty_m "b" with
  | Error _ -> Alcotest.fail "Unexpected mkdir result"
  | Ok m ->
    match Mirage_fs_mem.Pure.mkdir m "b/c" with
    | Error _ -> Alcotest.fail "Unexpected mkdir result"
    | Ok m' ->
      match Mirage_fs_mem.Pure.mkdir m' "b/d" with
      | Error _ -> Alcotest.fail "Unexpected mkdir result"
      | Ok m'' ->
        match Mirage_fs_mem.Pure.mkdir m'' "b" with
        | Error e ->
          Alcotest.check we __LOC__ `File_already_exists e
        | Ok _m''' ->
          Alcotest.fail "expected error"

let write_tests = [
  "create empty filesystem", `Quick, empty;
  "reading a file", `Quick, read;
  "reading a file giving an illegally long length", `Quick, readWithLength;
  "size of file", `Quick, size;
  "create file", `Quick, create;
  "create directory", `Quick, mkdir;
  "create directory with slash inside", `Quick, mkdirWithSlash;
  "remove file", `Quick, destroy;
  "get stat of file", `Quick, stat;
  "list a directory", `Quick, listdir;
  "writing a file", `Quick, write;
  "writing multiple files", `Quick, write_multiple;
  "writing a file with offset", `Quick, writeOffset;
  "overwrite a file", `Quick, overwrite;
  "mkdir twice", `Quick, create_multiple_directories ;
  "create multiple directories and root again", `Quick, create_multiple_directories_and_root ;
]

let tests = [
  "Write", write_tests;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "Mirage-FS-Mem test" tests
