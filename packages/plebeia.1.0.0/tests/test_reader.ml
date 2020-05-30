(* Reader root catchup test *)
open Plebeia.Internal
open Test_utils
open Result

let test_with_vc2 () =
  let tempfile = Filename.temp_file "plebeia" ".context" in
  if Sys.file_exists tempfile then Sys.remove tempfile;
  let vc2_writer = Vc.create tempfile in
  let vc2_reader = Vc.open_ ~mode:Storage.Reader tempfile in
  let c_writer = Vc.empty_cursor vc2_writer in
  let c_writer = from_Ok @@ Deep.insert c_writer [Segment.encode_string "file1"] (Value.of_string "file1") in
  let c_writer, hash1 = Vc.commit (Bud_cache.empty ()) vc2_writer ~parent: None ~meta:(String.make 20 '\000') c_writer in
  let rhash1 = Roots.RootHash.of_plebeia_hash hash1 in

  prerr_endline "rhash1";
  let c_reader = from_Some @@ Vc.checkout vc2_reader rhash1 in
  assert (snd @@ from_Ok @@ Deep.get_value c_reader [Segment.encode_string "file1"] = Value.of_string "file1");
  
  let c_writer = from_Ok @@ Deep.insert c_writer [Segment.encode_string "file2"] (Value.of_string "file2") in
  let _c_writer, hash2 = Vc.commit (Bud_cache.empty ()) vc2_writer ~parent: None ~meta:(String.make 20 '\000') c_writer in
  let rhash2 = Roots.RootHash.of_plebeia_hash hash2 in
  
  prerr_endline "rhash2";
  let c_reader = from_Some @@ Vc.checkout vc2_reader rhash2 in
  assert (snd @@ from_Ok @@ Deep.get_value c_reader [Segment.encode_string "file1"] = Value.of_string "file1");
  assert (snd @@ from_Ok @@ Deep.get_value c_reader [Segment.encode_string "file2"] = Value.of_string "file2");

  (* Reader only with RDONLY fails the following ! *)
  prerr_endline "rhash1";
  let c_reader = from_Some @@ Vc.checkout vc2_reader rhash1 in
  assert (snd @@ from_Ok @@ Deep.get_value c_reader [Segment.encode_string "file1"] = Value.of_string "file1");

  (* Huuuuge catch-ups.  Make a file > 500MB *)
  let rec f n parent c_writer =
    if n = 1000 then parent, c_writer
    else
      let rec g c_writer = function
        | 0 -> c_writer
        | m ->
            g (from_Ok @@ Deep.insert c_writer [Segment.encode_string @@ string_of_int n ; Segment.encode_string @@ string_of_int m] (Value.of_string (string_of_int m))) (m-1)
      in
      let c_writer = g c_writer 10000 in
      let c_writer, hash = Vc.commit (Bud_cache.empty ()) vc2_writer ~parent: (Some parent) ~meta:(String.make 20 '\000') c_writer in
      let rhash = Roots.RootHash.of_plebeia_hash hash in
      f (n+1) rhash c_writer
  in
  let rhash_final, _c_writer = f 0 rhash2 c_writer in
  
  prerr_endline "rhash_final";
  let c_reader = from_Some @@ Vc.checkout vc2_reader rhash_final in
  assert (snd @@ from_Ok @@ Deep.get_value c_reader [Segment.encode_string "file1"] = Value.of_string "file1")
      
let () = test_with_vc2 ()

