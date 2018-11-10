(*
 * Copyright (c) 2015      Mindy Preston <mindy@somerandomidiot.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
open Mirage_fs

let test_fs = "test_directory"
let empty_file = "empty"
let content_file = "content"
let big_file = "big_file"
let directory = "a_directory"

module FS_impl = FS_unix

let lwt_run f () = Lwt_main.run (f ())

let do_or_fail = Rresult.R.get_ok

let failf fmt = Fmt.kstrf Alcotest.fail fmt
let assert_fail e = failf "%a" FS_unix.pp_error e
let assert_write_fail e = failf "%a" FS_unix.pp_write_error e

let connect_present_dir () =
  FS_impl.connect test_fs >>= fun _fs -> Lwt.return_unit

let clock = lwt_run (fun () -> Pclock.connect ()) ()

let append_timestamp s =
  let now = Ptime.v (Pclock.now_d_ps clock) in
  Fmt.strf "%s-%a" s (Ptime.pp_rfc3339 ~space:false ()) now

let full_path dirname filename = dirname ^ "/" ^ filename

let just_one_and_is expected read_bufs =
  Alcotest.(check int) __LOC__ 1 (List.length read_bufs);
  Alcotest.(check string) __LOC__
    expected (Cstruct.to_string (List.hd read_bufs));
  Lwt.return read_bufs

let expect_error_connecting where () =
  Lwt.catch
    (fun () ->
       FS_impl.connect where >>= fun _ ->
       Lwt.fail_with "expected error")
    (fun _ -> Lwt.return_unit)

let connect_to_empty_string = expect_error_connecting ""

let connect_to_dev_null = expect_error_connecting "/dev/null"

let read_nonexistent_file file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs file 0 1 >>= function
  | Ok _ ->
    failf "read returned Ok when no file was expected. Please make sure there \
           isn't actually a file  named %s" file
  | Error (`Is_a_directory) ->
    failf "Unreasonable error response when trying to read a nonexistent file"
  | Error `No_directory_entry ->
    Lwt.return_unit
  | Error `Not_a_directory ->
    failf "not-a-directory when trying to read a non-existent file"
  | Error _ -> failf "fs errors"

let read_empty_file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs empty_file 0 1 >>= function
  | Ok [] -> Lwt.return_unit
  | Ok _  -> failf "reading an empty file returned some cstructs"
  | Error `No_directory_entry ->
    failf "read failed for a present but empty file; please make sure %s \
           is present in the test filesystem" empty_file
  | Error _ ->
    failf "unhelpful error returned from nonzero size read on an empty file"

let read_zero_bytes () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs content_file 0 0 >>= function
  | Ok [] -> Lwt.return_unit
  | Ok _  ->
    failf "reading zero bytes from a non-empty file returned some cstructs"
  | Error `No_directory_entry ->
    failf "read failed for a present file; please make  sure %s is present \
           in the test filesystem" content_file
  | Error _ ->
    failf "unhelpful error returned from reading 0 bytes of a non-empty file"

(* reasonable responses are Ok [] or an error *)
let read_negative_bytes () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs content_file 0 (-5) >>= function
  | Ok [] -> Lwt.return_unit
  | Ok  _ ->
    failf "reading -5 bytes from a non-empty file returned some cstructs"
  | Error `No_directory_entry ->
    failf "read failed for a present file; please make sure %s is present in \
           the test filesystem" content_file
  | Error `Negative_bytes -> Lwt.return_unit
  | Error _ ->
    failf "reading negative bytes from a file returned some misclassified error"

let read_too_many_bytes () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs content_file 0 65535 >>= function
  | Ok bufs ->
    Alcotest.(check int) __LOC__ 1 (List.length bufs);
    Alcotest.(check int) __LOC__ 13 (Cstruct.len (List.hd bufs));
    Lwt.return_unit
  | Error `No_directory_entry ->
    failf "read failed for a present file; please make sure %s is present in \
           the test filesystem" content_file
  | Error _ -> failf "read beyond EOF for a valid file returned an error"

let read_at_offset () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs content_file 1 20 >|= do_or_fail >>=
  just_one_and_is "ome content\n" >>= fun _ -> Lwt.return_unit

let read_subset_of_bytes () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs content_file 0 4 >|= do_or_fail >>= just_one_and_is "some" >>=
  fun _bufs -> Lwt.return_unit

let read_at_offset_past_eof () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.read fs content_file 50 10 >|= do_or_fail >>= function
  | [] -> Lwt.return_unit
  | _  -> failf "read returned content when we asked for an offset past EOF"

let read_big_file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.size fs big_file >|= do_or_fail >>= fun size ->
  FS_impl.read fs big_file 0 10000 >|= do_or_fail >>= function
  | [] -> failf "read returned nothing for a large file"
  | (hd::tl::[]) ->
    Alcotest.(check int) __LOC__
      (Int64.to_int size) ((Cstruct.len hd) + (Cstruct.len tl));
    Lwt.return_unit
  | _ -> failf "read didn't split a large file across pages as expected"

let size_nonexistent_file () =
  FS_impl.connect test_fs >>= fun fs ->
  let filename = "^#$\000not a file!!!. &;" in
  FS_impl.size fs filename >>= function
  | Ok d -> failf "Got a size of %Ld for absent file" d
  | Error `No_directory_entry -> Lwt.return_unit
  | Error e -> assert_fail e

let size_empty_file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.size fs empty_file >|= do_or_fail >>= fun n ->
  Alcotest.(check int64) "size of an empty file" (Int64.zero) n;
  Lwt.return_unit

let size_small_file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.size fs content_file >|= do_or_fail >>= fun n ->
  Alcotest.(check int64) "size of a small file" (Int64.of_int 13) n;
  Lwt.return_unit

let size_a_directory () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.size fs directory >>= function
  | Error `Is_a_directory -> Lwt.return_unit
  | Error e -> assert_fail e
  | Ok n -> failf "got size %Ld on a directory" n

let size_big_file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.size fs big_file >|= do_or_fail >>= fun size ->
  Alcotest.(check int64) __LOC__ (Int64.of_int 5000) size;
  Lwt.return_unit

let mkdir_already_empty_directory () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs directory >|= do_or_fail >>= fun () -> Lwt.return_unit

let mkdir_over_file () =
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs empty_file >>= function
  | Error `File_already_exists -> Lwt.return_unit
  | Error e -> assert_write_fail e
  | Ok () ->
    (* really?  We're already in trouble, but let's see how bad it is *)
    FS_impl.stat fs empty_file >>= function
    | Error _ ->
      failf "mkdir reported success in making a directory over an existing \
             file, and now the location isn't even stat-able"
    | Ok s ->
      match s.directory with
      | true -> failf "mkdir made a dir over top of an existing file"
      | false ->
        failf "mkdir falsely reported success making a directory over an \
               existing file"

let mkdir_over_directory_with_contents () =
  FS_impl.connect test_fs >>= fun fs ->
  let tempdir = append_timestamp "mkdir_over_directory_with_contents" in
  FS_impl.mkdir fs tempdir >|= do_or_fail >>= fun () ->
  FS_impl.mkdir fs (tempdir ^ "/cool tapes") >|= do_or_fail >>= fun () ->
  FS_impl.mkdir fs tempdir >>= function
  | Error `Is_a_directory | Error `File_already_exists -> Lwt.return_unit
  | Error e -> assert_write_fail e
  | Ok () -> (* did we clobber the subdirectory that was here? *)
    FS_impl.stat fs (tempdir ^ "/cool tapes") >>= function
    | Error `No_directory_entry ->
      failf "mkdir silently clobbered an existing directory and all of its \
             contents. That is bad. Please fix it."
    | Error e -> assert_fail e
    | Ok _    -> Lwt.return_unit

let mkdir_in_path_not_present () =
  let not_a_thing = "%%#@*%#@  $ /my awesome directory!!!" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs not_a_thing >|= do_or_fail >>= fun () -> Lwt.return_unit

let mkdir_credibly () =
  let dirname = append_timestamp "mkdir_credibly" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs dirname >|= do_or_fail >>= fun () ->
  FS_impl.stat fs dirname >|= do_or_fail >>= fun s ->
  Alcotest.(check bool) __LOC__ true s.directory;
  Alcotest.(check string) __LOC__ dirname s.filename;
  Lwt.return_unit

let write_not_a_dir () =
  let dirname = append_timestamp "write_not_a_dir" in
  let subdir = "not there" in
  let content = "puppies" in
  let full_path = (dirname ^ "/" ^ subdir ^ "/" ^ "file") in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs dirname >|= do_or_fail >>= fun () ->
  FS_impl.write fs full_path 0 (Cstruct.of_string content) >|=
  do_or_fail >>= fun () ->
  FS_impl.stat fs full_path >>= function
  | Error `No_directory_entry ->
    failf "Write to a nonexistent dir falsely claimed success"
  | Error _ ->
    failf "Write to nonexistent dir claimed success, but attempting to stat \
           it fails"
  | Ok _ ->
    FS_impl.read fs full_path 0 4096 >|= do_or_fail >>= fun bufs ->
    Alcotest.(check int) __LOC__  1 (List.length bufs);
    Alcotest.(check string) __LOC__ content (Cstruct.to_string (List.hd bufs));
    Lwt.return_unit

let write_zero_bytes () =
  let dirname = append_timestamp "mkdir_not_a_dir" in
  let subdir = "not there" in
  let full_path = (dirname ^ "/" ^ subdir ^ "/" ^ "file") in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs dirname >|= do_or_fail >>= fun () ->
  FS_impl.write fs full_path 0 (Cstruct.create 0) >|= do_or_fail >>= fun () ->
  (* make sure it's size 0 *)
  FS_impl.stat fs full_path >>= function
  | Ok stat -> Alcotest.(check int64) __LOC__ Int64.zero stat.size;
    Lwt.return_unit
  | Error `No_directory_entry ->
    failf "write claimed to create a file that the fs then couldn't find"
  | Error _ ->
    failf "write claimed to create a file, but trying to stat it fails"

let write_contents_correct () =
  let dirname = append_timestamp "write_contents_correct" in
  let full_path = full_path dirname "short_phrase" in
  let phrase = "standing here on this frozen lake" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.write fs full_path 0 (Cstruct.of_string phrase) >|=
  do_or_fail >>= fun () ->
  FS_impl.read fs full_path 0 (String.length phrase) >|= do_or_fail >>=
  just_one_and_is phrase >>= fun _ -> Lwt.return_unit

let write_at_offset_within_file () =
  let dirname = append_timestamp "mkdir_not_a_dir" in
  let full_path = full_path dirname "content" in
  let preamble = "given this information, " in
  let boring = "action is required." in
  let better_idea = "let's go ride bikes" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs dirname >|= do_or_fail >>= fun () ->
  FS_impl.write fs full_path 0 (Cstruct.of_string (preamble ^ boring))
  >|= do_or_fail >>= fun () ->
  FS_impl.write fs full_path (String.length preamble) (Cstruct.of_string
                                                         better_idea)
  >|= do_or_fail >>= fun () ->
  FS_impl.read fs full_path 0 4096 >|= do_or_fail >>= fun read_bufs ->
  just_one_and_is (preamble ^ better_idea) read_bufs >>= fun _ ->
  Lwt.return_unit

let write_at_offset_past_eof () =
  let dirname = append_timestamp "write_at_offset_past_eof" in
  let full_path = full_path dirname "initially_contentful" in
  let content = "repetition for its own sake." in
  let replacement = "ating yourself is super fun!" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.write fs full_path 0 (Cstruct.of_string content) >|=
  do_or_fail >>= fun () ->
  FS_impl.write fs full_path 4 (Cstruct.of_string replacement) >|=
  do_or_fail >>= fun () ->
  FS_impl.stat fs full_path >|= do_or_fail >>= fun _ ->
  FS_impl.read fs full_path 0 4096 >|= do_or_fail >>= fun read_bufs ->
  just_one_and_is ("repe" ^ replacement) read_bufs >>= fun _ ->
  Lwt.return_unit

let write_overwrite_dir () =
  let dirname = append_timestamp "write_overwrite_dir" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.mkdir fs dirname >|= do_or_fail >>= fun () ->
  FS_impl.write fs dirname 0 (Cstruct.of_string "noooooo") >>= function
  | Error `Is_a_directory -> Lwt.return_unit
  | Error e -> assert_write_fail e
  | Ok () -> (* check to see whether it actually overwrote or just failed to
                 return an error *)
    FS_impl.stat fs dirname >|= do_or_fail >>= fun s ->
    match s.directory with
    | true  -> failf "write falsely reported success overwriting a directory"
    | false -> failf "write overwrite an entire directory!"

let write_at_offset_is_eof () =
  let dirname = append_timestamp "write_at_offset_is_eof" in
  let full_path = full_path dirname "database.sql" in
  let extant_data = "important record do not overwrite :)\n" in
  let new_data = "some more super important data!\n" in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.write fs full_path 0 (Cstruct.of_string extant_data)
  >|= do_or_fail >>= fun () ->
  FS_impl.write fs full_path (String.length extant_data) (Cstruct.of_string new_data)
  >|= do_or_fail >>= fun () ->
  FS_impl.read fs full_path 0
    ((String.length extant_data) + String.length new_data)
  >|= do_or_fail >>= just_one_and_is (extant_data ^ new_data) >>= fun _ ->
  Lwt.return_unit

let write_at_offset_beyond_eof () =
  let dirname = append_timestamp "write_at_offset_beyond_eof" in
  let filename = "database.sql" in
  let full_path = full_path dirname filename in
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.write fs full_path 0 (Cstruct.of_string "antici") >|=
  do_or_fail >>= fun () ->
  FS_impl.write fs full_path 10 (Cstruct.of_string "pation") >|=
  do_or_fail >>= fun () ->
  FS_impl.read fs full_path 0 4096 >|= do_or_fail
  >>= just_one_and_is "antici\000\000\000\000pation" >>= fun _ -> Lwt.return_unit

let write_big_file () =
  let how_big = 4100 in
  let dirname = append_timestamp "write_big_file" in
  let full_path = full_path dirname "so many bytes!" in
  let zero_cstruct cs =
    let zero c = Cstruct.set_char c 0 '\000' in
    let i = Cstruct.iter (fun _ -> Some 1) zero cs in
    Cstruct.fold (fun b _ -> b) i cs
  in
  let first_page = zero_cstruct (Cstruct.create how_big) in
  Cstruct.set_char first_page 4097 'A';
  Cstruct.set_char first_page 4098 'B';
  Cstruct.set_char first_page 4099 'C';
  FS_impl.connect test_fs >>= fun fs ->
  FS_impl.write fs full_path 0 first_page >|= do_or_fail >>= fun () ->
  FS_impl.size fs full_path >|= do_or_fail >>= fun sz ->
  let check_chars buf a b c =
    Alcotest.(check char) __LOC__ 'A' (Cstruct.get_char buf a);
    Alcotest.(check char) __LOC__ 'B' (Cstruct.get_char buf b);
    Alcotest.(check char) __LOC__ 'C' (Cstruct.get_char buf c)
  in
  Alcotest.(check int64) __LOC__ (Int64.of_int how_big) sz;
  FS_impl.read fs full_path 0 how_big >|= do_or_fail >>= function
  | [] -> failf "claimed a big file was empty on read"
  | _::buf::[]-> check_chars buf 1 2 3; Lwt.return_unit
  | _ -> failf "read sent back a nonsensible number of buffers"

let populate num depth fs =
  let rec gen_d pref = function
    | 0 -> "foo"
    | x -> Filename.concat (pref ^ (string_of_int x)) (gen_d pref (pred x))
  in
  let rec gen_l acc = function
    | 0 -> acc
    | x -> gen_l (gen_d (string_of_int x) depth :: acc) (pred x)
  in
  (* populate a bit *)
  Lwt_list.iteri_s (fun i x ->
      FS_impl.create fs (append_timestamp ("foo" ^ x ^ (string_of_int i)))
      >|= do_or_fail >>= fun _ -> Lwt.return_unit
  ) (gen_l [] num)


let create () =
  FS_impl.connect test_fs >>= fun fs ->
  let fn = "createdoesnotyetexist" in
  FS_impl.create fs fn  >>= function
  | Error _ -> failf "create failed"
  | Ok () -> FS_impl.destroy fs fn >>= function
    | Error _ -> failf "destroy after create failed"
    | Ok () -> Lwt.return_unit

let destroy () =
  let files = append_timestamp (test_fs ^ "2") in
  Lwt_unix.mkdir files 0o755 >>= fun () ->
  let cleanup () = Lwt_unix.rmdir files in
  FS_impl.connect files >>= fun fs ->
  populate 10 4 fs >>= fun () ->
  FS_impl.destroy fs "/" >>= function
  | Error _ -> cleanup () >>= fun () -> failf "create failed"
  | Ok () ->
    FS_impl.listdir fs "/" >>= function
    | Ok []   -> Lwt.return_unit
    | Ok _    -> failf "something exists after destroy"
    | Error _ -> failf "error in listdir"

let destroy_a_bit () =
  let files = append_timestamp (test_fs ^ "3") in
  Lwt_unix.mkdir files 0o755 >>= fun () ->
  let cleanup () = let _ = Sys.command ("rm -rf " ^ files) in Lwt.return_unit in
  FS_impl.connect files >>= fun fs ->
  populate 10 4 fs >>= fun () ->
  FS_impl.listdir fs "/" >>= (function
      | Ok xs -> Lwt.return (List.length xs)
      | Error _ -> failf "error in listdir") >>= fun files ->
  FS_impl.create fs "barf" >>= function
  | Error _ -> cleanup () >>= fun () -> failf "create failed"
  | Ok ()   -> FS_impl.destroy fs "barf" >>= (function
      | Error _ -> cleanup () >>= fun () -> failf "destroy failed"
      | Ok ()   -> FS_impl.listdir fs "/" >>= (function
          | Ok xs when List.length xs = files -> cleanup ()
          | Ok _ ->
            failf "something wrong in destroy: destroy  followed by create is \
                   not well behaving"
          | Error _ -> failf "error in listdir"))

let () =
  let connect = [
    "connect_to_empty_string", `Quick, lwt_run connect_to_empty_string;
    "connect_to_dev_null", `Quick, lwt_run connect_to_dev_null;
    "connect_present_dir", `Quick, lwt_run connect_present_dir;
  ] in
  let read = [
    "read_nonexistent_file_from_root", `Quick,
    lwt_run (read_nonexistent_file "^$@thing_that_isn't_in root!!!.space");
    "read_nonexistent_file_from_dir", `Quick,
    lwt_run (read_nonexistent_file "not a *dir*?!?/thing_that_isn't_in root!!!.space");
    "read_empty_file", `Quick, lwt_run read_empty_file;
    "read_zero_bytes", `Quick, lwt_run read_zero_bytes;
    "read_negative_bytes", `Quick, lwt_run read_negative_bytes;
    "read_too_many_bytes", `Quick, lwt_run read_too_many_bytes;
    "read_at_offset", `Quick, lwt_run read_at_offset;
    "read_subset_of_bytes", `Quick, lwt_run read_subset_of_bytes;
    "read_at_offset_past_eof", `Quick, lwt_run read_at_offset_past_eof;
    "read_big_file", `Quick, lwt_run read_big_file;
  ] in
  let create = [
    "create_file", `Quick, lwt_run create
  ] in
  let destroy = [
    "destroy_file", `Quick, lwt_run destroy;
    "create_destroy_file", `Quick, lwt_run destroy_a_bit
  ] in
  let size = [
    "size_nonexistent_file", `Quick, lwt_run size_nonexistent_file;
    "size_empty_file", `Quick, lwt_run size_empty_file;
    "size_small_file", `Quick, lwt_run size_small_file;
    "size_a_directory", `Quick, lwt_run size_a_directory;
    "size_big_file", `Quick, lwt_run size_big_file;
  ] in
  let mkdir = [
    "mkdir_credibly", `Quick, lwt_run mkdir_credibly;
    "mkdir_already_empty_directory", `Quick, lwt_run mkdir_already_empty_directory;
    "mkdir_over_file", `Quick, lwt_run mkdir_over_file;
    "mkdir_over_directory_with_contents", `Quick, lwt_run mkdir_over_directory_with_contents;
    "mkdir_in_path_not_present", `Quick, lwt_run mkdir_in_path_not_present;
  ] in
  let listdir = [ ] in
  let write = [
    "write_not_a_dir", `Quick, lwt_run write_not_a_dir;
    "write_zero_bytes", `Quick, lwt_run write_zero_bytes;
    "write_contents_correct", `Quick, lwt_run write_contents_correct;
    "write_at_offset_within_file", `Quick, lwt_run write_at_offset_within_file;
    "write_at_offset_past_eof", `Quick, lwt_run write_at_offset_past_eof;
    "write_overwrite_dir", `Quick, lwt_run write_overwrite_dir;
    "write_at_offset_is_eof", `Quick, lwt_run write_at_offset_is_eof;
    "write_at_offset_beyond_eof", `Quick, lwt_run write_at_offset_beyond_eof;
    "write_big_file", `Quick, lwt_run write_big_file;
  ] in
  Alcotest.run "FS_impl" [
    "connect", connect;
    "read", read;
    "size", size;
    "mkdir", mkdir;
    "destroy", destroy;
    "create", create;
    "listdir", listdir;
    "write", write;
  ]
