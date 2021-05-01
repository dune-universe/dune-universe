(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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
 *
 *)

let (/) = Filename.concat

module EventFlags : Alcotest.TESTABLE = struct

  type t = Fsevents.EventFlags.t

  let pp fmt flags =
    Format.pp_print_string fmt (Fsevents.EventFlags.to_string_one_line flags)

  let equal flags flags' = flags = flags'

end

let event_flags = (module EventFlags : Alcotest.TESTABLE)

let create_flags = Fsevents.CreateFlags.detailed_interactive

let run_loop_mode = Cf.RunLoop.Mode.Default

let () = Random.self_init ()

let rec mktmpdir_under dir =
  let k = Random.int 0x10000 in
  let path = dir / (Printf.sprintf "fsevents_test_%04x" k) in
  try Unix.mkdir path 0o700; path
  with Unix.Unix_error (Unix.EEXIST, _, _) -> mktmpdir_under dir

let ensuredir dir =
  try Unix.mkdir dir 0o700; dir
  with Unix.Unix_error (Unix.EEXIST, _, _) -> dir

let callback activity =
  print_endline (Cf.RunLoop.Observer.Activity.to_string activity)

let with_temp_stream f () =
  let open Lwt.Infix in
  let tmp = ensuredir (Unix.getcwd () / "tmp") in
  let dir = mktmpdir_under tmp in
  let watcher = Fsevents_lwt.create 0. create_flags [dir] in
  let after_runloop, runloop_done = Lwt.wait () in
  let exit_cb _exit =
    Lwt_preemptive.run_in_main (fun () ->
      Lwt.return (Lwt.wakeup runloop_done ())
    ) in
  let lwt = Cf_lwt.RunLoop.run_thread (fun runloop ->
    let observer = Cf.RunLoop.Observer.(create Activity.All callback) in
    let stop_observer =
      Cf.RunLoop.Observer.(create Activity.(Only [Exit]) exit_cb)
    in
    Cf.RunLoop.(add_observer runloop observer Mode.Default);
    Cf.RunLoop.(add_observer runloop stop_observer Mode.Default);
    Fsevents_lwt.schedule_with_run_loop watcher runloop run_loop_mode;
    begin
      if not (Fsevents_lwt.start watcher)
      then print_endline "failed to start FSEvents stream"
    end;
  ) >>= fun runloop ->
    f dir
    >>= fun f ->
    f watcher
    >>= fun () ->
    Cf.RunLoop.stop runloop;
    after_runloop
  in

  Lwt_main.run lwt;

  Unix.(match system ("rm -r "^dir) with
    | WEXITED 0 -> ()
    | _ -> Alcotest.fail "couldn't delete test dir"
  )

module Event = struct
  open Lwt
  open Fsevents.EventFlags
  open Fsevents_lwt

  type t =
    | DirType of t
    | FileType of t
    | Create of string
    | Remove of string
    | Modify of string
    | MetaModify of string
    | ChangeOwner of string

  let fail_event remaining event_type =
    Alcotest.fail
      ("expected "^event_type^" event but current event is:\n"^
       remaining.path^"\n"^
       to_string_one_line remaining.flags)

  let has_create remaining =
    if remaining.flags.item_created
    then
      { remaining with flags = { remaining.flags with item_created = false } }
    else fail_event remaining "ItemCreated"

  let has_remove remaining =
    if remaining.flags.item_removed
    then
      { remaining with flags = { remaining.flags with item_removed = false } }
    else fail_event remaining "ItemRemoved"

  let has_modify remaining =
    if remaining.flags.item_modified
    then
      { remaining with flags = { remaining.flags with item_modified = false } }
    else fail_event remaining "ItemModified"

  let has_meta_modify remaining =
    if remaining.flags.item_inode_meta_mod
    then
      { remaining with flags = {
          remaining.flags with item_inode_meta_mod = false
        } }
    else fail_event remaining "ItemInodeMetaMod"

  let has_change_owner remaining =
    if remaining.flags.item_change_owner
    then
      { remaining with flags = {
          remaining.flags with item_change_owner = false
        } }
    else fail_event remaining "ItemChangeOwner"

  let has_dir_type remaining = match remaining.flags.item_type with
    | Some Dir ->
      { remaining with flags = { remaining.flags with item_type = None } }
    | _ -> fail_event remaining "ItemType(Dir)"

  let has_file_type remaining = match remaining.flags.item_type with
    | Some File ->
      { remaining with flags = { remaining.flags with item_type = None } }
    | _ -> fail_event remaining "ItemType(File)"

  let check_path { path; flags; _ } expected_path =
    if expected_path = path
    then ()
    else
      Alcotest.fail
        ("unexpected path "^path^" expecting "^expected_path^"\n"^
         "for event "^to_string_one_line flags^"\n")

  let rec check_one remaining = function
    | Create path -> check_path remaining path; has_create remaining
    | Remove path -> check_path remaining path; has_remove remaining
    | Modify path -> check_path remaining path; has_modify remaining
    | MetaModify path -> check_path remaining path; has_meta_modify remaining
    | ChangeOwner path -> check_path remaining path; has_change_owner remaining
    | DirType n -> check_one (has_dir_type remaining) n
    | FileType n -> check_one (has_file_type remaining) n

  let end_of_stream stream =
    Lwt_stream.is_empty stream
    >>= fun is_empty ->
    if is_empty
    then Lwt.return_unit
    else
      Lwt_stream.next stream
      >>= fun { path; flags; _ } ->
      Alcotest.fail ("event stream is not empty:\n"^path^"\n"^
                     to_string_one_line flags)

  let rec check ?remaining stream = function
    | [] ->
      (match remaining with
       | Some { flags; _ } when flags = zero -> end_of_stream stream
       | Some event -> fail_event event "no"
       | None -> end_of_stream stream
      )
    | first::rest ->
      (match remaining with
       | Some { flags; _ } when flags = zero -> Lwt_stream.next stream
       | Some event -> return event
       | None -> Lwt_stream.next stream
      ) >>= fun remaining ->
      check ~remaining:(check_one remaining first) stream rest

  let expect events watcher =
    Lwt_unix.sleep 0.020
    >>= fun () ->
    Fsevents_lwt.flush watcher
    >>= fun () ->
    Fsevents_lwt.stop watcher;
    Fsevents_lwt.invalidate watcher;
    let stream = Fsevents_lwt.stream watcher in
    check stream events

end

module FileMod = struct
  open Lwt

  let noop dir =
    return Event.(expect [
      DirType (Create dir);
    ])

  let create dir =
    let path = dir / "create_file_test" in
    Lwt_unix.(openfile path [O_CREAT] 0o600)
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    return Event.(expect [
      DirType (Create dir); FileType (Create path);
    ])

  let create_remove dir =
    let file = "create_remove_file_test" in
    let path = dir / file in
    Lwt_unix.(openfile path [O_CREAT] 0o600)
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    Lwt_unix.unlink path
    >>= fun () ->
    return Event.(expect [
      DirType (Create dir); FileType (Create path); Remove path;
    ])

  let modify dir =
    let file = "modify_file_test" in
    let path = dir / file in
    Lwt_unix.(openfile path [O_CREAT; O_WRONLY] 0o600)
    >>= fun fd ->
    Lwt_unix.write fd (Bytes.of_string file) 0 (String.length file)
    >>= fun _written ->
    Lwt_unix.close fd
    >>= fun () ->
    return Event.(expect [
      DirType (Create dir); FileType (Create path); Modify path;
    ])

  let chmod dir =
    Lwt_unix.chmod dir 0o750
    >>= fun () ->
    return Event.(expect [
      DirType (Create dir); ChangeOwner dir;
    ])

  let tests = [
    "noop0",          `Quick, with_temp_stream noop;
    "noop1",          `Quick, with_temp_stream noop;
    "noop2",          `Quick, with_temp_stream noop;
    "noop3",          `Quick, with_temp_stream noop;
    "noop4",          `Quick, with_temp_stream noop;
    "noop5",          `Quick, with_temp_stream noop;
    "noop6",          `Quick, with_temp_stream noop;

    "create",        `Quick, with_temp_stream create;
    "create_remove", `Quick, with_temp_stream create_remove;
    "modify",        `Quick, with_temp_stream modify;
    (*"chmod",         `Quick, with_temp_stream chmod;*)
    (*"modify_noop",   `Quick, with_temp_stream modify_noop;*)
  ]
end

let tests = [
  "FileMod", FileMod.tests;
]

;;
Alcotest.run "FSEvents" tests
