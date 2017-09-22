(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.md.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open OUnit2

let lwt_get_exn f =
  Lwt_main.run @@
  Lwt.try_bind f (fun () -> Lwt.return_none) Lwt.return_some

let lockspace = "unit-test-nonexistent-lockspace"

let has_failed_nodlm f =
  Lwt.try_bind f (fun () -> Lwt.return_false)
    (function
      | Unix.Unix_error((Unix.ENOENT|Unix.ENOSYS|Unix.EPERM),
                        ("dlm_open_lockspace"|"dlm_create_lockspace"), ls) ->
        assert_equal ls lockspace;
        Lwt.return_true
      | e -> Lwt.fail e)

let test_nolockspace f =
  let has_failed = has_failed_nodlm f |> Lwt_main.run in
  Gc.compact ();
  assert_bool "Opening nonexistent lockspace must fail" has_failed

let test_withlockspace _ =
  test_nolockspace (fun () ->
      Dlm.with_lockspace lockspace ~f:(fun _ -> Lwt.return_unit))

let test_leavelockspace force _ =
  test_nolockspace (fun () ->
      Dlm.leave ~force lockspace)

let test_createlockspace _ =
  (* we don't run as root, so this must fail *)
  assert_bool "Must not run as root" (Unix.geteuid () > 0);
  test_nolockspace (fun () ->
      Dlm.join lockspace)

let suite =
  "lockspace" >::: [
    "open nonexistent lockspace" >:: test_withlockspace;
    "leave nonexistent lockspace" >:: test_leavelockspace false;
    "leave nonexistent lockspace force" >:: test_leavelockspace true;
    "create nonexistent lockspace" >:: test_createlockspace;
  ]

let () = run_test_tt_main suite
