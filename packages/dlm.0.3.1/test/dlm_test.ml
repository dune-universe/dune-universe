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
open Lwt.Infix

let nop _ _ = Lwt.return_unit

let test_openclose ctx uuid =
  logf ctx `Info "Opening lockspace %s" uuid;
  Dlm.with_lockspace uuid ~f:(fun _ ->
      Gc.compact ();
      logf ctx `Info "Closing lockspace %s" uuid;
      Lwt.return_unit
  )

let uuid_gen n =
  let gen _ = Uuidm.v `V4 |> Uuidm.to_string in
  Array.init n gen |> Array.to_list

let with_join_lockspace ctx name ~f =
  logf ctx `Info "Joining %s" name;
  Dlm.join name >>= fun () ->
  Lwt.finalize (fun () ->
      Gc.compact ();
      logf ctx `Info "Joined %s" name;
      f ctx name >>= fun () ->
      Gc.compact ();
      Lwt.return_unit
    ) (fun () ->
      logf ctx `Info "Leaving %s" name;
      Dlm.leave name >>= fun () ->
      logf ctx `Info "Done test for %s" name;
      Gc.compact ();
      Lwt.return_unit)


let test_joinleave n f ctx =
  OUnit2.skip_if (n > 10) "Only test small number of lockspaces";
  Random.self_init ();
  Lwt_main.run @@ begin
    let uuids = uuid_gen n in
    Lwt_list.iter_p (fun uuid ->
        Lwt.catch (fun () ->
            with_join_lockspace ctx uuid ~f
          ) (function
            | Unix.Unix_error(Unix.EEXIST, "dlm_create_lockspace", ls) when n >= 64 ->
              (* hit kernel limit of 64 lockspaces, error is expected *)
              assert_equal ls uuid;
              Lwt.return_unit
            | e -> Lwt.fail e
          )
      ) uuids
  end;
  Gc.compact ()

let test_lock_timeout ctx (ls, lock) =
  logf ctx `Info "Reacquiring lock %s (waiting for timeout)" lock;
  Lwt.try_bind (fun () ->
      Dlm.with_lock ls lock ~timeout:0.01 ~f:(fun () ->
          Lwt.fail_with "Lock granted twice"))
    (fun () -> Lwt.fail_with "Acquiring the lock should have timed out")
    (function
      | Unix.Unix_error(Unix.ETIMEDOUT, "dlm_ls_lockx.sb_status", lockname) ->
        assert_equal ~printer:(fun x -> x) lockname lock;
        (* the exception we expected *)
        Lwt.return_unit
      | e -> Lwt.fail e
    )

let test_acquire_release n f ctx name = 
  logf ctx `Info "Opening lockspace %s" name;
  Dlm.with_lockspace name ~f:(fun ls ->
      Gc.compact ();
      let uuids = uuid_gen n in
      Lwt_list.iter_p (fun lock ->
          logf ctx `Info "Acquiring lock %s" lock;
          Dlm.with_lock ls lock ~f:(fun () ->
              Gc.compact ();
              logf ctx `Info "Acquired lock %s" lock;
              f ctx (ls,lock) >>= fun () ->
              logf ctx `Info "Releasing lock %s" lock;
              Lwt.return_unit
          )
      ) uuids >>= fun () ->
      Gc.compact ();
      logf ctx `Info "Closing lockspace %s" name;
      Lwt.return_unit)

let suite =
  (* run on node with working DLM *)
  "DLM test" >:::
  List.map (fun n ->
      string_of_int n >::: [
        "join/leave new lockspace" >:: test_joinleave n nop;
        "join/leave new lockspace and open/close" >:: test_joinleave n test_openclose;
        "acquire/release locks" >:: test_joinleave 1 (test_acquire_release n nop);
        "lock timeouts" >:: test_joinleave 1 (test_acquire_release n test_lock_timeout)
      ]) [1; 10; 100; 1000]

let () = run_test_tt_main suite
