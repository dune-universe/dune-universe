open! Core
open Async
open Pam_test
open Import
open Pam
open Expect_test_helpers

module Env = struct
  type t =
    { key : string
    ; data : string
    }
  [@@deriving fields]

  let create = Fields.create
  let to_string t = sprintf "%s=%s" (key t) (data t)
end

let get_pam_config_lines auth_file session_file =
  [ "#%PAM-1.0"
  ; "auth     required   pam_env.so envfile=" ^ auth_file
  ; "auth     sufficient pam_permit.so"
  ; "account  sufficient pam_permit.so"
  ; "password sufficient pam_permit.so"
  ; "session  required   pam_env.so envfile=" ^ session_file
  ; "session  sufficient pam_permit.so"
  ]
;;

let setup_pam_service file auth_file session_file =
  match%bind Sys.file_exists file with
  | `Yes | `Unknown ->
    failwithf "Test config %s exists before test data is setup" file ()
  | `No -> Writer.save_lines file (get_pam_config_lines auth_file session_file)
;;

let with_custom_pam_stack ~auth_env ~session_env ~f =
  with_temp_dir (fun dir ->
    let setup_pam_envfile file env =
      Writer.with_file file ~f:(fun writer ->
        Writer.write_line writer (Env.to_string env);
        Deferred.unit)
    in
    let service = "ocaml-pam-test" in
    let pam_file = "/etc/pam.d" ^/ service in
    let auth_file = dir ^/ "auth" in
    let session_file = dir ^/ "session" in
    let%bind () = setup_pam_envfile auth_file auth_env in
    let%bind () = setup_pam_envfile session_file session_env in
    let%bind () = setup_pam_service pam_file auth_file session_file in
    let res = with_pam ~service ~f () in
    let%bind () =
      Deferred.List.iter ~f:Unix.unlink [ pam_file; auth_file; session_file ]
    in
    return res)
;;

let ensure_running_as_root () =
  if Unix.getuid () <> 0 then failwith "You must run this test as root."
;;

let test_pam_stack () =
  (* It requires root privileges to setup the desired pam configuration file
     in /etc/pam.d *)
  ensure_running_as_root ();
  let auth_env = Env.create ~key:"PAM_AUTH_TEST" ~data:"auth_phase" in
  let session_env = Env.create ~key:"PAM_SESSION_TEST" ~data:"session_phase" in
  with_custom_pam_stack ~auth_env ~session_env ~f:(fun t ->
    let open Or_error.Let_syntax in
    let flags = [] in
    let%bind () = pam_authenticate t ~flags in
    let%bind () = pam_setcred t ~flag:Pam_cred.Flag.PAM_ESTABLISH_CRED in
    let%bind e1 = pam_getenv t ~key:(Env.key auth_env) in
    let%bind () = pam_acct_mgmt t ~flags in
    let%bind () = pam_chauthtok t ~flags in
    let%bind () = pam_open_session t ~flags in
    let%bind e2 = pam_getenv t ~key:(Env.key session_env) in
    let%bind () = pam_close_session t ~flags in
    return (e1, e2))
  >>| Or_error.ok_exn
;;

let%expect_test "full pam stack run" =
  let%bind () =
    match%map Monitor.try_with_or_error test_pam_stack with
    | Error e -> print_s [%message "" ~_:(e : Error.t)]
    | Ok (auth_env, session_env) ->
      print_s [%message (auth_env : string) (session_env : string)]
  in
  [%expect {|
    ((auth_env    auth_phase)
     (session_env session_phase)) |}]
;;
