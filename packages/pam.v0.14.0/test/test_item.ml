open! Core
open Import
open Pam
open Expect_test_helpers_core

let get_item item_type = with_pam ~f:(pam_get_item ~item_type) ()
let set_item item_type item = with_pam ~f:(pam_set_item ~item_type ~item) ()

let print_items t =
  let open Or_error.Let_syntax in
  let get item_type = pam_get_item t ~item_type in
  let%bind user = get Pam_item_type.PAM_USER in
  let%bind service = get Pam_item_type.PAM_SERVICE in
  let%bind prompt = get Pam_item_type.PAM_USER_PROMPT in
  let%bind tty = get Pam_item_type.PAM_TTY in
  let%bind ruser = get Pam_item_type.PAM_RUSER in
  let%bind rhost = get Pam_item_type.PAM_RHOST in
  let%bind xdisplay = get Pam_item_type.PAM_XDISPLAY in
  let%bind authtok_type = get Pam_item_type.PAM_AUTHTOK_TYPE in
  print_s
    [%sexp
      { user : string option
      ; service : string option
      ; prompt : string option
      ; tty : string option
      ; ruser : string option
      ; rhost : string option
      ; xdisplay : string option
      ; authtok_type : string option
      }];
  return ()
;;

let%expect_test "pam_get_item" =
  with_pam_exn ~f:print_items ();
  [%expect
    {|
    ((user    (foo))
     (service (other))
     (prompt       ())
     (tty          ())
     (ruser        ())
     (rhost        ())
     (xdisplay     ())
     (authtok_type ())) |}]
;;

let%expect_test "pam_set_item" =
  with_pam_exn
    ~f:(fun t ->
      let open Or_error.Let_syntax in
      let set item_type item = pam_set_item t ~item_type ~item in
      let%bind () = set Pam_item_type.PAM_USER "user" in
      let%bind () = set Pam_item_type.PAM_SERVICE "service" in
      let%bind () = set Pam_item_type.PAM_USER_PROMPT "prompt" in
      let%bind () = set Pam_item_type.PAM_TTY "tty" in
      let%bind () = set Pam_item_type.PAM_RUSER "ruser" in
      let%bind () = set Pam_item_type.PAM_RHOST "rhost" in
      let%bind () = set Pam_item_type.PAM_XDISPLAY "xdisplay" in
      let%bind () = set Pam_item_type.PAM_AUTHTOK_TYPE "authtok_type" in
      print_items t)
    ();
  [%expect
    {|
    ((user         (user))
     (service      (service))
     (prompt       (prompt))
     (tty          (tty))
     (ruser        (ruser))
     (rhost        (rhost))
     (xdisplay     (xdisplay))
     (authtok_type (authtok_type))) |}]
;;

(* get authtok related items outside service module is not allowed *)
let%expect_test "pam_get_item (bad item)" =
  let result = get_item Pam_item_type.PAM_AUTHTOK in
  print_s [%message "" ~_:(result : string option Or_error.t)];
  [%expect {| (Error "[pam_get_item] Bad item passed to pam_*_item() (errnum: 29)") |}];
  let result = get_item Pam_item_type.PAM_OLDAUTHTOK in
  print_s [%message "" ~_:(result : string option Or_error.t)];
  [%expect {| (Error "[pam_get_item] Bad item passed to pam_*_item() (errnum: 29)") |}]
;;

(* set authtok related items outside service module is not allowed *)
let%expect_test "pam_set_item (bad item)" =
  let result = set_item Pam_item_type.PAM_AUTHTOK "some value" in
  print_s [%message "" ~_:(result : unit Or_error.t)];
  [%expect {| (Error "[pam_set_item] Bad item passed to pam_*_item() (errnum: 29)") |}];
  let result = set_item Pam_item_type.PAM_OLDAUTHTOK "some value" in
  print_s [%message (result : unit Or_error.t)];
  [%expect
    {|
    (result (Error "[pam_set_item] Bad item passed to pam_*_item() (errnum: 29)")) |}]
;;

let%expect_test "pam_get_item (unsupported item)" =
  let show_raise_get_item item_type = show_raise (fun () -> get_item item_type) in
  show_raise_get_item Pam_item_type.PAM_CONV;
  [%expect {| (raised (Failure "Item type is not supported.")) |}];
  show_raise_get_item Pam_item_type.PAM_FAIL_DELAY;
  [%expect {| (raised (Failure "Item type is not supported.")) |}];
  show_raise_get_item Pam_item_type.PAM_XAUTHDATA;
  [%expect {| (raised (Failure "Item type is not supported.")) |}]
;;
