open! Core
open Pam

(* The service [other] comes with the recent [pam] package *)
let default_service = "other"
let default_user = "foo"

let with_pam ?(user = default_user) ?(service = default_service) ~f () =
  let open Or_error.Let_syntax in
  let conv _ = Error Pam_conv.Result.PAM_CONV_ERR in
  let%bind t = pam_start ~service ~user ~conv in
  let value = f t in
  let%bind () = pam_end t in
  value
;;

let with_pam_exn ?user ?service ~f () = with_pam ?user ?service ~f () |> ok_exn
