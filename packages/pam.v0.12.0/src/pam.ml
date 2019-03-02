open! Core

(* [t] is an abstract type which holds the pam context *)
type t

module Pam_error = struct
  type t = int
end

module Pam_conv = struct
  module Message = struct
    type style_t =
      | PAM_PROMPT_ECHO_OFF
      | PAM_PROMPT_ECHO_ON
      | PAM_ERROR_MSG
      | PAM_TEXT_INFO
    [@@deriving bin_io]

    type t =
      { style : style_t
      ; message : string
      }
    [@@deriving bin_io, fields]

    (* The value here should not be changed without updating the code
       and constant values in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 47df5cd77358f7105834f9fc0e807676 |}]
    ;;

    let create = Fields.create
  end

  module Response = struct
    type t =
      { resp : string option
      ; resp_retcode : int
      }
    [@@deriving bin_io, fields]

    (* Use custom create function as [pam] expects a constant [resp_retcode] *)
    let create ~resp = Fields.create ~resp ~resp_retcode:0
  end

  module Result = struct
    type error_t =
      | PAM_BUF_ERR
      | PAM_CONV_ERR
    [@@deriving bin_io]

    type t = (Response.t list, error_t) Result.t [@@deriving bin_io]

    (* The value here should not be changed without updating the return
       code and ordering in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 622288696dcfeb2b5599101c06a18a26 |}]
    ;;
  end
end

module Pam_result = struct
  type 'a t = ('a, Pam_error.t) Result.t
end

module Pam_auth = struct
  module Flag = struct
    type t =
      | PAM_SILENT
      | PAM_DISALLOW_NULL_AUTHTOK
    [@@deriving bin_io]

    (* The value here should not be changed without updating the return
       code and ordering in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 1d56cbf13292909f3ae9b88709273566 |}]
    ;;
  end
end

module Pam_acct = struct
  module Flag = struct
    type t =
      | PAM_SILENT
      | PAM_DISALLOW_NULL_AUTHTOK
    [@@deriving bin_io]

    (* The value here should not be changed without updating the return
       code and ordering in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 1d56cbf13292909f3ae9b88709273566 |}]
    ;;
  end
end

module Pam_cred = struct
  module Flag = struct
    type t =
      | PAM_ESTABLISH_CRED
      | PAM_DELETE_CRED
      | PAM_REINITIALIZE_CRED
      | PAM_REFRESH_CRED
    [@@deriving bin_io]

    (* The value here should not be changed without updating the return
       code and ordering in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| f303df589fc8138228a35e94265f8eba |}]
    ;;
  end
end

module Pam_authtok = struct
  module Flag = struct
    type t =
      | PAM_SILENT
      | PAM_CHANGE_EXPIRED_AUTHTOK
    [@@deriving bin_io]

    (* The value here should not be changed without updating the return
       code and ordering in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 99753ea70855ca8f8bf962a186395393 |}]
    ;;
  end
end

type pam_conv
type pam_fail_delay
type pam_xauth_data

module Pam_item_type = struct
  (* The value here should not be changed or re-ordred without updating the return
     code and ordering in [pam_stub.c]. [bin_io] does not work with gadt so we cannot
     create unit test case to check like others *)
  type _ t =
    | PAM_SERVICE : string t
    | PAM_USER : string t
    | PAM_USER_PROMPT : string t
    | PAM_TTY : string t
    | PAM_RUSER : string t
    | PAM_RHOST : string t
    | PAM_AUTHTOK : string t
    | PAM_OLDAUTHTOK : string t
    | PAM_XDISPLAY : string t
    | PAM_XAUTHDATA : pam_xauth_data t
    | PAM_AUTHTOK_TYPE : string t
    | PAM_CONV : pam_conv t
    | PAM_FAIL_DELAY : pam_fail_delay t
end

module Pam_session = struct
  module Flag = struct
    type t = PAM_SILENT [@@deriving bin_io]

    (* The value here should not be changed without updating the return
       code and ordering in [pam_stub.c] *)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 5c9ce93eee7315a34bbf88ee63a24a3e |}]
    ;;
  end
end

external pam_start_c
  :  string
    -> string
    -> ((Pam_conv.Message.t, string) Result.t list -> Pam_conv.Result.t)
    -> (t, t * Pam_error.t) Result.t
  = "caml_pam_start"

external pam_end_c : t -> unit Pam_result.t = "caml_pam_end"

external pam_authenticate_c
  :  t
    -> Pam_auth.Flag.t list
    -> unit Pam_result.t
  = "caml_pam_authenticate"

external pam_acct_mgmt_c
  :  t
    -> Pam_acct.Flag.t list
    -> unit Pam_result.t
  = "caml_pam_acct_mgmt"

external pam_setcred_c
  :  t
    -> bool
    -> Pam_cred.Flag.t
    -> unit Pam_result.t
  = "caml_pam_setcred"

external pam_chauthtok_c
  :  t
    -> Pam_authtok.Flag.t list
    -> unit Pam_result.t
  = "caml_pam_chauthtok"

external pam_open_session_c
  :  t
    -> Pam_session.Flag.t list
    -> unit Pam_result.t
  = "caml_pam_open_session"

external pam_close_session_c
  :  t
    -> Pam_session.Flag.t list
    -> unit Pam_result.t
  = "caml_pam_close_session"

external pam_getenv_c : t -> string -> (string, string) Result.t = "caml_pam_getenv"
external pam_putenv_c : t -> string -> unit Pam_result.t = "caml_pam_putenv"
external pam_getenvlist_c : t -> (string list, string) Result.t = "caml_pam_getenvlist"

external pam_get_item_c
  :  t
    -> 'a Pam_item_type.t
    -> 'a option Pam_result.t
  = "caml_pam_get_item"

external pam_set_item_c
  :  t
    -> 'a Pam_item_type.t
    -> 'a
    -> unit Pam_result.t
  = "caml_pam_set_item"

external pam_strerror_c : t -> Pam_error.t -> string = "caml_pam_strerror"

let pam_strerror t errnum = pam_strerror_c t errnum
let pam_errmsg ~op errmsg = sprintf "[%s] %s" op errmsg

let pam_result_to_or_error ~op t result =
  Result.map_error result ~f:(fun errnum ->
    let errmsg = pam_errmsg ~op (pam_strerror t errnum) in
    Error.of_string (sprintf "%s (errnum: %d)" errmsg errnum))
;;

let pam_start ~service ~user ~conv =
  pam_start_c service user conv
  |> Result.map_error ~f:(fun (t, errnum) ->
    let err_desc = pam_strerror t errnum in
    ignore (pam_end_c t);
    Error.of_string err_desc)
;;

let pam_end t =
  (* We cannot use [t] after [pam_end_c] so we have to come up with an error message *)
  pam_end_c t
  |> Result.map_error ~f:(fun errnum ->
    Error.of_string
      (pam_errmsg ~op:"pam_end" (sprintf "failed to release (errnum: %d)" errnum)))
;;

let pam_authenticate t ~flags =
  pam_authenticate_c t flags |> pam_result_to_or_error ~op:"pam_authenticate" t
;;

let pam_acct_mgmt t ~flags =
  pam_acct_mgmt_c t flags |> pam_result_to_or_error ~op:"pam_acct_mgmt" t
;;

let pam_setcred ?(silent = false) t ~flag =
  pam_setcred_c t silent flag |> pam_result_to_or_error ~op:"pam_setcred" t
;;

let pam_chauthtok t ~flags =
  pam_chauthtok_c t flags |> pam_result_to_or_error ~op:"pam_chauthtok" t
;;

let pam_open_session t ~flags =
  pam_open_session_c t flags |> pam_result_to_or_error ~op:"pam_open_session" t
;;

let pam_close_session t ~flags =
  pam_close_session_c t flags |> pam_result_to_or_error ~op:"pam_close_session" t
;;

let pam_getenv t ~key =
  pam_getenv_c t key
  |> Result.map_error ~f:(Fn.compose Error.of_string (pam_errmsg ~op:"pam_getenv"))
;;

let pam_putenv t ~key ~data =
  pam_putenv_c t (sprintf "%s=%s" key data) |> pam_result_to_or_error ~op:"pam_putenv" t
;;

let pam_unsetenv t ~key = pam_putenv_c t key |> pam_result_to_or_error ~op:"pam_putenv" t

let pam_getenvlist t =
  pam_getenvlist_c t
  |> Result.map_error ~f:(Fn.compose Error.of_string (pam_errmsg ~op:"pam_getenvlist"))
;;

let pam_get_item (type a) t ~(item_type : a Pam_item_type.t) : a option Or_error.t =
  pam_get_item_c t item_type |> pam_result_to_or_error ~op:"pam_get_item" t
;;

let pam_set_item (type a) t ~(item_type : a Pam_item_type.t) ~(item : a)
  : unit Or_error.t =
  pam_set_item_c t item_type item |> pam_result_to_or_error ~op:"pam_set_item" t
;;
