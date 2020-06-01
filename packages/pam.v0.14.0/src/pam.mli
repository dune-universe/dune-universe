(** [Pam] contains functions that interface to the PAM library. *)

open! Core

module Pam_conv : sig
  module Message : sig
    type style_t =
      | PAM_PROMPT_ECHO_OFF
      | PAM_PROMPT_ECHO_ON
      | PAM_ERROR_MSG
      | PAM_TEXT_INFO

    type t

    val create : style:style_t -> message:string -> t
    val style : t -> style_t
    val message : t -> string
  end

  module Response : sig
    type t

    val create : resp:string option -> t
  end

  module Result : sig
    type error_t =
      | PAM_BUF_ERR
      | PAM_CONV_ERR

    type t = (Response.t list, error_t) Result.t
  end
end

module Pam_auth : sig
  module Flag : sig
    type t =
      | PAM_SILENT
      | PAM_DISALLOW_NULL_AUTHTOK
  end
end

module Pam_acct : sig
  module Flag : sig
    type t =
      | PAM_SILENT
      | PAM_DISALLOW_NULL_AUTHTOK
  end
end

module Pam_cred : sig
  module Flag : sig
    type t =
      | PAM_ESTABLISH_CRED
      | PAM_DELETE_CRED
      | PAM_REINITIALIZE_CRED
      | PAM_REFRESH_CRED
  end
end

module Pam_authtok : sig
  module Flag : sig
    type t =
      | PAM_SILENT
      | PAM_CHANGE_EXPIRED_AUTHTOK
  end
end


type pam_conv
type pam_fail_delay
type pam_xauth_data

module Pam_item_type : sig
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

module Pam_session : sig
  module Flag : sig
    type t = PAM_SILENT
  end
end

(** [t] is an abstract type representing [pam_handle_t] in the C-library *)
type t

(** [pam_start] initiates a pam transaction. This function works like its C-version except
    that it calls [pam_end] on a failure. This should make it easier to chain pam
    operations in the caller code. *)
val pam_start
  :  service:string
  -> user:string
  -> conv:((Pam_conv.Message.t, string) Result.t list -> Pam_conv.Result.t)
  -> t Or_error.t

val pam_end : t -> unit Or_error.t
val pam_authenticate : t -> flags:Pam_auth.Flag.t list -> unit Or_error.t
val pam_acct_mgmt : t -> flags:Pam_acct.Flag.t list -> unit Or_error.t
val pam_setcred : ?silent:bool -> t -> flag:Pam_cred.Flag.t -> unit Or_error.t
val pam_chauthtok : t -> flags:Pam_authtok.Flag.t list -> unit Or_error.t
val pam_open_session : t -> flags:Pam_session.Flag.t list -> unit Or_error.t
val pam_close_session : t -> flags:Pam_session.Flag.t list -> unit Or_error.t
val pam_getenv : t -> key:string -> string Or_error.t
val pam_putenv : t -> key:string -> data:string -> unit Or_error.t
val pam_unsetenv : t -> key:string -> unit Or_error.t
val pam_getenvlist : t -> string list Or_error.t
val pam_get_item : t -> item_type:'a Pam_item_type.t -> 'a option Or_error.t
val pam_set_item : t -> item_type:'a Pam_item_type.t -> item:'a -> unit Or_error.t
