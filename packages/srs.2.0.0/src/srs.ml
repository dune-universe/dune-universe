type t

exception Error of string

let _ = Callback.register_exception "SRS.Error" (Error "")

external create : unit -> t = "caml_srs_new"
external add_secret : t -> string -> unit = "caml_srs_add_secret"
external forward : t -> string -> string -> string = "caml_srs_forward"
external reverse : t -> string -> string = "caml_srs_reverse"
external set_separator : t -> char -> unit = "caml_srs_set_separator"
external get_separator : t -> char = "caml_srs_get_separator"
external set_max_age : t -> int -> unit = "caml_srs_set_maxage"
external get_max_age : t -> int = "caml_srs_get_maxage"
external set_hash_length : t -> int -> unit = "caml_srs_set_hashlength"
external get_hash_length : t -> int = "caml_srs_get_hashlength"
external set_hash_min : t -> int -> unit = "caml_srs_set_hashmin"
external get_hash_min : t -> int = "caml_srs_get_hashmin"
external set_no_forward : t -> bool -> unit = "caml_srs_set_noforward"
external get_no_forward : t -> bool = "caml_srs_get_noforward"
external set_no_reverse : t -> bool -> unit = "caml_srs_set_noreverse"
external get_no_reverse : t -> bool = "caml_srs_get_noreverse"

let make (secret, secrets) max_age hash_len sep =
  let srs = create () in
  set_separator srs sep;
  set_hash_length srs hash_len;
  add_secret srs secret;
  List.iter (add_secret srs) secrets;
  srs
