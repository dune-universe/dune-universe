external init
  : unit -> unit
  = "caml_tz_set" [@@noalloc]
external get
  : unit -> int
  = "caml_tz_get" [@@noalloc]
