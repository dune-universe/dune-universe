open! Core

val with_pam
  :  ?user:string
  -> ?service:string
  -> f:(Pam.t -> 'a Or_error.t)
  -> unit
  -> 'a Or_error.t

val with_pam_exn
  :  ?user:string
  -> ?service:string
  -> f:(Pam.t -> 'a Or_error.t)
  -> unit
  -> 'a
