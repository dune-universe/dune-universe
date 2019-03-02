type t =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }

val email_only : t Angstrom.t
val email_list_only : t list Angstrom.t
