type variable = [
  | `String of string
  | `List of string list
  | `Assoc of (string * string) list
]
(** The type of a variable that can be templated *)

val expand_template : template:Template.t -> variables:(string * variable) list -> string

val expand_template_with_strings : template:Template.t -> variables:(string * string) list -> string

val expand_template_with_lists : template:Template.t -> variables:(string * string list) list -> string

val expand_template_with_assoc_list : template:Template.t -> variables:(string * (string * string) list) list -> string
