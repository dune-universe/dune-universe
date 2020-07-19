type 'a options =
    string option -> string option -> string list -> bool -> bool ->
      string list -> 'a

val command_line : (Clang.language -> string list -> 'a) -> 'a options

val options : 'a options Cmdliner.Term.t -> 'a Cmdliner.Term.t
