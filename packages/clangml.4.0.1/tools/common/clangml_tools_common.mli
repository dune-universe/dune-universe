type 'a options =
    string option -> string option -> string list -> bool -> bool -> 'a

val command_line : (string list -> 'a) -> 'a options

val options : 'a options Cmdliner.Term.t -> 'a Cmdliner.Term.t
