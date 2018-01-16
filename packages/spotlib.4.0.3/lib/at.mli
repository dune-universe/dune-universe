(** Small \@var\@ replacement language *)

(* Requires Str *)

val replace_variables : (string -> string) -> string -> string
(** [replace_variables f s] replaces the occurrences of matches with 
    regexp "[A-Za-z0-9_]+" surrounded by '@' chars in [s] by function [f].

    Ex. [replace_variables String.uppercase "hello \@world\@" = "hello WORLD"]
*)

val replace_file : (string -> string) -> string -> string -> unit
(** [replace_variables f infile outfile] replaces the occurrences of matches with 
    regexp "[A-Za-z0-9_]+" surrounded by '@' chars by function [f] in a file [infile] 
    and write the result to a file [outfile].
*)
