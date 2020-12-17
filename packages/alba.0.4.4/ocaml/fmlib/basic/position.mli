(** Represents a position in a text file. *)


type t (** Position in a text file. *)


type range = t * t (* A range in a text file. *)




(** Print in memory source files with error markers. *)
module Print (PP: Pretty_printer.SIG):
sig
    (** [print_source_lines lines range] Print the source file given as a
        sequence of lines with line numbers and highlight the region [range].
     *)
    val print_source_lines:
        string Sequence.t -> range -> PP.t


    (** [print_source src range] Print the source file given as a
        string with line numbers and highlight the region [range].
     *)
    val print_source: string -> range -> PP.t
end




(** Make a position with points to the start of a textfile. *)
val start: t



(** Get the line number. First line is line 0. *)
val line: t -> int



(** Get the column number. First column is column 0. *)
val column: t -> int



(** Advance the position by using the next character. If the next character is a
    newline, then the line number is increment and the column number is reset to
    0.
*)
val next: char -> t -> t



(** Advance the position to the start of the next line. *)
val next_line: t -> t



(** Advance the column position by 1. *)
val next_column: t -> t
