open Fmlib

type pos = Character_parser.Position.t
type range = pos * pos


(**

Pretty printer for some standard messages needed by the Alba compiler.

E.g.

- Error header

- Source files with error markers

*)
module Make (PP: Pretty_printer.SIG):
sig
    val print_error_header: string -> PP.t

    val print_source: string -> range -> int list -> PP.t
    (**
        [print_source source_string range error_tabs]

        Print the source string with line numbers and put error markers around
        [range]. End the printing at the end of [range].

        Add tab positions for the error tabs.
    *)

    val print_source_lines:
        string Sequence.t -> range -> int list -> PP.t
end

