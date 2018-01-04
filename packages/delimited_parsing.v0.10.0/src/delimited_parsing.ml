(** [Delimited_parsing] contains parsers for three common file formats: *)

(** [Character_separated_without_quoting] parses fields separated by a character, where
    fields may contain escaped characters (e,g, [\n]) but fields may not be quoted (e.g.,
    ["foo bar"]). *)
module Character_separated_without_quoting      = Character_separated_without_quoting

(** [Csv] parses character-separated values where fields may be quoted and quotation
    marks within quoted fields are escaped with another quotation mark, MSExcel-style. *)
module Csv                                      = Csv

(** [Positional] parses fixed-width fields. *)
module Positional                               = Positional

(** {1} For upgrading or maintaining legacy code. *)

(** [Replace_deprecated_csv] is a drop-in replacement for existing users of
    [Async_extended.Delimited].  It is faster, tested, and supported.

    Whereas [Async_extended.Delimited] used [Deprecated_csv], [Replace_deprecated_csv]
    implements the same interface using [Csv]. Thus, code written against
    [Async_extended.Delimited] can upgrade to the new parser with one line: {[

      open Delimited_parsing.Replace_deprecated_csv

    ]} *)
module Replace_deprecated_csv                   = Replace_deprecated_csv

(** {1} Modules shared between multiple parsers. *)

module Header                                   = Header
module Row                                      = Row

(** {1} The following are exposed only for backwards-compatibility or testing. *)

module Deprecated                               = Deprecated
module Deprecated_intf                          = Deprecated_intf
module Deprecated_csv                           = Deprecated_csv
