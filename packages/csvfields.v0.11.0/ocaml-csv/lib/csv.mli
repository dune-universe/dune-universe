(** csv.mli - comma separated values parser
  *
  * $Id: csv.mli,v 1.5 2005/05/24 13:52:50 rich Exp $
  *)


type t = string list list
(** Representation of CSV files. *)

exception Bad_CSV_file of string
(** Badly formed CSV files throw this exception. *)

val lines : t -> int
(** Work out the number of lines in a CSV file. *)

val columns : t -> int
(** Work out the (maximum) number of columns in a CSV file. Note that each
  * line may be a different length, so this finds the one with the most
  * columns.
  *)

val load_in : ?separator:char -> in_channel -> t
(** Load a CSV file.
  * @param chan Input file stream
  *)

val load : ?separator:char -> string -> t
(** Load a CSV file.
  * @param filename CSV filename.
  *)

val load_string : ?separator:char -> string -> t
(** Load a CSV from a string.
  * @param s String whose contents is the entire CSV.
  *)

val load_rows : ?separator:char -> (string list -> unit) -> in_channel -> unit
(** For very large CSV files which cannot be processed in memory at once,
  * this function is appropriate. It parses the input one row at a time and
  * calls your function once for each row.
  *
  * Note that if you CSV file contains cells which have embedded
  * line feeds, then it is non-trivial to parse these lines and
  * pass them correctly to [load_rows].
  *
  * @param f Callout function.
  * @param chan Input file stream.
  *)

val load_rows_inchar :
  ?separator:char -> (string list -> unit) -> (unit -> char) -> unit
(** Works exactly like [load_rows] except that instead of reading from
  * a descriptor, it calls a read_char function.  This function
  * throws [End_of_file] on the end of input.
  *)

val trim : ?top:bool -> ?left:bool -> ?right:bool -> ?bottom:bool -> t -> t
(** This takes a CSV file and trims empty cells.
  *
  * All four of the option arguments ([~top], [~left], [~right], [~bottom])
  * default to [true].
  *
  * The exact behaviour is:
  *
  * [~right]: If true, remove any empty cells at the right hand end of
  * any row.  The number of columns in the resulting CSV structure will
  * not necessarily be the same for each row.
  *
  * [~top]: If true, remove any empty rows (no cells, or containing just empty
  * cells) from the top of the CSV structure.
  *
  * [~bottom]: If true, remove any empty rows from the bottom of the
  * CSV structure.
  *
  * [~left]: If true, remove any empty columns from the left of the
  * CSV structure.  Note that [~left] and [~right] are quite different:
  * [~left] considers the whole CSV structure, whereas [~right] considers
  * each row in isolation.
  *)

val square : t -> t
(** Make the CSV data "square" (actually rectangular).  This pads out
  * each row with empty cells so that all rows are the same length as
  * the longest row.  After this operation, every row will have length
  * {!columns}.
  *)

val associate : string list -> t -> (string * string) list list
(** [associate header data] takes a block of data and converts each
  * row in turn into an assoc list which maps column header to data cell.
  *
  * Typically a spreadsheet will have the format:
  * {v
  *   header1   header2   header3
  *   data11    data12    data13
  *   data21    data22    data23
  *     ...
  * v}
  *
  * This function arranges the data into a more usable form which is
  * robust against changes in column ordering.  The output of the
  * function is:
  * {v
  *   [ ["header1", "data11"; "header2", "data12"; "header3", "data13"];
  *     ["header1", "data21"; "header2", "data22"; "header3", "data23"];
  *     etc. ]
  * v}
  *
  * Each row is turned into an assoc list (see {!List.assoc}).
  *
  * If a row is too short, it is padded with empty cells ([""]).  If
  * a row is too long, it is truncated.
  *
  * You would typically call this function as:
  *
  * {v
  * let header, data = match csv with h :: d -> h, d | [] -> assert false;;
  * let data = Csv.associate header data;;
  * v}
  *
  * The header strings are shared, so the actual space in memory consumed
  * by the spreadsheet is not much larger.
  *)

val print : ?separator:char -> t -> unit
(** Print string list list - same as [save_out stdout] *)

val save_fn : ?separator:char -> (string -> unit) -> t -> unit
(** Save the string list list, writing the strings by calling the given
  * given functon.
  *)

val save_out : ?separator:char -> out_channel -> t -> unit
(** Save string list list to a channel. *)

val save : ?separator:char -> string -> t -> unit
(** Save string list list to a file. *)

val print_readable : t -> unit
(** Print the CSV data to [stdout] in a human-readable format.  Not much
  * is guaranteed about how the CSV is printed, except that it will be
  * easier to follow than a "raw" output done with {!print}.  This is
  * a one-way operation.  There is no easy way to parse the output of
  * this command back into CSV data.
  *)

val save_out_readable : out_channel -> t -> unit
(** As for {!print_readable}, allowing the output to be sent to a channel. *)

val save_fn_readable : (string -> unit) -> t -> unit
