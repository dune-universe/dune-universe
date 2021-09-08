
(** to make signatures more readable *)
type filename = string

(** work with open input file *)
val with_in_file: filename -> (in_channel -> 'a) -> 'a

(** work with open output file *)
val with_out_file: filename -> (out_channel -> 'a) -> 'a

(** work with open input and output files *)
val with_infile_outfile:
  filename -> filename -> (in_channel -> out_channel -> 'a) -> 'a

(** read all lines from file *)
val lines_of_file: filename -> string list

(** reverse read all lines from file *)
val rev_lines_of_file: filename -> string list

(** write all lines to file *)
val lines_to_file: filename -> string list -> unit

(** iter on lines of file *)
val iter: filename -> (string -> unit) -> unit

(** iteri on lines of file *)
val iteri: filename -> (int -> string -> unit) -> unit

(** map on lines of file *)
val map: filename -> (string -> 'a) -> 'a list

(** mapi on lines of file *)
val mapi: filename -> (int -> string -> 'a) -> 'a list

(** reverse map on lines of file *)
val rev_map: filename -> (string -> 'a) -> 'a list

(** fold on lines of file *)
val fold: filename -> ('a -> string -> 'a) -> 'a -> 'a

(** filter lines of file *)
val filter: filename -> (string -> bool) -> string list

(** count lines of file *)
val count: filename -> int

(** alias for count *)
val length: filename -> int

(** [save fn x]: marshal [x] to file [fn] *)
val save: filename -> 'a -> unit

(** [restore fn]: unmarshal value from whole file [fn] *)
val restore: filename -> 'a
