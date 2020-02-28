(** Command line parser for filter programs

{v Usage: prog [OPTIONS] [INPUT]
Options:
  -o FILE  --output=FILE  output to file (default is standard output)
  -v       --version      print version number
  -h       --help         print this message v} *)

(** [filter version run] parses the command line and then calls [run]
   with the input file name, the input channel, and the output file
   name.  String [version] is the program's version number.  The input
   channel is standard input when the input file name is the empty
   string.  The default output file name is the empty string. *)
val filter : string -> (string -> in_channel -> string -> unit) -> unit
