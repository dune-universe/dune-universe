type t

(* Simple interface *)

val html_of_commonmark : string -> string

(* Complex interface *)

type render_flag = [ `SourcePos | `HardBreaks | `Safe | `Unsafe ]

type parse_flag = [ `Normalize | `ValidateUTF8 | `Smart ]

(* Parsing *)

val of_string : ?flags:parse_flag list -> string -> t

val of_file :
  ?flags:parse_flag list -> string -> [ `Ok of t | `Error of string ]

(* Rendering *)

val to_xml : ?flags:render_flag list -> t -> string

val to_html : ?flags:render_flag list -> t -> string

val to_man : ?flags:render_flag list -> width:int -> t -> string

val to_commonmark : ?flags:render_flag list -> width:int -> t -> string

val to_latex : ?flags:render_flag list -> width:int -> t -> string
