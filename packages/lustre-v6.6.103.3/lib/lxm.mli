(* Time-stamp: <modified the 07/09/2017 (at 15:14) by Erwan Jahier> *)

(** Lexemes *)

(* Common to lus2lic and lic2loc  *)


type t
type pragma = Pragma of string * string

val dummy : string -> t

val str    : t -> string
val id     : t -> Lv6Id.t
val line   : t -> int 
val file   : t -> string
val pragma : t -> pragma list

val override_name : string -> t -> t
                         
(** column numbers *)
val cstart : t -> int
val cend   : t -> int


(** lexer/parser interface
    
    In order to able to compute line and colums numbers, 
    the lexer is supposed to:
    - use Lxm.make to return a lexeme to the parser
    - use newline to add a new line 
*)
val make     : Lexing.lexbuf -> t
val new_line : Lexing.lexbuf -> unit

(* remove the quotes from the string *)
val make_string: Lexing.lexbuf -> t

val add_pragma : t -> pragma list -> t

(** compiler interface *)

(** used to attach a source information to a lexeme *)
type 'a srcflagged = { src : t ; it  : 'a }

val flagit : 'a -> t -> 'a srcflagged

(** Returns the last created lexem. Useful to locate syntax errors. *)
val last_made : unit -> t

(** Erreur/Warning printing *)

val details : t -> string
val short_details : t -> string
(** prints something like: 'machin' (line:10, col:3 to 7) *)

val position : t -> string
(** prints something like: line:10, col:3 to 7 *)

