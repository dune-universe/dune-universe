(*
  Un module au dessus de lexer et parser
  pour éviter les dépendances bouclées
*)

val new_line : Lexing.lexbuf -> unit

type t = { 
  str : string ; 
  file : string ;
  line : int ; 
  cstart : int ; cend : int; (* column *)
  chstart : int ; chend : int; (* character *)
}
val to_string : t -> string

type 'a srcflaged = { src : t ; it  : 'a }

val flagit : 'a -> t -> 'a srcflaged

val dummy : t

val set_current_file : string -> unit
val make : Lexing.lexbuf -> t

val last_made : unit -> t

