(*-----------------------------------------------------------------------
** Copyright (C) 2003 - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: lexeme.mli
** Author: Pascal Raymond
*)

(*
  Un module au dessus de lexer et parser
  pour éviter les dépendances bouclées
*)

(* pour calculer line/col *)
let line_num = ref 1
let line_start_pos = ref 0
  

let new_line ( lexbuf ) = (
  line_start_pos := Lexing.lexeme_end lexbuf;
  incr line_num;
  ()
)

(* le type ``lexeme'', string + info source *)
type t = { 
  str : string ; 
  file : string ; 
  line : int ; 
  cstart : int ; cend : int; (* column *)
  chstart : int ; chend : int; (* character *)
}

let to_string x = 
  Printf.sprintf "'%s'(%s:%d:%d-%d)" x.str x.file x.line x.cstart x.cend


(* constructeur de type flaggé avec un lexeme *)
type 'a srcflaged = {
   src : t ;
   it  : 'a
}
    (* flagage d'une valeur quelconque *)
let (flagit : 'a -> t -> 'a srcflaged) =
  function x -> function lxm ->
    { it = x; src = lxm }

let curr_file = ref "stdin"

let set_current_file f = 
  line_num := 1;
  curr_file := f

let dummy = { 
  str = "dummy" ; file = !curr_file ; line = 0 ; cstart = 0 ; cend = 0; 
  chstart = 0 ; chend = 0 }
let last_lexeme = ref dummy 

let make lexbuf =
  let s = (Lexing.lexeme lexbuf) in
  let l = !line_num in
  let ch1 =  (Lexing.lexeme_start lexbuf)
  and ch2 =  (Lexing.lexeme_end lexbuf)
  and c1 = (Lexing.lexeme_start lexbuf - !line_start_pos) 
  and c2 = (Lexing.lexeme_end lexbuf - !line_start_pos) 

  in
    last_lexeme := { 
      str = s; file = !curr_file; 
      line = l; cstart = c1 ; cend = c2 ; chstart = ch1 ; chend = ch2};
    (* printf "making lexeme '%s'\n" s; flush stdout; *)
    !last_lexeme



let last_made () = !last_lexeme
