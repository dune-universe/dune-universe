(* Time-stamp: <modified the 07/09/2017 (at 15:14) by Erwan Jahier> *)

(** Common to lus2lic and lic2loc  *)

let new_line ( lexbuf ) = (
  Lv6MainArgs.global_opt.Lv6MainArgs.line_start_pos <- Lexing.lexeme_end lexbuf;
  Lv6MainArgs.global_opt.Lv6MainArgs.line_num <- Lv6MainArgs.global_opt.Lv6MainArgs.line_num +1;
  ()
)

(* exported *)
type pragma = Pragma of string * string

(* le type ``lexeme'', string + info source *)
type t = {
        _file : string ;
        _str : string ;
        _line : int ;
        _cstart : int ;
        _cend : int;
	     _pragma : pragma list
}

let str x = (x._str)
let id x = (Lv6Id.of_string x._str)
let line x = (x._line)
let cstart x = (x._cstart)
let cend x = (x._cend)
let file x = x._file
let pragma x = x._pragma
(* affichage standard: *)
let details lxm = (
  let file = if Lv6MainArgs.global_opt.Lv6MainArgs.nonreg_test then 
    (* during non-regression test, having absolute paths printed
       complicate the perusal (because of the diff output). *)
    Filename.basename lxm._file 
  else 
    lxm._file 
  in
    Printf.sprintf "in file \"%s\", line %d, col %d to %d, token '%s'"
      file lxm._line lxm._cstart lxm._cend lxm._str 
)
(* affichage compact: *)
let short_details lxm = (
   let file = Filename.basename lxm._file in
   Printf.sprintf "f:%s, l:%d, c:%d-%d, t:'%s'"
      file lxm._line lxm._cstart lxm._cend lxm._str 
)
let position lxm = (
  Printf.sprintf "line:%d, col:%d to %d"
    lxm._line lxm._cstart lxm._cend
)
let (override_name : string -> t -> t) =
  fun nname lxm ->
  { lxm with _str=nname }

(* constructeur de type flaggé avec un lexeme *)
type 'a srcflagged = {
   src : t ;
   it  : 'a
}
(* flagage d'une valeur quelconque *) 
let (flagit : 'a -> t -> 'a srcflagged) = 
  fun x lxm -> 
    { it = x; src = lxm }


let dummy str = 
  {
    _str = str ;  
    _file = "dummy"; 
    _line = 0 ; 
    _cstart = 0 ; 
    _cend = 0 ;
    _pragma = []
  }

let last_lexeme = ref (dummy "")

open Lv6MainArgs

let make ( lexbuf ) = (
  let s = (Lexing.lexeme lexbuf) in
  let l = global_opt.line_num in
  let c1 = (Lexing.lexeme_start lexbuf - global_opt.line_start_pos) in
  let c2 = (Lexing.lexeme_end lexbuf - global_opt.line_start_pos - 1) in
    last_lexeme := 
      { _str = s ; 
        _file = global_opt.current_file; 
        _line = l; 
        _cstart = c1 ; 
        _cend = c2 ;
	_pragma = []
      };
    !last_lexeme
)

let add_pragma lxm pl = { lxm with _pragma = pl }
let make_string ( lexbuf ) = 
  let lxm = make lexbuf in
    { lxm with _str = String.sub lxm._str 1 ((String.length lxm._str)-2) }



let last_made () = !last_lexeme 
