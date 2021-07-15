(*----------------------------------------------------------------------
	module : Lv6Verbose
	date :
------------------------------------------------------------------------
	description :

	Affichage verbeux avec appel "printf-like" :

Lv6Verbose.put "format" args...
----------------------------------------------------------------------*)


val on : unit -> unit
val off : unit -> unit
val set : int -> unit

[@@ocaml.warning "+32"]
val level : unit -> int

[@@ocaml.warning "-32"]

(* GESTION DES FLAGS DE VERBOSE POUR LE DEBUG "FIN" 
Usage typique :

- dans un module Toto, faire au début :

let dbgflag = Global.get_dbg_flag "Toto"

- puis utiliser dans le code :

Lv6Verbose.printf ~flag:dbg ...
Lv6Verbose.exe ~flag:dbg ...
 
- si "-dbg Toto" est passé en arguments
  tout les Lv6Verbose seront pris en compte
*)
type flag
val get_flag : string -> flag
val set_flag : flag -> unit
val flag_list : unit -> string list

val level : unit -> int

(* print/execute if EITHER:
   - level (dflt=1) is >= than the set level
   - flag is set
*)

val printf : ?level:int -> ?flag:flag -> ('a, unit, string, unit) format4 -> 'a
val put    : ?level:int -> ?flag:flag -> ('a, unit, string, unit) format4 -> 'a
val print_string : ?level:int -> ?flag:flag -> string -> unit 
val exe : ?level:int -> ?flag:flag -> (unit -> unit) -> unit

val profile_info : string -> unit
