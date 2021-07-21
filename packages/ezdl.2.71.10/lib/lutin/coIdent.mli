(** COMPILATION/EXPANSION : idents et retour au source

------------------------------------------------------------

Identificateurs et remontée au source

C'est un peu du luxe, mais on distingue :

- les ident source, Syntaxe.ident (sous-entendu avec src_info, donc UNIQUES),

- les ident target (de simple string, mais uniques par construction).

N.B. ça a beau être de simple string, on en fait quand même
un type abstrait, au cas où ...

----------------------------------------------------------*)

(**********************************************************)

(** remet le module a zero ... *)
type space
val new_space : unit -> space 

(** Ident dans target, unique par nommage *)
type t = string

val to_string : t -> string
val list_to_string : t list  -> string -> string

(** L'unicité des idents target est garantie par la fonction suivante,
qui prend un préfixe en paramètre
*)
val get_fresh : space -> string -> t

(** Si on est sûr qu'il n'y aurra pas de problème, on peut
forcer un nom *)
val get : string -> t

(* specifique aux compteurs : pour être sur
   que le compteur no i a toujours le meme nom !
*)
val of_cpt : int -> t

(** Si le nom doit rester tel quel *)
val from_string : string -> t

(** REMONTÉE AU SOURCE DANS LE PROGRAMME EXPANSÉ *)

(** Remontée au source d'un "target" depuis le code expansé *)
type src_stack = (Lexeme.t * Lexeme.t * Syntaxe.val_exp option) list

(** Remontée au source d'un scope depuis le code expansé *)
type scope_stack = (Lexeme.t * src_stack)


(** Scope de base *)
val main_scope : Lexeme.t -> scope_stack
val get_scope : Lexeme.t -> src_stack -> scope_stack 

val base_stack : unit -> src_stack

(** Avec un scope_stack et une instance de Syntaxe.ident,
    on fabrique un src_stack complet *)
val get_src_stack : Lexeme.t -> scope_stack -> Syntaxe.val_exp option -> src_stack

val print_src_stack : src_stack -> unit
val string_of_src_stack : src_stack -> string 
val head_of_src_stack : src_stack -> Lexeme.t 

