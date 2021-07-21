
(**----------------------------------------------------------

                 COMPILATION/EXPANSION

------------------------------------------------------------

Identificateurs et remontée au source

DOIT ETRE REMIS A ZERO entre 2 utilisations



Au niveau des identificateurs, il faut distinguer :
- les idents source (type [[Syntaxe.ident]], donc parfaitement
identifiés au niveau source,
- les idents target qui sont des chaînes uniques par construction.


Principe de la pile de remontée :

- Tout ident apparait dans le E1 d'un "let f = E1 in E2",
  (ou d'un let f = E1, ou d'un node n ... = E1)
- Cette référence est identifiée par son lexeme, plus
  le lexeme qui correspond à la déclaration du f
- Dans le cas de l'expansion, ce f est traité car on 
  est tombé au préalable sur une référence à f, dans un
  autre contexte, etc.


----------------------------------------------------------*)

(**********************************************************)

open Printf

(** Ident unique dans le target *)
type t = string

let to_string tid = tid
let list_to_string tidl _sep = String.concat "," tidl

let from_string s = s

(** L'unicité des ident target est garantie par un numérotage *)

type space = int ref

let new_space () = (
	let _idcpt = ref 0
	in _idcpt
)

let get_fresh _idcpt pfx = (
   incr _idcpt;
   sprintf "_%s%03d" pfx !_idcpt
)

let get s = (
(* XXX migth clash! (in the user name one of its I/O variable "_X001" for instance)

   The correct code would be :

      sprintf "_%s" s

   but it confuses the current version of lurette *)
  sprintf "%s" s 
)

let of_cpt i = sprintf "cpt%02d" i

(** Avec l'expansion, la remontée au source se fait par une "pile" de
    couples (lexeme de la ref, lexeme du contexte), le contexte étant
    toujours une déclaration de macro (ou de noeud pour le
    top-level). La val_exp (correspondant au lexeme de la ref) sert
    pour ldbg.
*)

type src_stack = (Lexeme.t * Lexeme.t * Syntaxe.val_exp option) list

(** Au cours de la construction, on a besoin de désigner le contexte
    uniquement, d'où le type suivant qui comprend le lexeme
    de la déclaration du scope (let ou node) + la pile d'instance
    qui a conduit à ce contexte *)
type scope_stack = (Lexeme.t * src_stack)

(** Scope de base *)
let main_scope (m: Lexeme.t) = (
	(m, []);
)

let get_scope (m: Lexeme.t) (s : src_stack) = (m,s)

(** Quand on a un lexeme, dans un contexte donné par un "scope_stack", on crée
le src_stack correspondant *)
let get_src_stack lxm sstack e = (
   (lxm, fst sstack,e)::(snd sstack)
)

(** Pile de base (vide !!) *)
let base_stack () = []

let rec print_src_stack ss = (
	match ss with
		[] -> ()
	| (i,c,_) :: tl -> (
		Printf.fprintf stdout "  ident [%s] in context [%s]\n"
			(LutErrors.lexeme_details i) 
			(LutErrors.lexeme_details c) ;
		print_src_stack tl
	)
)

let rec string_of_src_stack ss = (
	match ss with
		[] -> ""
	| (i,c,_) :: tl -> (
		(
			Printf.sprintf "  ident [%s] in context [%s]\n"
				(LutErrors.lexeme_details i) 
				(LutErrors.lexeme_details c) 
		) ^ (
			string_of_src_stack tl
		)
	)
)

let head_of_src_stack ss = (
	match ss with
	| (i,_c,_)::_ -> i
	| [] -> raise Not_found
)
