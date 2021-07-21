
(*-----------------------------------------------------------
Interface avec la libdl C standard (POSIX)
-----------------------------------------------------------*)
type t
type cfunc

(* stub pour les fonction c :
toutes terminent normalement ou lèvent Failure "explication"
 *)
external dlopen : string -> t  = "c_dlopen"
external dlsym : t -> string -> cfunc  = "c_dlsym"
external dlclose : t -> unit  = "c_dlclose"

(*-----------------------------------------------------------
Utilitaires pour l'appel des fonctions des dl 
*)
type cptr

external cptr_of : 'a -> cptr = "c_cptr_of"

(* Quelques "profils" standards sont fournis *)
external f2f : cfunc -> float -> float = "c_f2f"
external ff2f : cfunc -> float -> float -> float = "c_ff2f"

external i2i : cfunc -> int -> int = "c_i2i"
external ii2i : cfunc -> int -> int -> int = "c_ii2i"
external iii2i : cfunc -> int -> int -> int -> int = "c_iii2i"

(* pour les autres profils, les arguments sont passés dans une liste *)
type carg =
	Int_carg of int
|	Double_carg of float 
|	String_carg of string 
|	Ptr_carg of cptr 

type cargs = carg list

let (carg_to_string: carg -> string) =
  fun carg -> 
    match carg with
      | Int_carg i -> string_of_int i
      |	Double_carg f -> string_of_float f
      |	String_carg str -> str
      |	Ptr_carg _cptr -> "<C pointer>"


(* mais il faut quand même spécifier le type du résultat attendu ! *)

external cargs2i : cfunc -> cargs -> int = "c_cargs2i"
external cargs2f : cfunc -> cargs -> float = "c_cargs2f"
external cargs2s : cfunc -> cargs -> string = "c_cargs2s"

