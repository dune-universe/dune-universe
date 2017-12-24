(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

(** This modules implements the abstract syntax and the build function for the signatures *)

module Abstract_syntax :
sig
  
  (** The type of location in the signature files *)
  type location = Lexing.position*Lexing.position

  (** The type of the syntactic behaviour of constants defined in
      the signature *)    
  type syntactic_behavior =
    | Default 
    | Prefix 
    | Infix 
    | Binder


  (** The two types of abstraction *)
  type abstraction =
    | Linear
    | Non_linear
	
	
  (** The type of terms provided by the parser. *)
  type term =
    | Var of string * location
	(** If the term is variable (bound by a binder)*)
    | Const of string * location
	(** If the term is a constant (not bound by a binder) *)
    | Abs of string * location * term * location
	(** If the term is a intuitionistic abstraction. The first
	    location is the one of the variable, and the second one is
	    the one of the whole term. The latter take into account
	    the binder if the string is the first variable bound, and
	    starts with the string otherwise *)
    | LAbs of string * location * term * location
	(** If the term is a linear abstraction *)
    | App of term * term * location
	(** If the term is an application *)	
	

  val unlinearize_term : term -> term
	
  (** The type of types as found in the signature files *)
	
  type type_def =
    | Type_atom of string * location * term list
	(** If the type is atomic. The third parameter is the terms to
	    which the type is applied in case of a dependent type. The
	    list is empty if the type does not depend on any type *)
    | Linear_arrow of type_def * type_def * location
	(** If the type is described with a linear abstraction *)
    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
	(*
	  | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
	      | Type_Abs of (string * location * type_def)  * location
	(** If the type is a dependent type build with an abstraction *) *)
	
  type sig_entry = 
    | Type_decl of (string * location * kind)
	(** The first parameter ([string]) is the name of the type,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its kind *)
    | Type_def of (string * location * type_def * kind)
	(** Tthe first parameter ([string]) is the name of the defined type,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its value *)
    | Term_decl of (string * syntactic_behavior * location * type_def)
	(** The first parameter ([string]) is the name of the constant,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its type *)
    | Term_def of (string * syntactic_behavior * location * term * type_def)
	(** The first parameter ([string]) is the name of the constant,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its value *)
  and kind = K of type_def list


  type lex_entry =
    | Type of (string * location * type_def )
    | Constant of (string * location * term )
end
  



