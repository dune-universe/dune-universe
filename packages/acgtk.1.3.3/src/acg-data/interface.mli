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

open Logic.Abstract_syntax
open Logic.Lambda

(** This module signature describes the interface for modules implementing signatures *)

module type Signature_sig =
sig
  (** Exceptions raised when definitions of types or constants are
      duplicated *)
  
  exception Duplicate_type_definition
  exception Duplicate_term_definition

  (** Exception raised when no entry associated to a given symbol
      exists in a signature *)

  exception Not_found


  (** The type of the signature as abstract object *)
  type t

  (** The type of the entries of the signature *)
  type entry

  (** The (ocaml) type for the terms of the signature *)
  type term

  (** The (ocaml) type for the types of the signature *)
  type stype

  type data =
    | Type of stype
    | Term of (term*stype)

    
  (** [empty name] returns the empty signature of name [name] *)
  val empty : (string*Abstract_syntax.location) -> t
    
  (** [name s] returns the name of the signature [s] and the location of its definition *)
  val name : t -> (string*Abstract_syntax.location)
    
  (** [add_entry e s] returns a signature where the entry [e] has been
      added *)
  val add_entry : Abstract_syntax.sig_entry -> t -> t

  (** [find_type id s] returns the type as declared or defined in the
      signature [s], corresponding to the symbol [id] in [s] if it
      exists. Raise [Not_found] otherwise*)
  (* REVIEW: Commented out because of still missing impl. *)
  (* val find_type : string -> t -> stype *)


  (** [find_term id s] returns the term together with its type, as
      declared or defined in the signature [s], corresponding to the
      symbol [id] in [s] if it exists. Raise [Not_found] otherwise*)
  val find_term : string -> t -> term * stype

    
  (** [is_atomic_type id s] returns [true] if [id] is the name of an
      atomic type in [s] and [false] oterwise *)
  val is_type : string -> t -> bool
    
  (** [is_constant id s] returns [(true,Some b)] together with its
      syntactic behaviour [b] if [id] is the name of a constant in [s]
      and [false,None] oterwise *)
  val is_constant : string -> t -> bool * Abstract_syntax.syntactic_behavior option

  (** [type_to_string ty sg] returns the string corresponding to a
      type [ty] of type {!Lambda.Lambda.stype}) with respect to the
      signature [sg] *)
    val type_to_string : stype -> t -> string 

  (** [term_to_string t sg] returns the string corresponding to a
      term [t] of type {!Lambda.Lambda.term}) with respect to the
      signature [sg] *)
    val term_to_string : term -> t -> string 

  (** [id_to_string sg id] looks up a constant defined or declared in a
      signature by its id and returns a pair of its syntactic behavior and
      name. *)
  val id_to_string : t -> int -> Abstract_syntax.syntactic_behavior*string


  val type_to_formatted_string : Format.formatter -> stype -> t -> unit
  val term_to_formatted_string : Format.formatter -> term -> t -> unit

  (** [unfold_type_definition id t] returns the actual type for the
      type defined by [id] as the identifier of a type definition in
      the signature [t]. Fails with "Bug" if [id] does not correspond
      to a type definition *)
    val unfold_type_definition : int -> t -> Lambda.stype 

  (** [unfold_term_definition id t] returns the actual term for the
      term defined by [id] as the identifier of a term definition in
      the signature [t]. Fails with "Bug" if [id] does not correspond
      to a term definition *)
    val unfold_term_definition : int -> t -> Lambda.term 

    (** [expand_type t sg] returns a type where all the type
	definitions have been expanded *)
    val expand_type : Lambda.stype -> t -> Lambda.stype

    (** [expand_term t sg] returns a term where all the term
	definitions have been expanded *)
    val expand_term : Lambda.term -> t -> Lambda.term
      
  (** [add_warnings w s ] resturns a signature where the warning [w]
      have been added *)
  val add_warnings : Error.warning list -> t -> t

  (** [get_warnings sg] returns the warnigs emitted while parsing [sg]. *)
  val get_warnings : t -> Error.warning list

  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string

  (** [term_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  (*  val term_to_string : term -> t -> string *)
  (*  val raw_to_string : term -> string*)
    
  (** [type_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  (*  val type_to_string : stype -> t -> string *)
    
  (** [convert_term t ty sg] returns a the term corresponding to the
      parsed term [t] with parsed type [ty] wrt to the signature [sg]
  *)
  val convert_term : Abstract_syntax.term -> Abstract_syntax.type_def -> t -> term * stype
    
  (** [convert_type ty sg] returns a type to the parsed type [ty] wrt
      to the signature [sg] *)
  val convert_type : Abstract_syntax.type_def -> t -> stype
    
  (** [type_of_constant n sg] returns the type of the constant of name
      [n] as defined in the signature [sg] *)
  val type_of_constant : string -> t -> stype

  (** [typecheck t ty sg] returns a term if, according to [sg], it has
      type [ty] *)
  val typecheck : Abstract_syntax.term -> stype -> t -> term

  (** [fold f a sg] returns [f e_n (f e_n-1 ( ... ( f e_1 a) ... ))]
      where the [e_i] are the entries of the signature [sg]. It is
      ensured that the [e_i] are provided in the same order as they
      have been inserted. *)
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a

  (** [extract e sig] returns a data depending of the content of the
      entry [e] *)
  (* REVIEW: Commented out because of still missing impl. *)
  (* val extract : entry -> t -> data *)

  (** [get_binder_argument_functionnal_type s sg] returns [None] if
      the constant [s] is not defined in [sg] as a binder (that is
      something of type [ ('a ?> 'b) ?> 'c ]) and returns [Some abs]
      where [abs] is {!Abstract_syntax.Abstract_syntax.abstraction.Linear} or
      {!Abstract_syntax.Abstract_syntax.abstraction.Non_linear} otherwise and
      [abs] desribes the implication [?>] in [('a ?> 'b)] *)
  val get_binder_argument_functional_type : string -> t -> Abstract_syntax.abstraction option

  (** [is_declared e sg] returns [Some s] if the entry [e] is a
      declaration of the string [s] (and not a definiton) in [sg] and
      [None] otherwise *)
  val is_declared : entry -> t -> string option

    (** [eta_long_form t ty sg] returns the eta-long form of [t] with
	respect to the type [ty] and signature [sg]*)
  val eta_long_form : term -> stype -> t -> term

  val unfold : term -> t -> term

  (** [is_2nd_order s] returns [true] if the signature [s] is 2nd
      order and [false] otherwise. *)
  val is_2nd_order : t -> bool

end

(** This module signature describes the interface for modules implementing lexicons *)
module type Lexicon_sig =
sig
  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  type t
  module Signature:Signature_sig with type term=Lambda.term
  type signature = Signature.t
  type resume
  type dependency =
    | Signatures of (signature*signature)
    | Lexicons of (t*t)

  type data =
    | Signature of signature
    | Lexicon of t

  val get_dependencies : t -> dependency

  val empty : (string*Abstract_syntax.location) -> ?non_linear:bool -> abs:signature -> obj:signature -> t 
  val name : t -> (string*Abstract_syntax.location)
  val insert : Abstract_syntax.lex_entry -> t -> t
  val to_string : t -> string
  val interpret_type : Signature.stype -> t -> Signature.stype
  val interpret_term : Lambda.term -> t -> Lambda.term
  val interpret : Signature.term -> Signature.stype -> t -> (Signature.term*Signature.stype)
  val get_sig : t -> (signature*signature)
  val check : t -> unit
  (** [parse t stype lex] tries to parse the (object) term [t] and
      find it an antecedent of type [stype] by [lex] *)
  val parse : Signature.term -> Signature.stype -> t -> resume
  val get_analysis : resume -> t -> Lambda.term option * resume
  val compose: t -> t -> (string*Abstract_syntax.location) -> t
  val program_to_buffer : t -> Buffer.t
  val query_to_buffer : Signature.term -> Signature.stype -> t -> Buffer.t
  val interpret_linear_arrow_as_non_linear : t -> bool
  val update : t -> (string -> data) -> t

end
