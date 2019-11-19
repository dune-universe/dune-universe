(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2021 INRIA                             *)
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
    
  (** [is_constant id s] returns [(true,Some (b,pred))] together with
      its syntactic behaviour [b] and its precedence [pred] if [id] is
      the name of a constant in [s] and [false,None] oterwise *)
  val is_constant : string -> t -> bool * Abstract_syntax.syntactic_behavior option

  (** [precedence id s] returns a [Some f] where [f] is float
      indicating the precedence of the infix operator [id]. It returns
      [None] if [id] is not an infix operator. *)
  (*  val precedence : string -> t -> (float * Abstract_syntax.associativity) option *)

  (** [new_precedence ?before id s] returns a pair consisting of a new
      precedence value associated to [id], and the new signature
      taking this new value into account. If the optional string
      argument [before] is not provided, then [id] is given the
      highest precedence so far. Otherwise, it is given the precedence
      just below [before].*)
  val new_precedence : ?before:string -> string -> t -> (float * t)
                                        
  (** [type_to_string ty sg] returns the string corresponding to a
      type [ty] of type {!Logic.Lambda.Lambda.stype}) with respect to the
      signature [sg] *)
    val type_to_string : stype -> t -> string 

  (** [term_to_string t sg] returns the string corresponding to a
      term [t] of type {!Logic.Lambda.Lambda.term}) with respect to the
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
  (*  val get_warnings : t -> Error.warning list *)

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

  (** [entry_to_data e] returns a data depending of the content of
      the entry [e] in the signature [sig]*)
  val entry_to_data : entry -> data 

  (** [get_binder_argument_functionnal_type s sg] returns [None] if
      the constant [s] is not defined in [sg] as a binder (that is
      something of type [ ('a ?> 'b) ?> 'c ]) and returns [Some abs]
      where [abs] is {!Logic.Abstract_syntax.Abstract_syntax.abstraction.Linear} or
      {!Logic.Abstract_syntax.Abstract_syntax.abstraction.Non_linear} otherwise and
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
    (** Exceptions raised when interpretations of types or constants are
      duplicated *)

  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation
              
  (** The type of the lexicon as abstract object *)
  type t

  (** It contains a module that allows for manipulating the associated signatures *)
  module Signature:Signature_sig with type term=Lambda.term
  type signature = Signature.t

  (* The type of the resumption when parses of terms are computed *)
  type resume

  (** The type of dependencies of lexicons, in order to compute when recompilation is required if a signature or a lexicon is extended *)
  type dependency =
    | Signatures of (signature*signature)
    | Lexicons of (t*t)

  type data =
    | Signature of signature
    | Lexicon of t


  (** [get_dependencies l] returns the direct dependencies of [l] *)
  val get_dependencies : t -> dependency

  (** [empty name] returns the empty lexicon of name [name] *)
  val empty : (string*Abstract_syntax.location) -> ?non_linear:bool -> abs:signature -> obj:signature -> t

  (** [is_linear l] returns [true] if the lexicon [l] is linear, and [false] otherwise *)
  val is_linear : t -> bool

  (** [name l] returns the name of the lexicon [l] and the location of its definition *)
  val name : t -> (string*Abstract_syntax.location)

  (** [insert e l] returns a lexicon where the interpretation [e] has been added *)        
  val insert : Abstract_syntax.lex_entry -> t -> t

  (** [to_string l] returns a string describing the lexicon [l]. Should be parsable *)
  val to_string : t -> string

  (** [interpret_type t l] returns the interpreted (object) type of the (abstract) type [t] by the lexicon [l] *)
  val interpret_type : Signature.stype -> t -> Signature.stype

  (** [interpret_term t l] returns the interpreted (object) term of the (abstract) term [t] by the lexicon [l] *)
  val interpret_term : Lambda.term -> t -> Lambda.term

  (** [interpret t ty l] returns the interpreted (object) term and type of the (abstract) term [t] and type [ty] by the lexicon [l] *)
  val interpret : Signature.term -> Signature.stype -> t -> (Signature.term*Signature.stype)

  (** [get_sig l] returns a pair [(abs_sig,obj_sig)] consisting of the astract signature [abs_sig] and the object signature [obj_sig] of [l]. *)
  val get_sig : t -> (signature*signature)

  (** [check l] checks whether a lexicon defines all the required interpretations. Raises an exception [Error.Error (Error.Lexicon_error (e,pos))] {!Error.Error} otherwise, where [e] is of type {!Error.lexicon_error}.*)
  val check : t -> unit

  (** [parse t stype lex] tries to parse the (object) term [t] and
      find it an antecedent of type [stype] by [lex]. It returns a resumption that can be used to look for possible other parses *)
  val parse : Signature.term -> Signature.stype -> t -> resume

  (** [get_analysis r l] processe the resumption [r] to check if
  another solution is available. In this case, it returns a pair
  [(Some t,r')] where [t] is another solution, and [r'] a new
  resumption. Otherwise it returns [(None,r')].*)
  val get_analysis : resume -> t -> Lambda.term option * resume
                                                           
  (** [is_empty r] tells whether there is another solution to look in
      the resumption *)
  val is_empty : resume -> bool

  (** [compose l2 l1 (name,loc)] returns a new lexicon which is the
  composition of [l2] and [l1 ] ([l2] after [l1]) such that the
  abstract signature of the resulting lexicon is the same as the one
  of [l1] and its object signature is the same as the one of [l2].*)
  val compose: t -> t -> (string*Abstract_syntax.location) -> t

  (** [program_to_buffer l] returns a buffer containing a parsable version of [l]*)
  val program_to_buffer : t -> Buffer.t

  (** [program_to_log src level l] logs the content of [l] according
  to the source [src] and the level [level]*)
  val program_to_log : Logs.src -> Logs.level -> t -> unit

                                 
  (** [query_to_buffer te ty l] returns a buffer containing a datalog
  query corresponding to the (object) term [te] and the (abstract)
  type [ty] to be parsed with respect to [l].*)
  val query_to_buffer : Signature.term -> Signature.stype -> t -> Buffer.t

  (** [query_to_log src level te ty l] logs the datalog query
  corresponding to the (object) term [te] and the (abstract) type [ty]
  to be parsed with respect to [l] on the [src] source according to
  the [level] level.*)
  val query_to_log : Logs.src -> Logs.level -> Signature.term -> Signature.stype -> t -> unit

  val update : t -> (string -> data) -> t

end
