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

open AcgData
open Interface

open Logic
open Abstract_syntax

(** This modules implements a functor that build an environment
    containing signatures and lexicons when provided with to actual
    implementations of a signature and a lexicon *)


(** This module signature describes the interface for modules implementing environments *)
module type Environment_sig =
sig

  (** This exception can be raised when a signature or an entry is not
      found in the environmnent *)
  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string

  (** The modules implementing the signatures and the lexicons managed
      by the environment *)
  module Signature1:Signature_sig with type term=Lambda.Lambda.term
  module Lexicon:Interface.Lexicon_sig with type Signature.t=Signature1.t and type Signature.term=Signature1.term and type Signature.stype=Signature1.stype

  (** The type of the environment *)
  type t

  (** The type of what an environment can contain *)
  type entry = 
    | Signature of Signature1.t
    | Lexicon of Lexicon.t

  (** [empty] is the empty environmnent *)
  val empty : t

  (** [insert c d e] adds the content [c] into the environment [e] and
      returns the resulting environmnent. If [d] is set to true, then
      [c] is dumped in [e] by the
      {!val:Environment.Environment_sig.write} function. *)
  val insert : ?override:bool -> entry -> to_be_dumped:bool -> t -> t

  (** [get_signature name e] returns the signature of name [name] in
      the environment [e]. Raise
      {!Environment.Environment_sig.Signature_not_found} if such a
      signature does not exist *)
  val get_signature : string -> t -> Signature1.t

  (** [get_lexicon name e] returns the signature of name [name] in
      the environment [e]. Raise
      {!Environment.Environment_sig.Lexicon_not_found} if such a
      signature does not exist *)
  val get_lexicon : string -> t -> Lexicon.t

  (** [get name e] returns the entry of name [name] in the environment
      [e]. Raise {!Environment.Environment_sig.Lexicon_not_found} if
      such an entry does not exist. *)
  val get : string -> t -> entry

  (** [append e1 e2] merges the two environment [e1] and [e2]. If an
      entry appears in both environment then the one of [e2] is kept
      if the [override] parameter is set to [true] (default is
      [false]). If set to [false], if an entry appears in both
      environment, an error is emitted. *)
  val append : ?override:bool -> t -> t -> t

  (** [iter f e] applies f to every data contained in the environment
  *)
  val iter : (entry -> unit) -> t -> unit

  (** [fold f a e] returns [f a_n (f a_n-1 (... (f a1 (f a0 a))
      ... ))] where the [a_0 ... a_n] are the [n+1] elements of the
      environmnent *)
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a

  (** [sig_number e] returns the number of signatures an environment
      contains *)
  val sig_number : t -> int

  (** [sig_number e] returns the number of lexicons an environment
      contains *)
  val lex_number : t -> int

  (** [choose_signature e] returns a randomly chosen signature in the
      environment [e] *)
  val choose_signature : t -> Signature1.t option

  (** [compatible_version e] returns [true] if the environment [e]
      was created with the current versions of the compilers and/or
      interpreter *)
  val compatible_version : t -> bool

  (** [read filename dirs] returns [Some e] if the file [filename]
      contains the output value of an enviromnent compatible with the
      current version of the software. It returns [None]
      otherwise. The file [filename] is looked up in the list of
      directories [dirs]. *)
  val read : string -> string list -> t option

  (** [write filename e] write the output value of the enviromnent [e]
      into the file [filename] *)
  val write : string -> t -> unit


    
  val select : string -> t -> t

  val unselect : t -> t

  val focus : t -> entry option
end


(** The functor that builds the environment *)

(*module Make  (Lex:Interface.Lexicon_sig) : Environment_sig 
  with
    type Signature1.t=Lex.Signature.t
  and type Lexicon.t = Lex.t
  and type Signature1.term = Lex.Signature.term
  and type Signature1.stype = Lex.Signature.stype
*)

module Environment :  Environment_sig 
  with
    type Signature1.t=Acg_lexicon.Data_Lexicon.Signature.t
  and type Lexicon.t = Acg_lexicon.Data_Lexicon.t
  and type Signature1.term = Acg_lexicon.Data_Lexicon.Signature.term
  and type Signature1.stype = Acg_lexicon.Data_Lexicon.Signature.stype
