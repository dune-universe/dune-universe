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

module type SIG_ACCESS =
sig
  exception Not_found
  type t

  val expand_type :  Lambda.stype -> t -> Lambda.stype 
  val find_term : string -> t -> Lambda.term *Lambda.stype
  val type_to_string : Lambda.stype -> t -> string
  val term_to_string : Lambda.term -> t -> string
(*  val id_to_string : t -> int -> Abstract_syntax.syntactic_behavior*string*)
end

module Type_System :
sig
  module Make(Signature:SIG_ACCESS) : 
  sig
    val typecheck : Abstract_syntax.term -> Lambda.stype -> Signature.t -> Lambda.term
  end
end

      
