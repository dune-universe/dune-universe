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

open Logic.Lambda

(*
type sig_entry =
  | Type_declaration of string * int * Lambda.kind
  | Type_definition of string * int * Lambda.kind * Lambda.stype
  | Term_declaration of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype
  | Term_definition of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype * Lambda.term
  
*)    

module Data_Signature  : Interface.Signature_sig 
  with
    type term = Lambda.term
  and type stype = Lambda.stype 
(*  and type entry = Interfacesig_entry *)
