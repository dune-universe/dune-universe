(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2018 INRIA                             *)
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

(** This modules implements the types for the tokens that are useful
    to the parsers and the lexers *)

open Logic.Abstract_syntax

module Token :
sig

  (** The type of available tokens for parsers and lexers *)
  type t =    
    | SYMBOL of (string*Abstract_syntax.location)
    | IDENT of (string*Abstract_syntax.location)
    | LIN_ARROW of (Abstract_syntax.location)
    | COLON_EQUAL of (Abstract_syntax.location)
    | ARROW of (Abstract_syntax.location)
    | LAMBDA0 of (Abstract_syntax.location)
    | LAMBDA of (Abstract_syntax.location)
    | BINDER of (Abstract_syntax.location)
    | INFIX of (Abstract_syntax.location)
    | PREFIX of (Abstract_syntax.location)
    | TYPE of (Abstract_syntax.location)
    | END_OF_DEC of (Abstract_syntax.location)
    | LEX_OPEN of (Abstract_syntax.location)
    | NL_LEX_OPEN of (Abstract_syntax.location)
    | SIG_OPEN of (Abstract_syntax.location)
    | DOT of (Abstract_syntax.location)
    | RPAREN of (Abstract_syntax.location)
    | LPAREN of (Abstract_syntax.location)
    | COMMA of (Abstract_syntax.location)
    | COLON of (Abstract_syntax.location)
    | SEMICOLON of (Abstract_syntax.location)
    | EQUAL of (Abstract_syntax.location)
    | COMPOSE of (Abstract_syntax.location)
    | EOI
end	

