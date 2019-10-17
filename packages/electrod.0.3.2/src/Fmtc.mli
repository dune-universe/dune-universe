(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2019 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

include module type of Fmt

val nbsp : unit t

val hardline : Format.formatter -> unit -> unit

val lbrace : unit t

val rbrace : unit t

val lparen : unit t

val rparen : unit t

val langle : unit t

val rangle : unit t

val lbracket : unit t

val rbracket : unit t

val squote : unit t

val dquote : unit t

val bquote : unit t

val semi : unit t

val colon : unit t

val comma : unit t

val dot : unit t

val sharp : unit t

val slash : unit t

val backslash : unit t

val equal : unit t

val qmark : unit t

val tilde : unit t

val at : unit t

val percent : unit t

val dollar : unit t

val caret : unit t

val ampersand : unit t

val star : unit t

val plus : unit t

val minus : unit t

val underscore : unit t

val bang : unit t

val bar : unit t

val squotes : 'a t -> Format.formatter -> 'a -> unit

val bquotes : 'a t -> Format.formatter -> 'a -> unit

val angles : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val ( **@ ) : 'a t -> 'a t -> 'a t

val ( **< ) : unit t -> 'a t -> 'a t

val ( **> ) : unit t -> 'a t -> 'a t

val ( **! ) : 'a t -> 'b t -> ('a * 'b) t

val ( **- ) : 'a t -> 'b t -> ('a * 'b) t

val surround :
     ('a -> unit -> unit)
  -> ('a -> unit -> 'b)
  -> ('a -> 'c -> unit)
  -> 'a
  -> 'c
  -> 'b

val surround_ :
     (Format.formatter -> unit -> unit)
  -> (Format.formatter -> unit -> 'a)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> 'b
  -> 'a

val braces_ :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val brackets_ :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val parens_ :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val angles_ :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val unless :
     ('a -> bool)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit

val repeat : int -> unit t -> 'a t

val infix :
     ?indent:int
  -> ?par:bool
  -> (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> (Format.formatter -> 'c -> unit)
  -> Format.formatter
  -> 'a * 'b * 'c
  -> unit

val prefix :
     ?indent:int
  -> ?par:bool
  -> (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> 'a * 'b
  -> unit

val tuple2 : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t

val tuple3 :
     ?sep1:unit t
  -> ?sep2:unit t
  -> (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'c -> unit)
  -> (Format.formatter -> 'e -> 'f)
  -> Format.formatter
  -> 'a * 'c * 'e
  -> 'f

val triple :
     ?sep1:unit t
  -> ?sep2:unit t
  -> (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'c -> unit)
  -> (Format.formatter -> 'e -> 'f)
  -> Format.formatter
  -> 'a * 'c * 'e
  -> 'f

val bbox :
     ?indent:int
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit

val bbox2 : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val box2 : 'a t -> Format.formatter -> 'a -> unit

val hbox2 : 'a t -> Format.formatter -> 'a -> unit

val vbox2 : 'a t -> Format.formatter -> 'a -> unit

val hvbox2 : 'a t -> Format.formatter -> 'a -> unit
