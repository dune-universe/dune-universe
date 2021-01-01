(**************************************************************************
 *  Copyright (C) 2005-2008
 *  Dmitri Boulytchev (db@tepkom.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

(** Viewing values of various types. *)

(** {2 Combinatorial interface} *)

(** Type of the printer (to be referenced as [View.er]). *)
type er = Buffer.t -> unit

(** Synonym for unqualified reference. *)
type viewer = er

(** String conversion. *)
val toString : viewer -> string

(** {3 Primitive viewers} *)

(** empty viewer *)
val empty : viewer

(** composition (one afer another) *)
val concat : viewer -> viewer -> viewer

(** {3 Viewers for built-in types} *)

(** [unit] viewer. *)
val unit : unit -> viewer

(** [string] viewer. *)
val string : string -> viewer

(** [int] viewer. *)
val int : int -> viewer

(** [float] viewer. *)
val float : float -> viewer

(** [bool] viewer. *)
val bool : bool -> viewer

(** [char] viewer. *)
val char : char -> viewer

(** {3 Some predefined string viewers} *)

(** Semicolon [";"]. *)
val semicolon : viewer

(** Comma [","]. *)
val comma : viewer

(** Space [" "]. *)
val space : viewer

(** Break ["\n"]. *)
val break  : viewer

(** {3 Sequence combinators} *)

(** List viewer. *)
val seq  : viewer list -> viewer

(** Array viewer. *)
val seqa : viewer array -> viewer

(** List by delimiter. *)
val listBy : viewer -> viewer list -> viewer

(** List by comma. *)
val list : viewer list -> viewer

(** Array by delimiter. *)
val arrayBy : viewer -> viewer array -> viewer

(** Array by comma. *)
val array : viewer array -> viewer

(** [inbr l r b] prints [b] in brackets [l], [r]. *)
val inbr : viewer -> viewer -> viewer -> viewer

(** {3 Bracketing combinators} *)

(** [inrbr b] prints [b] in round brackets. *)
val inrbr : viewer -> viewer

(** [insqbr b] prints [b] in square brackets. *)
val insqbr : viewer -> viewer

(** [incvbr b] prints [b] in curved brackets. *)
val incvbr : viewer -> viewer

(** {2 Functorial interface} *)

(** Signature to provide viewing function. *)
module type Viewable = 
  sig

    (** The type. *)
    type t

    (** View function. *)
    val toString : t -> string

  end

(** Signature to supply concatenation function. *)
module type Concat =
  sig
 
    (** Concatenate function. *)
    val concat : string -> string -> string

  end

(** Viewing lists of {!Viewable} types with explicit concatenation function. *)
module ListC (C : Concat) (X : Viewable) : Viewable with type t = X.t list

(** Viewing arrays of {!Viewable} types with explicit concatenation function. *)
module ArrayC (C : Concat) (X : Viewable) : Viewable with type t = X.t array

(** Viewing sets of {!Viewable} types with explicit concatenation function. 
    Set items are ordered in according to their <b>string representations</b>.
 *)
module SetC (C : Concat) (S : Set.S) (V : Viewable with type t = S.elt) : Viewable with 
  type t = S.t

(** Viewing maps of {!Viewable} types with explicit concatenation function. 
    Set items are ordered in according to their <b>string representations</b>.
 *)
module MapC (C : Concat) (M : Map.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with 
  type t = V.t M.t

(** Viewing hash tables of {!Viewable} types with explicit concatenation function. 
    Set items are ordered in according to their <b>string representations</b>. 
 *)
module HashtblC (C : Concat) (M : Hashtbl.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with 
  type t = V.t M.t

(** Viewing lists of {!Viewable} types with concatenation with comma. *)
module List (X : Viewable) : Viewable with type t = X.t list

(** Viewing arrays of {!Viewable} types with concatenation with comma. *)
module Array (X : Viewable) : Viewable with type t = X.t array

(** Viewing sets of {!Viewable} types with concatenation with comma. 
    Set items are ordered in according to their <b>string representations</b>.
 *)
module Set (S : Set.S) (V : Viewable with type t = S.elt) : Viewable with type t = S.t

(** Viewing maps of {!Viewable} types with concatenation with comma. 
    Set items are ordered in according to their <b>string representations</b>. 
 *)
module Map (M : Map.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with 
  type t = V.t M.t

(** Viewing has htables of {!Viewable} types with concatenation with comma. 
     Set items are ordered in according to their <b>string representations</b>. 
 *)
module Hashtbl (M : Hashtbl.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with 
  type t = V.t M.t

(** Viewing named pairs. The first parameter supplies components names. *)
module NamedPair (N : sig val first : string val second : string end) (F : Viewable) (S : Viewable) : Viewable with 
  type t = F.t * S.t

(** Viewing unnamed pairs. *)
module Pair (F : Viewable) (S : Viewable) : Viewable with type t = F.t * S.t

(** {3 Wrappers to make builtin types viewable} *)

module String : Viewable with type t = string
module Integer : Viewable with type t = int
module Float : Viewable with type t = float
module Bool : Viewable with type t = bool
module Char : Viewable with type t = char
module Unit : Viewable with type t = unit
module Exn : Viewable with type t = exn
module Int32 : Viewable with type t = int32
module Int64 : Viewable with type t = int64
module Nativeint : Viewable with type t = nativeint

(** {3 Viewing helpers} *)

(** Concatenation function: [concatWithDelimiter x y delim] 
    returns [x ^ delim ^ y] if x is not empty and [y] otherwise. 
 *)
val concatWithDelimiter : string -> string -> string -> string

(** Concatenation with comma. *)
val concatWithComma : string -> string -> string

(** Concatenation with semicolon. *)
val concatWithSemicolon : string -> string -> string
