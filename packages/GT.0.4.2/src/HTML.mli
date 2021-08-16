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

(** Viewing values of various types in HTML format. *)

(** {2 Combinatorial interface} *)

(** Type synonym for viewer function to be referenced as [HTMLView.er]. *)
type er = View.er

(** Type synonym to be referenced unqualified. *)
type viewer = er

(** String conversion. *)
val toHTML : viewer -> string

(** Escapes special HTML symbols ("<", ">", "&", """"). *)
val escape : string -> string

(** Escaped string. *)
val string : string -> viewer

(** Raw string. *)
val raw : string -> viewer

(** {3 Viewer constructors for build-in types} *)

(** [unit] viewer. *)
val unit : unit -> viewer

(** [int] viewer. *)
val int : int -> viewer

(** [float] viewer. *)
val float : float -> viewer

(** [bool] viewer. *)
val bool : bool -> viewer

(** [char] viewer. *)
val char : char -> viewer

(** {3 Sequence constructors} *)

(** List viewer. *)
val seq : viewer list -> viewer

(** Array viewer. *)
val seqa : viewer array -> viewer

(** {3 Some predefined HTML-specific viewers} *)

(** [anchor ref p] outputs [p] within the anchor [ref]. *)
val anchor : string -> viewer -> viewer

(** [ref ref p] outputs [p] as hyper-reference to [ref]. *)
val ref : string -> viewer -> viewer

(** [named name p] outputs [p] as named by [name] item. *)
val named : string -> viewer -> viewer

(** Outputs unordered list. *)
val list : viewer list -> viewer

(** Outputs unordered list. *)
val array : viewer array -> viewer

(** Outputs a list of named elements. *)
val fields : (string * viewer) list -> viewer

(** Break viewer. *)
val br : viewer

(** Tagged viewer: [tag name p] surrounds [p] with open and close tags 
    with name [name]. Optional argument [attrs] can be given to provide
    attributes for the tag (for example, [tag "table" ~attrs:"align=center" p]).
 *)
val tag : ?attrs:string -> string -> viewer -> viewer

(** {3 Some conventional HTML tags. Optional argument [attrs] provides HTML tag attributes} *)

val html  : ?attrs:string -> viewer -> viewer
val title : ?attrs:string -> viewer -> viewer
val body  : ?attrs:string -> viewer -> viewer
val ul    : ?attrs:string -> viewer -> viewer
val ol    : ?attrs:string -> viewer -> viewer
val li    : ?attrs:string -> viewer -> viewer
val b     : ?attrs:string -> viewer -> viewer
val i     : ?attrs:string -> viewer -> viewer
val table : ?attrs:string -> viewer -> viewer
val tr    : ?attrs:string -> viewer -> viewer
val td    : ?attrs:string -> viewer -> viewer
val th    : ?attrs:string -> viewer -> viewer
val form  : ?attrs:string -> viewer -> viewer
val input : ?attrs:string -> viewer -> viewer

(** {3 Some conventional HTML inputs. Optional argument [attrs] provides HTML tag attributes} *)

val checkbox : ?attrs:string -> viewer -> viewer 
val button   : ?attrs:string -> viewer -> viewer 
val text     : ?attrs:string -> viewer -> viewer 
val textarea : ?attrs:string -> viewer -> viewer 
val div      : ?attrs:string -> viewer -> viewer 

val radio    : ?attrs:string -> (viewer * string * string) list -> viewer 
val select   : ?attrs:string -> (viewer * string * string) list -> viewer

val link     : string -> viewer
(** {2 Multi-page wizard generator} *)

module Wizard :
  sig

    (* {3 Type of page: each method adds a corresponding input control (in order of invocation)} *)
    type page = < string : ?attrs:string -> string -> page;
                  text   : ?attrs:string -> ?default:string -> string -> page;
                  div    : ?attrs:string -> ?default:string -> string -> page;
                  flag   : ?attrs:string -> string -> page;
                  combo  : ?attrs:string -> string -> (viewer * string * string) list -> page;
                  radio  : ?attrs:string -> string -> (viewer * string * string) list -> page;
                  id     : string -> string;
                >

    (* {3 Wizard type: first add pages, then generate. Returns a pair: top-level function name to call, and javascript 
          wizard code to embed}
    *)
    type t = < page : (page -> page) list -> page; generate : string * string >

    val string : ?attrs:string -> string -> page -> page
    val text   : ?attrs:string -> ?default:string -> string -> page -> page
    val div    : ?attrs:string -> ?default:string -> string -> page -> page
    val flag   : ?attrs:string -> string -> page -> page
    val combo  : ?attrs:string -> string -> (viewer * string * string) list -> page -> page
    val radio  : ?attrs:string -> string -> (viewer * string * string) list -> page -> page

    (* {3 [create id target navigate] an empty wizard. "id" - some id, which distinguish this wizard from others,
        "target" - a DOM element to embed the wizard to, "navigate" - the name of navigation function. Navigation 
         function takes two arguments: a number of page and context object, which collects the information from
         the wizard. This object binds input names, provided when these inputs were added to the page, to their
         values. Navigation function returns -1, if the values, provided by the user, are incorrect, and the number
         of the next page to show otherwize. For the last page it can invoke some action the wizard was designed to
         provide a configuration for, and return the same number of page} *)
    val create : ?attrs:string -> string -> string -> string -> t

  end

(** {2 Helper module to provide anchors to values} *)

(** Functor anchor takes one argument which describes type 
    for which values anchors are set and name to distinguish
    namespaces (if any).
 *)
module Anchor (X : sig type t val name : string end) :
  sig

    (** Set anchor for value. *)
    val set : X.t -> unit

    (** Get anchor value. *)
    val get : X.t -> string

    (** Get url for the value. *)
    val url : X.t -> string

    (** Make reference to the value. *)
    val ref : X.t -> viewer -> viewer

    (** Make reference for strings. *)
    module String :
      sig

	val ref : X.t -> string -> string

      end

  end

(** {2 Functorial interface} *)

(** An abstract element to generate HTML from. *)
module type Element =
  sig 

    (** The type. *)
    type t 

    (** Generate HTML representation. *)    
    val toHTML : t -> string 

  end

(** Functor to provide list to HTML generation. *)
module List (T : Element) : Element with type t = T.t list

(** Functor to provide array to HTML generation. *)
module Array (T : Element) : Element with type t = T.t array

(** Functor to provide set to HTML generation. 
    Set items are ordered in according to their <b>string representations</b>. 
 *)
module Set (S : Set.S) (V : Element with type t = S.elt) : Element with type t = S.t

(** Functor to provide map to HTML generation. 
    Set items are ordered in according to their <b>string representations</b>. 
 *)
module Map (M : Map.S) (K : Element with type t = M.key) (V : Element) : Element with type t = V.t M.t

(** Functor to provide hash table to HTML generation. 
    Set items are ordered in according to their <b>string representations</b>. 
 *)
module Hashtbl (M : Hashtbl.S) (K : Element with type t = M.key) (V : Element) : Element with type t = V.t M.t

(** Functor to provide named pair to HTML generation. The first parameter sets component names. *)
module NamedPair (N : sig val first : string val second : string end) (F : Element) (S : Element) : Element with 
  type t = F.t * S.t

(** Functor to provide unnamed pair to HTML generation. *)
module Pair (F : Element) (S : Element) : Element with type t = F.t * S.t

(** Module to provide raw string HTML generation. *)
module Raw : Element with type t = string

(** Module to provide string to HTML generation. *)
module String :
  sig

    (** Type synonym. *)
    type t = string

    (** HTML viewer. *)
    val toHTML : string -> string

    (** Synonym for [named] for string values. *)
    val named  : string -> string -> string

    (** Synonym for [fields] for string values. *)
    val fields : (string * string) list -> string

    (** Synonym for [anchor] for string values. *)
    val anchor : string -> string -> string

    (** Synonym for [ref] for string values. *)
    val ref : string -> string -> string

  end
