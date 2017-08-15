(* The MIT License (MIT)
   Copyright (c) 2016 Leonardo Laguna Ruiz

   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
   associated documentation files (the "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or substantial
   portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
   LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(** Main module *)

module PlaBuffer :                                                                                                                                                                                 sig                                                                                                                                                                                                type dest = File of out_channel | Buffer of Buffer.t                                                                                                                                         
   type t
   val appendToBuff : dest -> string -> unit
   val newBuffer : unit -> t
   val newFile : string -> t
   val contents : t -> string
   val close : t -> unit
   val newline : t -> unit
   val indent : t -> unit
   val outdent : t -> unit
   val append : t -> string -> unit
end

type t = PlaBuffer.t -> unit
(** Main template type *)

(** {6 Builtin templates} *)

val unit : t
(** Empty template *)

val newline : t
(** Template for a new line *)

val comma : t
(** Template for a comma ',' *)

val commaspace : t
(** Template for a comma followed by a space ', ' *)

val semi : t
(** Template for a semicolon ';' *)

val space : t
(** Template for a white space ' ' *)


(** {6 Templates of basic types} *)

val string : string -> t
(** [string str] makes a template from a string [str] *)

val int : int -> t
(** [int i] makes a template from an integer value [i] *)

val float : float -> t
(** [float f] makes a template from a float value *)

val string_quoted : string -> t
(** [string_quoted str] makes a template from a string [str] but the contents are quoted *)


(** {6 Functions to wrap templates} *)

val wrap : t -> t -> t -> t
(** [wrap left right t] makes new template wrapped by the [left] and [right] templates *)

val quote : t -> t
(** [quote t] makes a new template wrapped with double quotes *)

val parenthesize : t -> t
(** [parenthesize t] makes a new template wrapped by parenthesis *)

val indent : t -> t
(** [indent t] makes an indented block with the contents of the template [t] *)


(** {6 Functions to append templates} *)

val append : t -> t -> t
(** [append t1 t2] makes a new template with the contents of [t1] followed by the contents of [t2] *)

val (++) : t -> t -> t
(** [t1 ++ t2] equivalent to [append t1 t2] *)

val join : t list -> t
(** [join elems] makes a new template by appending the list [elems] of templates *)

val join_sep : t -> t list -> t
(** [join sep elems] makes a new template by appending the list [elems] of templates separated by [sep] *)

val join_sep_all : t -> t list -> t
(** [join_sep_all sep elems] similar to [join_sep sep elems] but also adds the separator after the last element *)

val map_join : ('a -> t) -> 'a list -> t
(** [map_join f elems] makes a new template by applying the function [f] to the list [elems] and appending them *)

val map_sep : t -> ('a -> t) -> 'a list -> t
(** [map_sep sep f elems] makes a new template by applying the function [f] to the list [elems] and appending them separated by the template [sep] *)

val map_sep_all : t -> ('a -> t) -> 'a list -> t
(** [map_sep_all sep f elems] similar to [map_sep sep f elems] but also adds the separator after the last element *)


(** {6 Printing of templates} *)

val print : t -> string
(** [print t] returns the contents template [t] as a string *)

val write : string -> t -> unit
(** [write file t] writes the contents of template [t] to file [file] *)
