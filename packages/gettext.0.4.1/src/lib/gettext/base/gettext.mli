(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(**************************************************************************)

(**
    Modules to use in libraries and programs.
    @author Sylvain Le Gall
*)

(**
    This module defines all the function required to use gettext. The primary
    design is to use applicative function. The "side effect" of such a choice is
    that you must defines, before using any function, all the text domains,
    codeset et al. When building a library, you should give access to
    [Library.init] (by defining a [gettext_init = YouLibrary.init]). This is
    required to enable string translation in the library and programs that uses
    the library. The only function missing here is the [realize] function. This
    function is defined in a real implementation library :
    {ul
      {- {!GettextDummy}}
      {- {!GettextCamomile}}
      {- {!GettextStub}}
    }
  *)

(** {1 Exception} *)

(** Return the string representation of a ocaml-gettext exception. *)
val string_of_exception : exn -> string

(** {1 High level interfaces} *)

(** Value of the dependencies for the initialization of the library
    Gettext (for translating exception and help message).
*)
val init : GettextTypes.dependencies

(** Module to handle typical library requirement *)
module Library (Init : GettextTypes.INIT_TYPE) : sig
  (** Definition of all variables required by ocaml-gettext to use this module
        (includes all the dependencies of the library, as defined in
        {!GettextTypes.Init}).
     *)
  val init : GettextTypes.dependencies

  (** Translate a singular string. *)
  val s_ : string -> string

  (** Translate a [Printf] singular argument.  *)
  val f_ : ('a, 'b, 'c, 'c, 'c, 'd) format6 -> ('a, 'b, 'c, 'c, 'c, 'd) format6

  (** Translate a plural string. *)
  val sn_ : string -> string -> int -> string

  (** Translate a [Printf] plural argument. *)
  val fn_ :
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    int ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6
end

(** Module to handle typical program requirement *)
module Program
    (Init : GettextTypes.INIT_TYPE)
    (Realize : GettextTypes.REALIZE_TYPE) : sig

  (** The first element is a [Arg] argument list. The second element
        contains some information about the gettext library (version,
        build date and author).
      *)
  val init : (Arg.key * Arg.spec * Arg.doc) list * string

  (** See {!Library.s_} *)
  val s_ : string -> string

  (** See {!Library.f_} *)
  val f_ : ('a, 'b, 'c, 'c, 'c, 'd) format6 -> ('a, 'b, 'c, 'c, 'c, 'd) format6

  (** See {!Library.sn_} *)
  val sn_ : string -> string -> int -> string

  (** See {!Library.fn_} *)
  val fn_ :
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    int ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6
end
