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

(** Gettext compatibility with the native gettext API
    @author Sylvain Le Gall
  *)

open GettextTypes
open GettextCategory

val textdomain : textdomain -> t -> t
(** [textdomain domain t] Set the current text domain.
  *)

val get_textdomain : t -> textdomain
(** [get_textdomain t] Returns the current text domain.
  *)

val bindtextdomain : textdomain -> dir -> t -> t
(** [bindtextdomain textdomain dir] Set the default base directory for the
    specified domain.
  *)

val bind_textdomain_codeset : textdomain -> codeset -> t -> t
(** [bind_textdomain_codeset textdomain codeset] Set the codeset to use for the
    specified domain. [codeset] must be a valid codeset for the underlying
    character encoder/decoder (iconv, camomile, extlib...)
  *)

val gettext : t' -> string -> string
(** [gettext t' str] Translate the string [str].
  *)

val fgettext :
  t' -> ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [fgettext t' str] [gettext] returning format.
  *)

val dgettext : t' -> textdomain -> string -> string
(** [dgettext t' textdomain str] Translate the string [str] for the specified
    domain.
  *)

val fdgettext :
  t' ->
  textdomain ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [fdgettext t' textdomain str] [dgettext] returning format.
  *)

val dcgettext : t' -> textdomain -> string -> category -> string
(** [dcgettext t' textdomain str category] Translate the string [str] for the
    specified domain and category.
  *)

val fdcgettext :
  t' ->
  textdomain ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  category ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [fdcgettext t' textdomain str category] [dcgettext] returning format.
  *)

val ngettext : t' -> string -> string -> int -> string
(** [ngettext t' str str_plural n] Translate the string [str] using a plural form.
    str_plural is the default english plural. n is the relevant number for plural
    (i.e. the number of objects deals with the string).
  *)

val fngettext :
  t' ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  int ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [fngettext t' str str_plural n] [ngettext] returning format.
  *)

val dngettext : t' -> textdomain -> string -> string -> int -> string
(** [dngettext t' textdomain str str_plural n] Translate the string [str] using
    a plural form for the specified domain.
  *)

val fdngettext :
  t' ->
  textdomain ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  int ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [fdngettext t' textdomain str str_plural n] [dngettext] returning format.
  *)

val dcngettext :
  t' -> textdomain -> string -> string -> int -> category -> string
(** [dcngettext t' textdomain str str_plural n category] Translate the string
    [str] using a plural form for the specified domain and category.
  *)

val fdcngettext :
  t' ->
  textdomain ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  int ->
  category ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [fdcngettext t' textdomain str str_plural n category] [dcngettext] returning
    format.
  *)
