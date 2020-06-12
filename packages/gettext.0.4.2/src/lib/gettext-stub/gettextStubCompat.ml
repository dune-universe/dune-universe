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

(** Low level interface to gettext C library.
    @author Sylvain Le Gall
  *)

type lc =
  | LC_CTYPE
  | LC_NUMERIC
  | LC_TIME
  | LC_COLLATE
  | LC_MONETARY
  | LC_MESSAGES
  | LC_ALL

external setlocale : lc -> string -> string option
  = "gettextStubCompat_setlocale"
(** Set the current localization for the category
  *)

external gettext : string -> string = "gettextStubCompat_gettext"
(** Look up MSGID in the current default message catalog for the current
  * LC_MESSAGES locale.  If not found, returns MSGID itself (the default text).
  *)

external dgettext : string -> string -> string = "gettextStubCompat_dgettext"
(** Look up MSGID in the DOMAINNAME message catalog for the current LC_MESSAGES
  * locale.
  *)

external dcgettext : string -> string -> lc -> string
  = "gettextStubCompat_dcgettext"
(** Look up MSGID in the DOMAINNAME message catalog for the current CATEGORY
  * locale.
  *)

external ngettext : string -> string -> int -> string
  = "gettextStubCompat_ngettext"
(** Similar to `gettext' but select the plural form corresponding to the number
  * N.
  *)

external dngettext : string -> string -> string -> int -> string
  = "gettextStubCompat_dngettext"
(** Similar to `dgettext' but select the plural form corresponding to the number
  * N.
  *)

external dcngettext : string -> string -> string -> int -> lc -> string
  = "gettextStubCompat_dcngettext"
(** Similar to `dcgettext' but select the plural form corresponding to the
  * number N.
  *)

external textdomain : string -> string option = "gettextStubCompat_textdomain"
(** Set the current default message catalog to DOMAINNAME.If DOMAINNAME is "",
  * reset to the default of "messages".
  *)

external get_textdomain : unit -> string option
  = "gettextStubCompat_get_textdomain"
(** Get the current default message catalog to DOMAINNAME.
  *)

external bindtextdomain : string -> string -> string option
  = "gettextStubCompat_bindtextdomain"
(** Specify that the DOMAINNAME message catalog will be foundin DIRNAME rather
  * than in the system locale data base.
  *)

external bind_textdomain_codeset : string -> string -> string option
  = "gettextStubCompat_bind_textdomain_codeset"
(** Specify the character encoding in which the messages from theDOMAINNAME
  * message catalog will be returned.
  *)
