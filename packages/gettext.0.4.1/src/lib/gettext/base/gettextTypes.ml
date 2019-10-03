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

(** Types and exception of ocaml-gettext.
    @author Sylvain Le Gall
  *)

open GettextCategory

(** {1 Core types of ocaml-gettext library} *)

type range = Int32.t * Int32.t

type textdomain = string

type locale = string

type dir = string

type filename = string

type codeset = string

(** {1 Exceptions} *)

exception CompileProblemReadingFile of filename * string
(** Filename wich generates the error message str
  *)

exception CompileExtractionFailed of filename * string * int
(** While extracting filename the command str returns exit code i.
  *)

exception CompileExtractionInterrupted of filename * string * int
(** While extracting filename the command receive signal i.
  *)

exception DomainFileDoesntExist of filename list
(** Cannot the filename corresponding to a textdomain among the specified files.
  *)

exception FormatInconsistent of string * string
(** The two strings returned doesn't have the same meaning regarding [Printf]
    syntax.
  *)

exception GettextUninitialized
(** A part of the code try to translate a string, but ocaml-gettext is not
    initialized.
  *)

exception MoInvalidOptions of Lexing.lexbuf * string
(** There is an invalid field in the content information of a MO file.
  *)

exception MoInvalidPlurals of Lexing.lexbuf * string
(** The plural-form field is not correct.
  *)

exception MoInvalidContentType of Lexing.lexbuf * string
(** The content-type field is not correct.
  *)

exception MoInvalidTranslationSingular of string * int
(** A plural translation of a singular string has occured.
  *)

exception MoInvalidTranslationPlural of string list * int
(** An out-of-bound plural translation has occured.
  *)

exception MoJunk of string * string list
(** There is more plural translation than the number of plural forms.
  *)

exception MoEmptyEntry
(**
  *)

exception MoInvalidFile
(** A MO corrupted file has been read.
  *)

exception MoInvalidHeaderNegativeStrings
(** The MO file specified a negative number of strings.
  *)

exception MoInvalidHeaderTableStringOutOfBound of range * range
(** Offset of the string table is out of bound.
  *)

exception MoInvalidHeaderTableTranslationOutOfBound of range * range
(** Offset of the translation table is out of bound.
  *)

exception MoInvalidHeaderTableTranslationStringOverlap of range * range
(** String and translation table overlap.
  *)

exception MoInvalidStringOutOfBound of int * int
(** The offset and length of a string entry leads to an access beyond the end
    of the MO file.
  *)

exception MoInvalidTranslationOutOfBound of int * int
(** The offset and length of a translation entry leads to an access beyond the end
    of the MO file.
  *)

exception MoCannotOpenFile of string
(** An error occured when trying to open a MO file.
  *)

exception PoInvalidFile of string * Lexing.lexbuf * in_channel
(** A PO file cannot be parsed.
  *)

exception PoFileInvalidIndex of string * int
(** When parsing a PO file, found an out of order table indices in a plural
    form.
  *)

exception PoFileDoesntExist of string
(** The PO file doesn't exist.
  *)

exception PoInconsistentMerge of string * string
(** Cannot merge two PO files.
  *)

exception TranslateStringNotFound of string
(** A string to translate cannot be found.
  *)

exception LocalePosixUnparseable of string
(** Cannot parse the POSIX representation of the locale.
  *)

(** {1 Modules signatures} *)

type dependencies = (textdomain * codeset option * dir option) list

module type INIT_TYPE = sig
  val textdomain : textdomain

  val codeset : codeset option

  val dir : dir option

  val dependencies : dependencies
end

(* We stop documentation here, for the gettext API reference, all those types
   are internals : use at your own risk.
 *)
(**/**)

(** {1 Extended core types} *)

module MapString = Map.Make (String)
module SetString = Set.Make (String)

module MapTextdomain = Map.Make (struct
  type t = textdomain

  let compare = String.compare
end)

(** Defines behavior regarding exception in the ocaml-gettext library
  *)
type failsafe = Ignore | InformStderr of (exn -> string) | RaiseException

type t = {
  failsafe : failsafe;
  textdomains : (codeset option * dir option) MapTextdomain.t;
  categories : locale MapCategory.t;
  language : locale option;
  codeset : codeset;
  path : dir list;
  default : textdomain;
}
(** Data structure handling initialization variable of ocaml-gettext
  *)

type t' =
  bool ->
  textdomain option ->
  string ->
  (string * int) option ->
  category ->
  string
(** Function to translate effectively a string
  *)

(** {1 Types for MO file processing} *)

(** Endianess of a MO file
  *)
type endianess = BigEndian | LittleEndian

type mo_header = {
  endianess : endianess;
  file_format_revision : int32;
  number_of_strings : int32;
  offset_table_strings : int32;
  offset_table_translation : int32;
  size_of_hashing_table : int32;
  offset_of_hashing_table : int32;
}
(** Specification of .MO file

    @see <http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC136> GNU Gettext documentation.

   Format of MO file :

        byte
             +------------------------------------------+
          0  | magic number = 0x950412de                |
             |                                          |
          4  | file format revision = 0                 |
             |                                          |
          8  | number of strings                        |  == N
             |                                          |
         12  | offset of table with original strings    |  == O
             |                                          |
         16  | offset of table with translation strings |  == T
             |                                          |
         20  | size of hashing table                    |  == S
             |                                          |
         24  | offset of hashing table                  |  == H
             |                                          |
             .                                          .
             .    (possibly more entries later)         .
             .                                          .
             |                                          |
          O  | length & offset 0th string  ----------------.
      O + 8  | length & offset 1st string  ------------------.
              ...                                    ...   | |
O + ((N-1)*8)| length & offset (N-1)th string           |  | |
             |                                          |  | |
          T  | length & offset 0th translation  ---------------.
      T + 8  | length & offset 1st translation  -----------------.
              ...                                    ...   | | | |
T + ((N-1)*8)| length & offset (N-1)th translation      |  | | | |
             |                                          |  | | | |
          H  | start hash table                         |  | | | |
              ...                                    ...   | | | |
  H + S * 4  | end hash table                           |  | | | |
             |                                          |  | | | |
             | NUL terminated 0th string  <----------------' | | |
             |                                          |    | | |
             | NUL terminated 1st string  <------------------' | |
             |                                          |      | |
              ...                                    ...       | |
             |                                          |      | |
             | NUL terminated 0th translation  <---------------' |
             |                                          |        |
             | NUL terminated 1st translation  <-----------------'
             |                                          |
              ...                                    ...
             |                                          |
             +------------------------------------------+

*)

type mo_translation = {
  project_id_version : string option;
  report_msgid_bugs_to : string option;
  pot_creation_date : string option;
  po_revision_date : string option;
  last_translator : string option;
  language_tream : string option;
  mime_version : string option;
  content_type : string option;
  content_transfer_encoding : string option;
  plural_forms : string option;
  (* The only interesting fields *)
  (* Those field are precomputed for regular use *)
  content_type_charset : string;
  nplurals : int;
  fun_plural_forms : int -> int;
}
(** Details associated with ""
    Project-Id-Version: PACKAGE VERSION\n
    Report-Msgid-Bugs-To: \n
    POT-Creation-Date: 2004-05-31 16:53+0200\n
    PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n
    Last-Translator: FULL NAME <EMAIL@ADDRESS>\n
    Language-Team: LANGUAGE <LL@li.org>\n
    MIME-Version: 1.0\n
    Content-Type: text/plain; charset=CHARSET\n
    Content-Transfer-Encoding: 8bit\n
    Plural-Forms: specific ( 0 is false and 1 is
    true
  *)

(** Base type of MO content : translation of string. The first string members are
    the string identifier ( singular form ).
*)
type translation =
  | Singular of string * string
  | Plural of string * string * string list

(** Types for the PO processing. The main difference with the type translation
    comes from the necessity of keeping a maximum of comment.
*)
type po_translation =
  | PoSingular of string list * string list
  | PoPlural of string list * string list * string list list

type po_filepos = filename * int
(** PO string localizator : represents in which file/lineno a string can be
    found.
  *)

type po_special = string
(** PO keyword: represents special keyword like fuzzy, wrap, c-format...
  *)

type po_commented_translation = {
  po_comment_special : po_special list;
  po_comment_filepos : po_filepos list;
  po_comment_translation : po_translation;
}

type po_translations = po_commented_translation MapString.t
(** Mapping of PO content using the string identifier as the key.
*)

type po_content = {
  no_domain : po_translations;
  domain : po_translations MapTextdomain.t;
}
(** Content of a PO file. Since comments should be saved, and that we only save
    comments before and in message translation, we need to keep trace of the
    last comments, which is not attached to any translation.
*)

(** {1 Modules signatures} *)

(** Signature for module handling transformation of initialization parameters
    to concrete translation function.
  *)
module type REALIZE_TYPE = sig
  val realize : t -> t'
end
