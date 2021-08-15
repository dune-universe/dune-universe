(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Individual languages and macrolanguages as identified by ISO 639-3.

    ISO 639-3 defines language codes for individual languages and
    macrolanguages.  This module defines an abstract denotaion of these language
    codes.  It provides partial conversions to and from language codes of ISO
    639-1 and ISO 639-2, and one-to-one conversions to and from language codes
    of ISO 639-3.

    ISO 639-1 and 639-2 contains a subset of ISO 639-3 along with a subset of
    ISO 639-5, so the correspondence is partial in both directions. *)

type t
(** A representation of languages present in ISO 639-3. *)

(** {2 Basic Operations} *)

val equal : t -> t -> bool
(** Equality of the corresponding ISO 639-3 language codes. *)

val compare : t -> t -> int
(** Lexicographic order of the corresponding ISO 639-3 language codes. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf lang] prints the ISO 639-3 language code of [lang] on [ppf]. *)

val scope : t -> [> `Individual | `Macro | `Special]
(** [scope lang] is [`Individual] if [lang] is an individual language [`Macro]
    if [lang] is a macrolanguage, or [`Special] if [lang] is not a language. *)

val macrolanguage : t -> t option
(** [macrolanguage lang] is the macrolanguage including [lang], if any. *)

val macrolanguage_members : t -> t list
(** If [lang] is a macrolanguage, then [macrolanguage_members lang] is the list
    of individual languages included in [lang], otherwise the empty list. *)

(** {2 Conversions} *)

val to_int : t -> int
(** An injective mapping to 16 bit integers. *)

val of_int : int -> t option
(** The partial inverse of {!to_int}. *)

val of_int_exn : int -> t
(** The partial inverse of {!to_int}.
    @raise Invalid_argument if the argument is out of range. *)

val to_lang_or_family : t -> Lang_or_family.t
(** Injection into the combined ISO 639 type. *)

val of_lang_or_family : Lang_or_family.t -> t option
(** Restriction from the combined ISO 639 type. *)

(** {2 Language Code Conversions} *)

val to_string : t -> string
(** [to_string lang] is the ISO 639-3 language code of [lang]. *)

val of_string : string -> t option
(** [of_string s] is the language represented by the ISO 639-3 language code
    [s]. *)

val of_string_exn : string -> t
(** [of_string_exn s] is the language represented by the ISO 639-3 language code
    [s]. *)

val is_iso639p1 : t -> bool
(** [is_iso639p1 lang] is true iff [lang] is represented in ISO 639-1. *)

val to_iso639p1 : t -> string option
(** [to_iso639p1 lang] is the ISO 693-1 language code of [lang], if it
    exists. *)

val of_iso639p1 : string -> t option
(** [of_iso639p1 s] is the language represented by the ISO 639-1 language
    code [s]. *)

val is_iso639p2 : t -> bool
(** [is_iso639p2 lang] is true iff [lang] is represented in ISO 639-2. *)

val to_iso639p2t : t -> string option
(** [to_iso639p2t lang] is the ISO 639-2T language code of [lang], if it
    exists. *)

val to_iso639p2b : t -> string option
(** [to_iso639p2b lang] is the ISO 639-2B language code of [lang], if it
    exists. *)

val of_iso639p2 : string -> t option
(** [of_iso639p2 s] is the language represented by the ISO 639 part 2T or 2B
    language code [s]. *)
