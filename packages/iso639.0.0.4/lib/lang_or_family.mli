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

(** Individual and collective languages as a unified type.

    This module defines an abstract denotation of language codes of ISO 639-3
    and ISO 639-5 and provides conversions to and from 2- and 3-letter codes
    defined in parts 1, 2, 3, and 5 of the standard.

    If you only want to represent individual languages, consider using the
    {!Lang} module instead. *)

type t
(** This type represents an individual language or macrolanguage from ISO 639-3
    or a language group from ISO 639-5. *)

(** {2 Basic Operations} *)

val equal : t -> t -> bool
(** [equal lang1 lang2] is true if [lang1] and [lang2] refer to the same
    individual, macro-, or collective language. *)

val compare : t -> t -> int
(** Total order corresponding to lexicographic order of codes within ISO 639-3
    and ISO 639-5.  {e The order of elements belonging to different parts of the
    standard is left unspecified} for now, and may change between versions until
    this notice is removed. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf lang] prints the ISO 639-3 or 639-5 language code on [ppf]. *)

val scope : t -> [> `Individual | `Macro | `Collective | `Special]
(** [scope lang] is [`Individual], [`Macro], or [`Special] if [lang] corresponds
    to an individual language, macro language, or special language code from ISO
    639-3, and [`Collective] if [lang] corresponds to a language group or family
    form ISO 693-5. *)

(* {2 Conversions} *)

val to_int : t -> int
(** An injective mapping to 16 bit integers.  The result can be used for
    serialization, but note that {e the representation may change between
    versions} until this notice is removed.  An alternative for permanent storge
    or operability between versions, is to use {!to_part3} or {!to_part5} or
    both. *)

val of_int : int -> t option
(** [of_int] is the partial inverse of {!to_int}. *)

val of_int_exn : int -> t
(** [of_int_exn] is the partial inverse of {!to_int}.
    @raise Invalid_argument if the argument is out of range. *)

val of_int_unsafe : int -> t
(** An unchecked variant of {!of_int_exn}.  Integers which do not represent ISO
    language codes are mapped to subterranean languages which at runtime tend to
    erupt through the logic of the program in unpredictable ways. *)

(** {2 Language Code Conversions} *)

val of_string : string -> t option
(** [of_string s] is the language or language family represented by the ISO
    639-3 or 639-5 language code [s]. *)

val to_string : t -> string
(** [to_string lang] is the ISO 639-3 language code of [lang] if [lang] is an
    individual or macrolanguage and the 639-5 language code of [lang] if [lang]
    is a language group or family. *)

val is_iso639p1 : t -> bool
(** [is_iso639p1 lang] is true iff [lang] is represented in ISO 639-1. *)

val to_iso639p1 : t -> string option
(** [to_iso639p1 lang] is the two-letter ISO 639-2 code for [lang] if it
    exists. *)

val of_iso639p1 : string -> t option
(** [of_iso639p1 s] is the language represented by the ISO 639-1 language
    code [s]. *)

val is_iso639p2 : t -> bool
(** [is_iso639p2 lang] is true iff [lang] is represented in ISO 639-2. *)

val to_iso639p2t : t -> string option
(** [to_part3t_string lang] is the three-letter ISO 639-2T code for [lang]. *)

val to_iso639p2b : t -> string option
(** [to_part3b_string lang] is the three-letter ISO 639-2B code for [lang]. *)

val of_iso639p2 : string -> t option
(** [of_iso639p2 s] is the language represented by the ISO 639 part 2T or 2B
    language code [s]. *)

val is_iso639p3 : t -> bool
(** [is_iso639p3 lang] is true iff [lang] is represented in ISO 639-3, i.e. an
    individual language or macro language. *)

val to_iso639p3 : t -> string option
(** [to_iso639p3 lang] is the three-letter ISO 639-3 code for [lang].  The
    result coincides with the ISO 639-2 code if it exist, and may clash with an
    ISO 639-5 code for a language group. *)

val of_iso639p3 : string -> t option
(** [of_string s] is the language represented by the ISO 639-3 language code
    [s]. *)

val is_iso639p5 : t -> bool
(** [is_iso639p3 lang] is true iff [lang] is represented in ISO 693-5, i.e. a
    language family or group. *)

val to_iso639p5 : t -> string option
(** [to_iso639p5 lang] is the three-letter ISO 639-5 code for [lang].  The
    result coincides with the ISO 639-2 code if it exist, and may clash with an
    ISO 639-3 code for an individual language or macrolanguage.  *)

val of_iso639p5 : string -> t option
(** [of_string s] is the language family or group represented by the ISO 639-5
    code [s]. *)
