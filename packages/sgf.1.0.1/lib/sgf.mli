(*
 * Copyright (c) 2012-2015 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Rresult

(** {2 Types} *)

type pvalue =
  | Empty
  | Number of int
  | Real of float
  | Normal
  | Emph
  | Black
  | White
  | Text of string
  | Point of char * char
  | Move of (char * char) option
  | Compose of pvalue * pvalue

type pvalues = One of pvalue | List of pvalue list
type property = string * pvalues
type node = property list
type sequence = node list
type gametree = Node of sequence * gametree list | Leaf of sequence
type collection = gametree list

type err =
  | Lexing_error of Lexing.position * string
  | Parsing_error of Lexing.position

(** {2 Parsing} *)

val of_string : string -> (collection, err) result
val of_channel : in_channel -> (collection, err) result
val of_file : string -> (collection, err) result

(** {2 Printing} *)

val pp_property : Format.formatter -> property -> unit
val pp_node : Format.formatter -> node -> unit
val pp_sequence : Format.formatter -> sequence -> unit
val pp_gametree : Format.formatter -> gametree -> unit
val pp_collection : Format.formatter -> collection -> unit
