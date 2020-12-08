(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module OpamParserTypes : sig

  (** Defines the types for the opam format lexer and parser *)

  (** Type definitions used by the legacy and the new full position modules *)

  (** Relational operators *)
  type relop = [ `Eq  (** [=] *)
               | `Neq (** [!=] *)
               | `Geq (** [>=] *)
               | `Gt  (** [>] *)
               | `Leq (** [<=] *)
               | `Lt  (** [<] *)
               ]

  (** Logical operators *)
  type logop = [ `And (** [&] *) | `Or (** [|] *) ]

  (** Prefix operators *)
  type pfxop = [ `Not (** [!] *) | `Defined (** [?] *) ]

  type file_name = string

  (** Source file positions: [(filename, line, column)] *)
  type pos = file_name * int * int

  (** Environment variable update operators *)
  type env_update_op = Eq       (** [=] *)
                     | PlusEq   (** [+=] *)
                     | EqPlus   (** [=+] *)
                     | ColonEq  (** [:=] *)
                     | EqColon  (** [=:] *)
                     | EqPlusEq (** [=+=] *)

  (** [OpamParserTypes] transitional module with full position types *)
  module FullPos : sig

    (** Source file positions *)
    type file_name = string

    (** Full position *)
    type pos = {
      filename: file_name;
      start: int * int; (* line, column *)
      stop: int * int;  (* line, column *)
    }

    (** [with_pos] type, used for all units, embedding the element [pelem] ans
        its position [pos] *)
    type 'a with_pos = {
      pelem : 'a;
      pos : pos
    }

    type relop_kind = relop
    type relop = relop_kind with_pos

    type logop_kind = logop
    type logop = logop_kind with_pos

    type pfxop_kind = pfxop
    type pfxop = pfxop_kind with_pos

    type env_update_op_kind = env_update_op
    type env_update_op = env_update_op_kind with_pos

    (** Base values *)
    type value_kind =
      | Bool of bool
      (** [bool] atoms *)
      | Int of int
      (** [int] atoms *)
      | String of string
      (** [string] atoms *)
      | Relop of relop * value * value
      (** Relational operators with two values (e.g. [os != "win32"]) *)
      | Prefix_relop of relop * value
      (** Relational operators in prefix position (e.g. [< "4.07.0"]) *)
      | Logop of logop * value * value
      (** Logical operators *)
      | Pfxop of pfxop * value
      (** Prefix operators *)
      | Ident of string
      (** Identifiers *)
      | List of value list with_pos
      (** Lists of values ([[x1 x2 ... x3]]) *)
      | Group of value list with_pos
      (** Groups of values ([(x1 x2 ... x3)]) *)
      | Option of value * value list with_pos
      (** Value with optional list ([x1 {x2 x3 x4}]) *)
      | Env_binding of value * env_update_op * value
      (** Environment variable binding ([FOO += "bar"]) *)
    and value = value_kind with_pos

    (** An opamfile section *)
    type opamfile_section =
      { section_kind  : string with_pos;             (** Section kind
                                                         (e.g. [extra-source]) *)
        section_name  : string with_pos option;      (** Section name
                                                         (e.g. ["myfork.patch"]) *)
        section_items : opamfile_item list with_pos; (** Content of the section *)
      }

    (** An opamfile is composed of sections and variable definitions *)
    and opamfile_item_kind =
      | Section of opamfile_section         (** e.g. [kind ["name"] { ... }] *)
      | Variable of string with_pos * value (** e.g. [opam-version: "2.0"] *)
    and opamfile_item = opamfile_item_kind with_pos

    (** A file is a list of items and the filename *)
    type opamfile = {
      file_contents: opamfile_item list; (** Content of the file *)
      file_name    : file_name;          (** Name of the disk file this record was
                                             loaded from *)
    }

  end

end

module OpamParser : sig

  (*  open OpamParserTypes *)

  module FullPos : sig

    open OpamParserTypes.FullPos


  (*
  (** {2 Raw OpamBaseParser entry points } *)

  (**
      Providing a custom [lexbuf] argument allows you, for example, to set the
      initial lexing position. For the first argument, you may use the
      {!OpamLexer.token} lexing function:

     {[
       let lexbuf = Lexing.from_string input in
       lexbuf.Lexing.lex_curr_p <- current_position;
       OpamParser.value OpamLexer.token lexbuf
     ]}
  *)

  val main:
    (Lexing.lexbuf  -> OpamBaseParser.token) ->
    Lexing.lexbuf -> file_name -> opamfile
  (** Principal parser: given a lexbuf and the filename it was read from, returns
      an {!OpamParserTypes.FullPos.opamfile} record parsed from it. *)

  val value:
    (Lexing.lexbuf  -> OpamBaseParser.token) ->
    Lexing.lexbuf -> value
  (** Lower-level function just returning a single
      {!OpamParserTypes.FullPos.value} from a given lexer. *)

  (** {2 File parsers } *)

  val string: string -> file_name -> opamfile
  (** Parse the content of a file already read to a string. Note that for
      CRLF-detection to work on Windows, it is necessary to read the original file
      using binary mode on Windows! *)

  val channel: in_channel -> file_name -> opamfile
  (** Parse the content of a file from an already-opened channel. Note that for
      CRLF-detection to work on Windows, it is necessary for the channel to be
      in binary mode! *)
     *)

  val file: file_name -> opamfile
  (** Parse the content of a file. The file is opened in binary mode, so
      CRLF-detection works on all platforms. *)

  (*
  (** {2 [value] parsers } *)

     *)

       val value_from_string: string -> file_name -> value
  (** Parse the first value in the given string. [file_name] is used for lexer
      positions. *)

       (*
  val value_from_channel: in_channel -> file_name -> value
  (** Parse the first value from the given channel. [file_name] is used for
      lexer positions. *)

  val value_from_file: file_name -> value
  (** Parse the first value from the given file. *)

  (** {2 Conversion functions, from full position to simple position (legacy)} *)

  val to_value: value -> OpamParserTypes.value
  val to_section: opamfile_section -> OpamParserTypes.opamfile_section
  val to_item: opamfile_item -> OpamParserTypes.opamfile_item
  val to_opamfile: opamfile -> OpamParserTypes.opamfile
     *)

end

  (*
val main:
  (Lexing.lexbuf  -> OpamBaseParser.token) ->
  Lexing.lexbuf -> file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.main instead."]

val value:
  (Lexing.lexbuf  -> OpamBaseParser.token) ->
  Lexing.lexbuf -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value instead."]

val string: string -> file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.string instead."]

val channel: in_channel -> file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.channel instead."]
val file: file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.file instead."]

val value_from_string: string -> file_name -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value_from_string instead."]

val value_from_channel: in_channel -> file_name -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value_from_channel instead."]

val value_from_file: file_name -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value_from_file instead."]
     *)

end

module OpamPrinter : sig

  (*
(** Functions for converting parsed opam files back to strings *)


(** [OpamPrinter] transitional module with full position types *)


*)
module FullPos : sig

  open OpamParserTypes.FullPos

    (*

  (** {2 Printers for the [value] and [opamfile] formats} *)

  val relop_kind: [< relop_kind ] -> string
  (** Converts {!OpamParserTypes.FullPos.relop_kind} to its string representation
      ([=], [!=], ..., [~]). *)

  val logop_kind: [< logop_kind ] -> string
  (** Converts {!OpamParserTypes.FullPos.logop_kind} to its string representation
      ([&] and [|]). *)

  val pfxop_kind: [< pfxop_kind ] -> string
  (** Converts {!OpamParserTypes.FullPos.logop_kind} to its string representation
      ([&] and [|]). *)

  val env_update_op_kind: env_update_op_kind -> string
  (** Converts {!OpamParserTypes.FullPos.env_update_op_kind} to its string representation
      ([=], [+=], ..., [=:]). *)


  val relop: relop -> string
  (** Converts {!OpamParserTypes.FullPos.relop} to its string representation
      ([=], [!=], ..., [~]). *)

  val logop: logop -> string
  (** Converts {!OpamParserTypes.FullPos.logop} to its string representation
      ([&] and [|]). *)

  val pfxop: pfxop -> string
  (** Converts {!OpamParserTypes.FullPos.pfxop} to its string representation
      ([!] and [?]). *)

  val env_update_op: env_update_op -> string
  (** Converts {!OpamParserTypes.FullPos.env_update_op} to its string representation
      ([=], [+=], ..., [=:]). *)

*)

  val value : value -> string
  (** Converts {!value} to a string {b always using LF-encoding of newlines}. *)

  (*
  val value_list: value list with_pos -> string
  (** Converts a list of {!value}s to a string {b always using LF-encoding of
      newlines}. *)

  val items: opamfile_item list -> string

*)

  val opamfile: opamfile -> string
  (** Converts an {!opamfile} to a string. *)

  (*
  val format_opamfile: Format.formatter -> opamfile -> unit
  (** Writes an {!opamfile} to a [Format.formatter]. The function ensures that all
      newlines are sent using [Format]'s break instructions (and so ultimately are
      processed with the [out_newline] function of the formatter) but it is the
      responsibility of the caller to ensure that the formatter is configured for
      the required output, if necessary. *)

  (** {2 Normalised output for opam syntax files} *)

  (** opam normalised file format, for signatures.

      - each top-level field on a single line
      - newlines are LF-encoded (including on Windows)
      - file ends with a newline
      - spaces only after [fieldname:], between elements in lists, before braced
          options, between operators and their operands
      - fields are sorted lexicographically by field name
          (using [String.compare])
      - newlines in strings turned to ['\n'], backslashes and double quotes
          escaped
      - no comments (they don't appear in the internal file format anyway)
      - fields containing an empty list, or a singleton list containing an empty
          list, are not printed at all
  *)
  module Normalise : sig
    val escape_string : string -> string
    val value : value -> string
    val item : opamfile_item -> string
    val item_order : opamfile_item -> opamfile_item -> int
    val items : opamfile_item list -> string
    val opamfile : opamfile -> string
  end

  (** {2 Format-preserving reprinter} *)

  module Preserved : sig

    val items: string -> opamfile_item list -> opamfile_item list -> string
    (** [items str orig_its its] converts [its] to string, while attempting to
        preserve the layout and comments of the original [str] for unmodified
        elements. The function assumes that [str] parses to the items
        [orig_its]. *)

    val opamfile: ?format_from:file_name -> opamfile -> string
    (** [opamfile f] converts [f] to string, respecting the layout and comments in
        the corresponding on-disk file for unmodified items. [format_from] can be
        specified instead of using the filename specified in [f]. *)
  end

  (** {2 Random utility functions} *)

  val value_equals: value -> value -> bool
  (** Compares structurally, without considering file positions *)

  val opamfile_item_equals: opamfile_item -> opamfile_item -> bool
  (** Compares structurally, without considering file positions *)
*)

end

(*
open OpamParserTypes

val relop: [< relop ] -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.relop instead."]

val logop: [< logop ] -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.logop instead."]

val pfxop: [< pfxop ] -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.pfxop instead."]

val env_update_op: env_update_op -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.env_update_op instead."]

val value : value -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.value instead."]

val value_list: value list -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.value_list instead."]

val items: opamfile_item list -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.items instead."]

val opamfile: opamfile -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.opamfile instead."]

val format_opamfile: Format.formatter -> opamfile -> unit
[@@ocaml.deprecated "Use OpamPrinter.FullPos.format_opamfile instead."]

module Normalise : sig
  val escape_string : string -> string
  val value : value -> string
  val item : opamfile_item -> string
  val item_order : opamfile_item -> opamfile_item -> int
  val items : opamfile_item list -> string
  val opamfile : opamfile -> string
end
[@@ocaml.deprecated "Use OpamPrinter.FullPos.Normalise instead."]

module Preserved : sig
  val items: string -> opamfile_item list -> opamfile_item list -> string

  val opamfile: ?format_from:file_name -> opamfile -> string
end
[@@ocaml.deprecated "Use OpamPrinter.FullPos.Preserved instead."]


val value_equals: value -> value -> bool
[@@ocaml.deprecated "Use OpamPrinter.FullPos.value_equals instead."]

val opamfile_item_equals: opamfile_item -> opamfile_item -> bool
[@@ocaml.deprecated "Use OpamPrinter.FullPos.opamfile_item_equals instead."]


*)

end
