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

module T = OpamParserTypes

module OpamParserTypes = struct

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
  type env_update_op = T.env_update_op = Eq       (** [=] *)
                     | PlusEq   (** [+=] *)
                     | EqPlus   (** [=+] *)
                     | ColonEq  (** [:=] *)
                     | EqColon  (** [=:] *)
                     | EqPlusEq (** [=+=] *)

  (** [OpamParserTypes] transitional module with full position types *)
  module FullPos = struct

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


module OpamParser = struct

  open OpamParserTypes.FullPos

  let v (filename,start,stop) pelem = {
    pos = { filename ; start = (0, start) ; stop = (0, stop) } ; pelem }

  let rec translate = function
    | T.Bool ( pos, bool ) -> v pos (Bool bool)
    | T.Int ( pos, bool ) -> v pos (Int bool)
    | T.String ( pos, bool ) -> v pos (String bool)
    | T.Ident ( pos, bool ) -> v pos (Ident bool)
    | T.Relop (pos, relop, v1, v2 ) ->
        v pos (Relop (v pos relop, translate v1, translate v2))
    | T.Prefix_relop (pos, relop, v1 ) ->
        v pos (Prefix_relop (v pos relop, translate v1))
    | T.Logop (pos, relop, v1, v2 ) ->
        v pos (Logop (v pos relop, translate v1, translate v2))
    | T.Pfxop (pos, relop, v1 ) ->
        v pos (Pfxop (v pos relop, translate v1))
    | T.List (pos, list )  ->
        v pos (List
                 ( v pos (List.map translate list)))
    | T.Group (pos, list )  ->
        v pos (Group
                 ( v pos (List.map translate list)))
    | T.Option (pos, v1, list )  ->
        v pos (Option (translate v1,
                       v pos (List.map translate list)))
    | T.Env_binding (pos, v1, env_update_op, v2 )  ->
        v pos (Env_binding (translate v1,
                            v pos env_update_op, translate v2))

  and translate_file { T.file_contents ; T.file_name } =
      {
        file_contents = List.map translate_item file_contents;
        file_name
      }

  and translate_item = function
    | T.Section (pos, section) ->
        v pos ( Section (translate_section pos section))
    | T.Variable (pos, s, v1) ->
        v pos ( Variable (v pos s, translate v1))

  and translate_section pos
      { T.section_kind ; T.section_name ; T.section_items } =
    {
      section_kind = v pos section_kind ;
      section_name = ( match section_name with
          | None -> None
          | Some section_name -> Some ( v pos section_name ) );
      section_items =
        v pos ( List.map translate_item section_items );
    }

  module FullPos = struct
    let value_from_string a b =
      translate ( OpamParser.value_from_string a b  )
    let file name =
      translate_file ( OpamParser.file name)
  end
end

module OpamPrinter = struct

  open OpamParserTypes.FullPos

  let rpos v = ( v.pos.filename, snd v.pos.start, snd v.pos.stop)

  let rec translate { file_contents; file_name } =

    let file_contents = List.map translate_item file_contents in
    { T.file_contents ; T.file_name }

  and translate_item item = match item.pelem with
    | Section section -> T.Section (rpos item,
                                    translate_section section)
    | Variable (string, value) ->
        T.Variable (rpos string, string.pelem, translate_value value)

  and translate_section { section_kind ; section_name ; section_items } =
    {
      T.section_kind = section_kind.pelem ;
      T.section_name = ( match section_name with
          | None -> None
          | Some s -> Some s.pelem) ;
      T.section_items = List.map translate_item section_items.pelem ;
    }
  and translate_value v =
    let pos = rpos v in
    match v.pelem with
    | Bool b -> T.Bool (pos, b)
    | Int b -> T.Int (pos, b)
    | String b -> T.String (pos, b)
    | Ident b -> T.Ident (pos, b)
    | Relop (relop, v1, v2) ->
        T.Relop ( pos, relop.pelem, translate_value v1, translate_value v2 )
    | Logop (relop, v1, v2) ->
        T.Logop ( pos, relop.pelem, translate_value v1, translate_value v2 )
    | Prefix_relop (relop, v1) ->
        T.Prefix_relop ( pos, relop.pelem, translate_value v1 )
    | Pfxop (relop, v1) ->
        T.Pfxop ( pos, relop.pelem, translate_value v1 )
    | List list ->
        T.List ( pos, List.map translate_value list.pelem )
    | Group list ->
        T.Group ( pos, List.map translate_value list.pelem )
    | Option (v1, list) ->
        T.Option ( pos, translate_value v1,
                   List.map translate_value list.pelem )
    | Env_binding (v1, relop, v2) ->
        T.Env_binding ( pos, translate_value v1, relop.pelem, translate_value v2 )

  module FullPos = struct
    let opamfile file =
      OpamPrinter.opamfile (translate file)
    let value file =
      OpamPrinter.value (translate_value file)
  end

end


(*

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types for the opam format lexer and parser *)

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

(** Base values *)
type value =
  | Bool of pos * bool
      (** [bool] atoms *)
  | Int of pos * int
      (** [int] atoms *)
  | String of pos * string
      (** [string] atoms *)
  | Relop of pos * relop * value * value
      (** Relational operators with two values (e.g. [os != "win32"]) *)
  | Prefix_relop of pos * relop * value
      (** Relational operators in prefix position (e.g. [< "4.07.0"]) *)
  | Logop of pos * logop * value * value
      (** Logical operators *)
  | Pfxop of pos * pfxop * value
      (** Prefix operators *)
  | Ident of pos * string
      (** Identifiers *)
  | List of pos * value list
      (** Lists of values ([[x1 x2 ... x3]]) *)
  | Group of pos * value list
      (** Groups of values ([(x1 x2 ... x3)]) *)
  | Option of pos * value * value list
      (** Value with optional list ([x1 {x2 x3 x4}]) *)
  | Env_binding of pos * value * env_update_op * value
      (** Environment variable binding ([FOO += "bar"]) *)

(** An opamfile section *)
type opamfile_section = {
  section_kind  : string;            (** Section kind (e.g. [extra-source]) *)
  section_name  : string option;     (** Section name (e.g. ["myfork.patch"]) *)
  section_items : opamfile_item list (** Content of the section *);
}

(** An opamfile is composed of sections and variable definitions *)
and opamfile_item =
  | Section of pos * opamfile_section (** e.g. [kind ["name"] { ... }] *)
  | Variable of pos * string * value  (** e.g. [opam-version: "2.0"] *)

(** A file is a list of items and the filename *)
type opamfile = {
  file_contents: opamfile_item list; (** Content of the file *)
  file_name    : file_name;          (** Name of the disk file this record was
                                         loaded from *)
}


*)
