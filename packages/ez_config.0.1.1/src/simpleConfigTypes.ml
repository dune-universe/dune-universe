(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type option_value =
    Module of option_module
  | StringValue of string
  | IntValue of int
  | FloatValue of float
  | List of option_value list
  | SmallList of option_value list
  | OnceValue of option_value
  | DelayedValue of (Buffer.t -> string -> unit)
and  option_module = (string * option_value) list

type load_error =
  | FileDoesNotExist
  | FileCannotBeRead
  | ParseError of int * string
  | FileHasTempBackup of FileAbstract.t
  | SetOptionFailed of string * string

exception LoadError of FileAbstract.t * load_error
