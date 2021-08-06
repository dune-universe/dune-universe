(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Module handling the local environment of a file.
    
    Pre-processing is applied in the first place on the interface file.
    That first step collect information on functions where there is a pbt
    specification attached to a signature_item. These information are storred
    in [t], they later can be stored on the disk and restored when the
    pre-processing is applied on the implementation file *)

type md_expr = [ `Structure | `Functor ]

(** [path] is a DFS representation of recursive signature item *)
type path = [ `Psig_module of string | md_expr ] list

(** [psig_value] stores the function with the associated specification found
    in the attributes.

    The {!path} and actual signature_item is stored inside [psig_value] in order
    to be retrived in the respective structure_item *)
type psig_value

type t

(** [get_path psig_value] returns the path inside [psig_value] *)
val get_path : psig_value -> path

(** [get_properties psig_value] returns the properties inside [psig_value] *)
val get_properties : psig_value -> Properties.t

(** [get_name psig_value] returns the function name inside [psig_value] *)
val get_name : psig_value -> string

(** [get_value psig_value] returns the actual value inside [psig_value] *)
val get_value : psig_value -> Ppxlib.signature_item option

(** [get_file_name ()] returns the file_name inside the environment [t]

    Default value is "_none_" if the file_name has not been set *)
val get_file_name : unit -> string

(** [get_file_name_exn ()] returns the file_name inside the environment [t]

    Raise an exception if the file_name has not been set *)
val get_file_name_exn : unit -> string

(** [get_file_name_opt ()] returns the file_name inside the environment [t]

    Returns None if the file_name has not been set *)
val get_file_name_opt : unit -> string option

(** [get_psig_values ()] returns the list of psig_value in the environment [t] *)
val get_psig_values : unit -> psig_value list

(** [env_with_psig_value psig_value] add [psig_value] to the environment [t] *)
val env_with_psig_value : psig_value -> unit

(** [env_with_file_name file_name] set the file_name in the environment [t] *)
val env_with_file_name : string -> unit

(** [add_env name] build and add a psig_value in the environment [t] *)
val add_env :
  ?path:path ->
  ?properties:Properties.t ->
  ?value:Ppxlib.signature_item ->
  string ->
  unit

(** [init_env] reset the environment to default state, file_name being processed can
    be set at this point with the parameter [file_name] *)
val init_env : ?file_name:string -> unit -> unit

(** [store_env] store on disk the environment [t] *)
val store_env : unit -> unit

(** [fetch_env file_path] restore the environment [t] from the disk using [file_name]
    to fetch the correct environment *)
val fetch_env : string -> unit

(** [pp ()] pretty print the current environment, exposed only for internal testing *)
val pp : Format.formatter -> unit
