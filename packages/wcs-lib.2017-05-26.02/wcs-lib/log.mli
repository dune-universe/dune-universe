(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

(**
   Logging utilities.
*)

(** {6 Errors} *)

exception Error of string * string
(** Exception raise in case of error. It is associated with the module
    name and the error message.
*)

val error_recovery : bool ref
(** Set if we should avoid to fail in case of error. *)

val error : string -> 'a option -> string -> 'a
(** [error module_name default msg] raises [Error]. If a default value
    is provided and [error_recovery] is [true], it returns the value
    instead of raising the exception [Error]. In this case, the error
    message is printed.
*)

val print_error : string -> string -> unit
(** [print_error module_name msg] prints the error message [msg]
    prefixed with the module name [module_name].
*)

(** {6 Warnings} *)

val warning_message : bool ref
(** Set if we should display warnings. *)

val warning : string -> string -> unit
(** [warning module_name msg] prints the warning message [msg] prefixed
    with the module name [module_name].
*)


(** {6 Debug messages} *)

val debug_message : bool ref
(** Set if we should display debug. *)

val debug : string -> string -> unit
(** [debug module_name msg] prints the debug message [msg] prefixed
    with the module name [module_name].
*)
