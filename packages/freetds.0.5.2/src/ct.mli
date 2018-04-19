(*
  This file is part of ocaml-freetds - An OCaml binding to the FreeTDS library
  Copyright (C) 2004 Kenneth Knowles

  ocaml-freetds is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  ocaml-freetds is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with ocaml-freetds; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)


exception End_results
exception End_data
exception Cancelled
exception Cmd_fail
exception Datatype_not_implemented

(** I've added this type to be an opaque "void*" type that will return
    a sql_t *)
type binding_buffer

(** CS_CONTEXT *)
type context

(** CS_CONNECTION *)
type connection

(** CS_COMMAND *)
type command

(** CS_CLEAR CS_GET and CS_SET *)
type prop_action = [ `Clear | `Get | `Set ]

(** CS_APPNAME, CS_PASSWORD, and CS_USERNAME - there are others but
    I've never used them...  Easy to add on demand; patches welcome *)
type string_property = [ `Appname | `Password | `Username ]

(** CS_LANG and CS_RPC - rpc is currently not really implemented
    - you'll notice no ct_param wrapper *)
type cmd_type = [ `Lang | `Rpc ]

(** Options for compiling CS_NORECOMPILE and CS_RECOMPILE *)
type cmd_option = [ `NoRecompile | `Recompile ]


(** Result types; usually we only care about Row *)
type result_type =
        [ `Cmd_done | `Cmd_fail | `Cmd_succeed | `Param | `Row | `Status ]


type resinfo_type = [ `Cmd_number | `Numdata | `Row_count ]

type status = [ `CanBeNull | `Identity | `NoData | `Return ]

type column = {
    col_name : string;
    col_status : status list;
    col_buffer : binding_buffer;
}

(** Types for fetching / inspecting errors *)
type location =
        [
        | `Server
        | `Client
        ]


type severity =
        [
        | `Inform
        | `Api_fail
        | `Retry_fail
        | `Resource_fail
        | `Config_fail
        | `Comm_fail
        | `Internal_fail
        | `Fatal
        ]

type sql_t =
[
| `Bit of bool
| `Tinyint of int
| `Smallint of int
| `Int of int32
| `Text of string
| `String of string
| `Binary of string

(* 64 bits *)
| `Float of float

(* These are the types I didn't want to even try to convert, so I let
   the DB convert them to strings for me *)

(* Contains DATETIME and DATETIME4 *)
| `Datetime of string

(* Contains DECIMAL, MONEY, NUMERIC, etc *)
| `Decimal of string
| `Null
]

(** {4 Context} *)
external ctx_create : unit -> context = "mltds_cs_ctx_create"

(** {4 Connection} *)

external con_alloc : context -> connection = "mltds_ct_con_alloc"
external con_setstring : connection -> string_property -> string -> unit
    = "mltds_ct_con_setstring"
external connect : connection -> string -> unit = "mltds_ct_connect"
val close : ?force:bool -> connection -> unit

(** {4 Command} *)
external cmd_alloc : connection -> command = "mltds_ct_cmd_alloc"
external command :
    command -> cmd_type -> ?option:cmd_option -> string -> unit
    = "mltds_ct_command"
external send : command -> unit = "mltds_ct_send"

(** {4 Results} *)
external results : command -> result_type = "mltds_ct_results"
external res_info : command -> resinfo_type -> int = "mltds_ct_res_info"

external fetch : command -> int = "mltds_ct_fetch"
val bind : command -> ?maxlen:int -> int -> column

external buffer_contents : binding_buffer -> sql_t
    = "mltds_buffer_contents"

external get_messages : connection -> location list -> (severity * string) list
    = "mltds_get_messages"
