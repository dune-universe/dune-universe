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

(** A wrapper on the FreeTDS library for accessing Sybase and
    Microsoft database providers. *)
open Printf

(* This is the catch-all exception that client code should trap *)
exception End_results
exception End_data
exception Cancelled
exception Cmd_fail

(** And this is for me to let you know I didn't wrap every datatype :-) *)
exception Datatype_not_implemented

type context
type connection
type command

type binding_buffer

type prop_action = [ `Get | `Set | `Clear ]

type string_property = [ `Username | `Password | `Appname ]

type cmd_type = [ `Lang | `Rpc ]

type cmd_option = [ `Recompile | `NoRecompile ]

type result_type = [ `Row | `Param | `Status | `Cmd_done | `Cmd_succeed | `Cmd_fail ]

let string_of_result_type : result_type -> string = function
  | `Row -> "Row"
  | `Param -> "Param"
  | `Status -> "Status"
  | `Cmd_done -> "Cmd_done"
  | `Cmd_succeed -> "Cmd_succeed"
  | `Cmd_fail -> "Cmd_fail"

type resinfo_type = [ `Row_count | `Cmd_number | `Numdata ]

(* type datetime = {
 *     days : int;
 *     minutes : int;
 *     three_hundredths : int;
 * } *)

(* type locale = {
 *     language : string;
 *     charset : string;
 *     time : string;
 *     collate : string;
 * } *)

type status = [ `CanBeNull | `NoData | `Identity | `Return ]

type column = {
    col_name : string;
    col_status : status list;
    col_buffer : binding_buffer
}

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

        (* These are the types I didn't want to even try to convert, so I let the DB convert them
           to strings for me *)

        (* Contains DATETIME and DATETIME4 *)
        | `Datetime of string

        (* Contains DECIMAL, MONEY, NUMERIC, etc *)
        | `Decimal of string

        | `Null
        ]

let string_of_sql_t : sql_t -> string = function
  | `Bit b -> if b then "Bit(1)" else "Bit(0)"
  | `Tinyint i -> sprintf "Tinyint(%i)" i
  | `Smallint i -> sprintf "Smallint(%i)" i
  | `Int i -> sprintf "Int(%li)" i
  | `Text s -> sprintf "Text(%S)" s
  | `String s -> sprintf "String(%S)" s
  | `Binary s -> sprintf "Binary(%S)" s
  | `Float f -> sprintf "Float(%g)" f
  | `Datetime s -> sprintf "Datetime(%s)" s
  | `Decimal s -> sprintf "Decimal(%s)" s
  | `Null -> "Null"

let _ =
    List.iter (fun (x,y) -> Callback.register_exception x y)
        [
            "cs_end_results", End_results;
            "cs_end_data", End_data;
            "cs_cancelled", Cancelled;
            "cs_cmd_fail", Cmd_fail;
            "cs_not_implemented", Datatype_not_implemented;
        ]
;;

(* Public interface, with unused parameters omitted. *)

(* This wraps both ctx_alloc and ct_init *)
external ctx_create : unit -> context = "mltds_cs_ctx_create"

(* Conections *)
external con_alloc : context -> connection = "mltds_ct_con_alloc"

(* This is a type-safer wrapper around ct_con_prop *)
external con_setstring : connection -> string_property -> string -> unit = "mltds_ct_con_setstring"
external connect : connection -> string -> unit = "mltds_ct_connect"

(* Commands *)
external cmd_alloc : connection -> command = "mltds_ct_cmd_alloc"
external command : command -> cmd_type -> ?option:cmd_option -> string -> unit = "mltds_ct_command"
external send : command -> unit = "mltds_ct_send"

(* Results *)
external results : command -> result_type = "mltds_ct_results"
external res_info : command -> resinfo_type -> int = "mltds_ct_res_info"
external fetch : command -> int = "mltds_ct_fetch"

(* Bind returns a binding buffer *)
external buffer_contents : binding_buffer -> sql_t = "mltds_buffer_contents"

(* A level of indirection to save me some C *)
external bind_col : command -> maxlen:int -> int -> column = "mltds_ct_bind"
let bind comm ?(maxlen = 256) index = bind_col comm ~maxlen index

(* Closing of connections *)
external close_con : connection -> bool -> unit = "mltds_ct_close"
let close ?(force = false) conn = close_con conn force

type severity =
  | Inform
  | Api_fail
  | Retry_fail
  | Resource_fail
  | Comm_fail
  | Internal_fail
  | Fatal

(* Error handling - but ct_diag not yet implemented *)
external add_messages_client :
  connection -> (severity * string) list -> (severity * string) list
  = "mltds_add_messages_client"

external add_messages_server :
  connection -> (severity * string) list -> (severity * string) list
  = "mltds_add_messages_server"

let get_messages ?(client=false) ?(server=false) conn =
  let l = if client then add_messages_client conn [] else [] in
  if server then add_messages_server conn l else l

;;
