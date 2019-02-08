(* File: dblib.ml

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Printf

type severity =
  | INFO
  | USER
  | NONFATAL
  | CONVERSION
  | SERVER
  | TIME
  | PROGRAM
  | RESOURCE
  | COMM
  | FATAL
  | CONSISTENCY

let pp_severity _ = function
  | INFO -> "INFO"
  | USER -> "USER"
  | NONFATAL -> "NONFATAL"
  | CONVERSION -> "CONVERSION"
  | SERVER -> "SERVER"
  | TIME -> "TIME"
  | PROGRAM -> "PROGRAM"
  | RESOURCE -> "RESOURCE"
  | COMM -> "COMM"
  | FATAL -> "FATAL"
  | CONSISTENCY -> "CONSISTENCY"

exception Error of severity * string

let err_handler (f: severity -> int -> string -> unit) =
  Callback.register "Freetds.Dblib.err_handler" f

let default_err_handler severity _err msg =
  raise(Error(severity, msg))

let msg_handler (f: severity -> int -> string -> unit) =
  Callback.register "Freetds.Dblib.msg_handler" f

let default_msg_handler severity line msg =
  match severity with
  | FATAL | CONSISTENCY ->
    let msg = sprintf "Error on line %d: %s" line msg in
    raise(Error(severity, msg))
  | INFO | USER | NONFATAL | CONVERSION | SERVER | TIME | PROGRAM
    | RESOURCE | COMM -> ()

type version = V42 | V46 | V70 | V71 | V72 | V73 | V74

external dbinit : unit -> unit = "ocaml_freetds_dbinit"

let init version =
  Callback.register_exception "Freetds.Dblib.Error"
                              (Error(FATAL, "message"));
  let pp_error = function
    | Error(s, m) -> Some(sprintf "Error(%a, %S)" pp_severity s m)
    | _ -> None in
  Printexc.register_printer pp_error;
  err_handler default_err_handler;
  msg_handler default_msg_handler;
  dbinit version
  (* One must call this function before trying to use db-lib in any
     way.  Allocates various internal structures and reads
     locales.conf (if any) to determine the default date format.  *)

let is_initialized = ref false

type dbprocess

external dbopen :
  user:string option -> password:string option ->
  charset: string option -> language: string option ->
  application:string option -> version: version option
  -> server:string -> dbprocess
  = "ocaml_freetds_dbopen_bc" "ocaml_freetds_dbopen"

let connect ?user ?password ?charset ?language ?application ?version
      server =
  if not !is_initialized then init ();
  dbopen ~user ~password ~charset ~language ~application ~version ~server

external close : dbprocess -> unit = "ocaml_freetds_dbclose"

external dbuse : dbprocess -> string -> unit = "ocaml_freetds_dbuse"

let use db name =
  try dbuse db name
  with _ ->
    let msg = sprintf "Freetds.Dblib.use: unable to open the database %S"
                name in
    raise(Error(PROGRAM, msg))

external name : dbprocess -> string = "ocaml_freetds_dbname"

external dbsqlexec : dbprocess -> string -> unit = "ocaml_freetds_dbsqlexec"

let sqlexec db sql =
  try dbsqlexec db sql
  with
  | Not_found ->
     let msg = sprintf "Freetds.Dblib.sqlexec: the SQL query %S is invalid. \
                        It may be due to a SQL syntax error, incorrect column \
                        or table names, or if the previous query results were \
                        not completely read,..." sql in
     raise (Error(PROGRAM, msg))
  | Error(severity, msg) -> (* The handler may raise exceptions. *)
     let msg = sprintf "Freetds.Dblib.sqlexec: the SQL query %S generated \
                        the error %S" sql msg in
     raise(Error(severity, msg))

external cancel :  dbprocess -> unit = "ocaml_freetds_dbcancel"
external canquery :  dbprocess -> unit = "ocaml_freetds_dbcanquery"

external results : dbprocess -> bool = "ocaml_freetds_dbresults"

external numcols : dbprocess -> int = "ocaml_freetds_numcols" [@@noalloc]
    (** Return number of regular columns in a result set.  *)

external colname : dbprocess -> int -> string = "ocaml_freetds_dbcolname"

(* See /usr/include/sybdb.h *)

type col_type =
  | SYBCHAR (* 0 *) | SYBVARCHAR (* 1 *)
  | SYBINTN (* 2 *) | SYBINT1 (* 3 *) | SYBINT2 (* 4 *)
  | SYBINT4 (* 5 *) | SYBINT8 (* 6 *)
  | SYBFLT8 (* 7 *) | SYBFLTN (* 8 *)
  | SYBNUMERIC (* 9 *)
  | SYBDECIMAL (* 10 *)
  | SYBDATETIME (* 11 *) | SYBDATETIME4 (* 12 *) | SYBDATETIMN (* 13 *)
  | SYBBIT (* 14 *)
  | SYBTEXT (* 15 *)
  | SYBIMAGE (* 16 *)
  | SYBMONEY4 (* 17 *) | SYBMONEY (* 18 *) | SYBMONEYN (* 19 *)
  | SYBREAL (* 20 *)
  | SYBBINARY (* 21 *) | SYBVARBINARY (* 22 *)

let string_of_col_type = function
  | SYBCHAR -> "CHAR"
  | SYBVARCHAR -> "VARCHAR"
  | SYBINTN -> "INT"    | SYBINT1 -> "INT1" | SYBINT2 -> "INT2"
  | SYBINT4 -> "INT4"   | SYBINT8 -> "INT8"
  | SYBFLT8 -> "FLOAT8" | SYBFLTN -> "FLOAT"
  | SYBREAL -> "REAL"
  | SYBBIT -> "BIT"
  | SYBTEXT -> "TEXT"
  | SYBIMAGE -> "IMAGE"
  | SYBMONEY4 -> "MONEY4" | SYBMONEY -> "MONEY" | SYBMONEYN -> "MONEY"
  | SYBDATETIME -> "DATETIME"
  | SYBDATETIME4 -> "DATETIME4" | SYBDATETIMN -> "DATETIME"
  | SYBBINARY -> "BINARY" | SYBVARBINARY -> "VARBINARY"
  | SYBNUMERIC -> "NUMERIC"
  | SYBDECIMAL -> "DECIMAL"
;;
external coltype : dbprocess -> int -> col_type = "ocaml_freetds_dbcoltype"

(* See /usr/include/sybdb.h, CHARBIND ... *)
type data =
  | NULL
  | STRING of string                    (* tag = 0 *)
  | TINY of int
  | SMALL of int
  | INT of int
  | INT32 of int32
  | INT64 of int64
  | FLOAT of float                      (* tag = 6 *)
  | DATETIME of int * int * int * int * int * int * int * int
  | MONEY of float
  | BIT of bool
  | BINARY of string                    (* tag = 10 *)
  | NUMERIC of string (* FIXME: do better *)
  | DECIMAL of string (* FIXME: do better *)

let string_of_data = function
  | NULL -> "NULL"
  | STRING s -> sprintf "STRING(%S)" s
  | TINY i -> sprintf "TINY(%i)" i
  | SMALL i -> sprintf "SMALL(%i)" i
  | INT i -> sprintf "INT(%i)" i
  | INT32 i -> sprintf "INT32(%li)" i
  | INT64 i -> sprintf "INT64(%Li)" i
  | FLOAT f -> sprintf "FLOAT(%f)" f
  | DATETIME(y, mo, day, h, m, s, _, _) ->
      sprintf "DATETIME(%i/%i/%i %i:%02i:%02i)" y mo day h m s
  | MONEY f -> sprintf "MONEY(%f)" f
  | BIT b -> sprintf "BIT(%b)" b
  | BINARY s -> sprintf "BINARY(%S)" s
  | NUMERIC s -> sprintf "NUMERIC(%S)" s
  | DECIMAL s -> sprintf "DECIMAL(%S)" s


external dbnextrow : dbprocess -> int = "ocaml_freetds_dbnextrow"
(* Return the ID of computed queries or REG_ROW (value extracted
   by discover.ml) for a regular result.
   Raises [Not_found] if NO_MORE_ROWS. *)

type data_ptr

external dbdata : dbprocess -> col:int -> data_ptr
  = "ocaml_freetds_dbdata"
(* Note that [data_ptr] is a pointer to the data. *)

external is_null : data_ptr -> bool = "ocaml_freetds_is_null" [@@noalloc]

external dbdatlen : dbprocess -> col:int -> int
  = "ocaml_freetds_dbdatlen" [@@noalloc]

external get_data : dbprocess -> col:int -> data_ptr -> data
  = "ocaml_freetds_get_data"
(* Beware that [data_ptr] must be those associated with [col]. *)

let nextrow db =
  let status = dbnextrow db in
  if status = REG_ROW then (
    let row = ref [] in
    for c = numcols db downto 1 do
      let data_ptr = dbdata db ~col:c in
      let len = dbdatlen db ~col:c in
      if len < 0 then
        let msg = sprintf "FreeTDS.Dblib.nextrow: column %d does not exist. \
                           Contact the OCaml FreeTDS developers." c in
        raise(Error(FATAL, msg))
      else if is_null data_ptr then
        if len = 0 then
          row := NULL :: !row
        else
          let msg = sprintf "Freetds.Dlib.nextrow: column %d has a length \
                             of %d but no data is returned." c len in
          raise(Error(FATAL, msg))
      else
        row := get_data db ~col:c data_ptr :: !row
    done;
    !row
  )
  else
    (* [status] = ID of computed row *)
    failwith "Computed rows are not handled at the moment.  Please write \
              to the developers of OCaml FreeTDS."

external count : dbprocess -> int = "ocaml_freetds_dbcount"

external settime : int -> unit = "ocaml_freetds_dbsettime"
