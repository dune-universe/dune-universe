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

external dbinit : unit -> unit = "ocaml_freetds_dbinit"

let () =
  Callback.register_exception "Freetds.Dblib.Error"
                              (Error(FATAL, "message"));
  err_handler default_err_handler;
  msg_handler default_msg_handler;
  dbinit()
  (* One must call this function before trying to use db-lib in any
     way.  Allocates various internal structures and reads
     locales.conf (if any) to determine the default date format.  *)

type dbprocess

external dbopen :
  user:string option -> password:string option ->
  charset: string option -> language: string option -> application:string option
  -> server:string -> dbprocess
  = "ocaml_freetds_dbopen_bc" "ocaml_freetds_dbopen"

let connect ?user ?password ?charset ?language ?application server =
  dbopen ~user ~password ~charset ~language ~application ~server

external close : dbprocess -> unit = "ocaml_freetds_dbclose"

external use : dbprocess -> string -> unit = "ocaml_freetds_dbuse"

external name : dbprocess -> string = "ocaml_freetds_dbname"

external sqlexec : dbprocess -> string -> unit = "ocaml_freetds_dbsqlexec"

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
  | INT64 of string (* FIXME: do better *)
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
  | INT64 i -> sprintf "INT64(%s)" i
  | FLOAT f -> sprintf "FLOAT(%f)" f
  | DATETIME(y, mo, day, h, m, s, _, _) ->
      sprintf "DATETIME(%i/%i/%i %i:%02i:%02i)" y mo day h m s
  | MONEY f -> sprintf "MONEY(%f)" f
  | BIT b -> sprintf "BIT(%b)" b
  | BINARY s -> sprintf "BINARY(%S)" s
  | NUMERIC s -> sprintf "NUMERIC(%S)" s
  | DECIMAL s -> sprintf "DECIMAL(%S)" s


external nextrow : dbprocess -> data list = "ocaml_freetds_dbnextrow"

external count : dbprocess -> int = "ocaml_freetds_dbcount"

external settime : int -> unit = "ocaml_freetds_dbsettime"
