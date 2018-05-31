(* File: dblib.mli

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


(** Low level binding to the DB-lib part of freetds.  These bindings
    mimic the C API and therefore some functions must be used in the
    right order.  Look at the
    {{:http://freetds.schemamania.org/userguide/}FreeTDS User Guide}
    and {{:http://sybooks.sybase.com/sybooks/sybooks.xhtml}Sybase
    documentation} for fine points.  Use OCamlDBI with the freetds
    driver for an easier interaction with such databases. *)

type dbprocess
  (** Value that contains all information needed by [Freetds.Dblib] to
      manage communications with the server.  *)

(** Protocol version.  Note that the protocol version is chosen in
   your freetds.conf file or via the environment variable TDSVER
   (which supersedes the configuration file).  See
   {{:http://www.freetds.org/userguide/choosingtdsprotocol.htm}Choosing
   a TDS protocol version} to help you choose.  Be warned that 4.x
   versions of the protcol do not allow empty strings which will be
   returned as strings of length 1.  *)
type version =
  | V42 (** Works with all products, subject to limitations.  *)
  | V46
  | V70 (** Includes support for the extended datatypes in SQL Server
           7.0 (such as char/varchar fields of more than 255
           characters), and support for Unicode. *)
  | V71 (** Include support for bigint (64 bit integers), variant and
           collation on all fields.  Collation is not widely used.  *)
  | V72 (** Includes support for varchar(max), varbinary(max), xml
           datatypes and MARS. *)
  | V73 (** Includes support for time, date, datetime2, datetimeoffset. *)
  | V74 (** Includes support for session recovery. *)

val connect : ?user:string -> ?password:string ->
              ?charset:string -> ?language:string -> ?application: string ->
              ?version: version ->
              string -> dbprocess
(** [connect server]: open a connection to the given database server.

    @param charset The name of the character set the client will use.
    Default values include "iso_1" for ISO-8859-1 (most platforms),
    "cp850" for Code Page 850 (IBM RS/6000), and "roman8" for the
    Roman8 character set (HP platforms).  Note that, if the chosen
    charset is not completely compatible with the one of the database,
    [Error(CONVERSION, msg)] will be raised with [msg] being something
    like "Some character(s) could not be converted into client’s
    character set. Unconverted bytes were changed to question marks
    (‘?’)".  Set an {!err_handler} to tread these errors differently.

    @param application If given, the server uses this name in its
    sysprocesses table to help identify your process. If you set the
    application name, you will see it if you query the sysprocesses
    table in the master database.

    @param language The name of the national language to use.  If
    language support is installed in the server, error messages are
    returned in the designated national language.  Set this only if
    you do not wish to use the server's default national language.

    @param version protocol version for this connection.  Be warned
    that, even if set, this is superseded by the value of the
    environment variable TDSVER.

    @raise Dblib.Error if the connection to the database could not be
    established.  Note that if [server] cannot be converted to an IP
    address, the error message "Server name not found in configuration
    files" is returned in {!Error}. *)

val close : dbprocess -> unit
(** [close conn] close the connection [conn] to the server. *)

val use : dbprocess -> string -> unit
(** [use conn name] change the current database to [name].
    @raise Dblib.Error if the database cannot be used. *)

val name : dbprocess -> string
(** [name conn] returns the name of the current database. *)

(************************************************************************)
(** {2 Executing SQL queries and getting the results} *)

val sqlexec : dbprocess -> string -> unit
(** Send the SQL command to the server and wait for an answer.
   @raise Dblib.Error if the SQL query is incorrect or another problem
   occurs.

   {b Warning}: There is one absolutely crucial, inflexible,
   unalterable requirement: the application must process all rows
   produced by the query. Before the [dbprocess] can be used for
   another query, the application must either fetch all rows, or
   cancel the results and receive an acknowledgement from the
   server. *)

val cancel :  dbprocess -> unit
(** Cancel the current command batch.  *)

val canquery :  dbprocess -> unit
(** Cancel the query currently being retrieved, (retriving and)
   discarding all pending rows. *)

val results : dbprocess -> bool
(** [results conn] returns [true] if some results are available and
   [false] if the query produced no results.  There may be several
   results if COMPUTE clauses are used.  One MUST CALL this function
   before trying to retrieve any rows.

   @raise Dblib.Error if the query was not processed successfully by
   the server. *)

val numcols : dbprocess -> int
(** Return number of regular columns in a result set.  *)

val colname : dbprocess -> int -> string
(** [colname conn c] returns the name of a regular result column [c].
   The first column has number 1.
   @raise Invalid_argument if the column is not in range.  *)

type col_type =
  | SYBCHAR | SYBVARCHAR
  | SYBINTN | SYBINT1 | SYBINT2 | SYBINT4 | SYBINT8
  | SYBFLT8 | SYBFLTN
  | SYBNUMERIC
  | SYBDECIMAL
  | SYBDATETIME | SYBDATETIME4  | SYBDATETIMN
  | SYBBIT
  | SYBTEXT
  | SYBIMAGE
  | SYBMONEY4 | SYBMONEY | SYBMONEYN
  | SYBREAL
  | SYBBINARY | SYBVARBINARY

val string_of_col_type : col_type -> string
(** Returns a string description of the column type. *)

val coltype : dbprocess -> int -> col_type
(** Get the datatype of a regular result set column.
   @raise Invalid_argument if the column does not exists. *)

type data =
  | NULL
  | STRING of string
  | TINY of int
  | SMALL of int
  | INT of int
  | INT32 of int32
  | INT64 of int64
  | FLOAT of float
  | DATETIME of int * int * int * int * int * int * int * int
      (** (year, month, day, hour, minute, second, millisecond, zone) *)
  | MONEY of float
  | BIT of bool
  | BINARY of string
  | NUMERIC of string
  | DECIMAL of string

val string_of_data : data -> string

val nextrow : dbprocess -> data list
(** Retrieve the next row.
   @raise Not_found if no more rows are available. *)

val count : dbprocess -> int
(** Get count of rows processed.
    - for insert/update/delete, count of rows affected.
    - for select, count of rows returned, after all rows have been
      fetched.    *)

(************************************************************************)
(** {2 Error handling} *)

type severity =
  | INFO        (** Informational, non-error. *)
  | USER        (** User error. *)
  | NONFATAL    (** Non-fatal error. *)
  | CONVERSION  (** Error in DB-Library data conversion. *)
  | SERVER      (** The Server has returned an error flag. *)
  | TIME        (** We have exceeded our timeout period while waiting
                   for a response from the Server—the [dbprocess] is
                   still alive. *)
  | PROGRAM     (** Coding error in user program. *)
  | RESOURCE    (** Running out of resources—the [dbprocess] may be dead. *)
  | COMM        (** Failure in communication with Server—the [dbprocess]
                   is dead. *)
  | FATAL       (** Fatal error—the [dbprocess] is dead. *)
  | CONSISTENCY (** Internal software error—please open an issue. *)

exception Error of severity * string
(** [Error(severity, message)] is raised on dblib errors.  You
    can change the reaction to some errors by installing your own
    handler with {!err_handler}. *)

val err_handler : (severity -> int -> string -> unit) -> unit
(** [err_handler f] installs [f] as the default error handler for
    non-OS related errors and errors not coming from the server.  [f]
    is given the {!severity}, the number of the error (see the 400 or
    so error numbers sybdb.h, macros SYBE...) and the message. *)

val msg_handler : (severity -> int -> string -> unit) -> unit
(** [msg_handler f] installs [f] as the default message handler for
    messages coming from the server. [f] is given the {!severity}, the line
    number, and the message. *)

val settime : int -> unit
(** [settime seconds] Set the number of seconds that DB-Library will
    wait for a server response to a SQL command.  If not response is
    obtained within that period, raise [Error(TIME, _)]. *)


;;
