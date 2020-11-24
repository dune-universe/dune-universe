(** MySQL Protocol natively implements the MySQL client protocol
    (ie without any binding to C library).

    License:
    - The MySQL Protocol library uses the
     {{:http://www.gnu.org/licenses/lgpl.html} LGPL license version 3}.

    External dependencies:
    - {{:https://github.com/xguerin/bitstring/} Bitstring}.
    - {{:https://github.com/xavierleroy/cryptokit/} Cryptokit}.
    - optional: {{:https://github.com/gildor478/ounit/} oUnit } (to run the tests suite).

    Tested configurations:
    - MySQL 5.1.69 server (64 bits) - FreeBSD -stable.
    - MySQL 5.5.31 server (64 bits) - FreeBSD -stable.
    - MySQL 5.6.26 server (64 bits) - FreeBSD -stable.
    - MariaDB 10.5.6 server (64 bits) - FreeBSD -stable.

    The following functionalities are not implemented:
    - Master/slave.
    - Long data packet.
    - Compression.
    - Encryption.

    Known limitations:
    - On 32 bits platforms, the data retrieved from the server cannot exceed 16777211 bytes
    (the OCaml Sys.max_string_length value). So, for instance, you cannot retrieve a column
    with a binary BLOB data greater than this max size. And if you have a TEXT column encoded
    in UTF-8, as a character will take 3 to 4 bytes, you will only be able to retrieve between
    16777211/3=5592403 and 16777211/4=4194302 characters. On 64 bits platforms, the limit is
    so high that this limitation can be ignored.

    Usage examples:
    - See "examples" directory.

*)

(**
   MySQL error.
*)
type client_error = {
  client_error_errno : int; (** error number *)
  client_error_sqlstate : string; (** state *)
  client_error_message : string; (** error message *)
}

(**
   Raise if the MySQL server returns an error.
*)
exception Error of client_error

(**
   Raise if fetch is called and the server has no more rows to return.
*)
exception Fetch_no_more_rows

(**
   Client configuration.
*)
type configuration = {
  sockaddr : Unix.sockaddr; (** socket *)
  capabilities : Mp_capabilities.capabilities list; (** capabilities *)
  max_packet_size : Int64.t; (** max packet size *)
  charset_number : int; (** charset *)
  user : string; (** login *)
  password : string; (** password *)
  databasename : string; (** database name (can be empty) *)
}

(** 
    Client connection.
*)
type connection = {
  configuration : configuration; (** configuration *)
  mutable channel : (in_channel * out_channel) option; (** channel between client and server *)
  mutable handshake : Mp_handshake.handshake option; (** handshake answer from the server *)
}

(**
   DML (Data Manipulation Language) and DCL (Data Control Language) result.
   Result of INSERT, UPDATE, GRANT... statements.

   insert_id can be negative in two cases:
   - the auto_increment value is indeed negative (SIGNED field).
   - the returned value overflows Int64 maximum (BIGINT UNSIGNED field).

   Unfortunately, the protocol gives no way to differentiate these two
   cases (see {{:https://bugs.mysql.com/bug.php?id=69228} this bug report}).
   So we return two values:
   - the first one is a Int64 and must be used when the
     auto_increment is {b _not_} a BIGINT UNSIGNED field.
   - the second one is a Big_int and must be used when the
     auto_increment is a BIGINT UNSIGNED field.
*)
type dml_dcl_result = {
  affected_rows : Int64.t; (** number of affected rows *)
  insert_id : (Int64.t * Big_int.big_int); (** auto_increment id after an INSERT statement *)
  server_status : int; (** status *)
  warning_count : int; (** warning *)
  message : string; (** warning message *)
}

(**
   Result for a prepare command.
*)
type prepare_result = {
  prepare_handler : Int64.t;
  prepare_nb_columns : int;
  prepare_nb_parameters : int;
  prepare_warning_count : int;
  prepare_parameters_fields : Mp_field_packet.field_packet list;
  prepare_parameters_names : Mp_field.field_name list;
  prepare_columns_fields : Mp_field_packet.field_packet list;
  prepare_columns_names : Mp_field.field_name list;
}

type executable_statement

type execute_result

type result

(**
   Convert MySQL exception to string.
*)
val error_exception_to_string : client_error -> string

(**
   Convert DML (Data Manipulation Language) and DCL (Data Control Language) result to string.
*)
val dml_dcl_result_to_string : dml_dcl_result -> string 

(**
   Build client configuration.
   @param user Login.
   @param password Password.
   @param sockaddr Socket for the connection to the server.
   @param databasename Database name.
   @param max_packet_size Max client/server packet size.
   @param charset Charset and collation name.
   @param capabilities Client capabilities.
*)
val configuration :
  user:string
  -> password:string
  -> sockaddr:Unix.sockaddr
  -> ?databasename:string
  -> ?max_packet_size:Int64.t
  -> ?charset:Mp_charset.charset_name * Mp_charset.collation_name
  -> ?capabilities:Mp_capabilities.capabilities list
  -> unit
  -> configuration

(**
   Connection to the server.
   @param configuration Client configuration.
   @param force If true, the connection is immediately opened.
   Otherwise, the connection is opened only the first time it is needed.
*)
val connect :
  configuration:configuration
  -> ?force:bool
  -> unit
  -> connection

(**
  Change user / databasename / charset
  @param connection Connection.
  @param user Login.
  @param password Password.
  @param databasename Database name.
  @param charset Charset and collation name.
*)
val change_user :
  connection:connection
  -> user:string
  -> password:string
  -> ?databasename:string
  -> ?charset:Mp_charset.charset_name * Mp_charset.collation_name
  -> unit
  -> configuration

(**
   Reset the session : equivalent to a disconnect/reconnect with the same configuration.
   @param connection Connection.
*)
val reset_session :
  connection:connection
  -> unit

(**
  Reset the connection without re-authentication.
  From {{:https://mariadb.com/kb/en/com_reset_connection/} documentation}), this will:
  - rollback any open transaction
  - reset transaction isolation level
  - reset session variables
  - delete user variables
  - remove temporary tables
  - remove all PREPARE statement

  Database will NOT be reset to initial value.
  @param connection Connection.
*)
val reset_connection :
  connection:connection
  -> unit

(**
   Change current database.
   @param connection Connection.
   @param databasename Database name.
*)
val use_database :
  connection:connection
  -> databasename:string
  -> unit

(**
   Send a PING to the server.
   @param connection Connection.
*)
val ping :
  connection:connection
  -> unit

(**
   Create a new statement from a SQL string.
*)
val create_statement_from_string :
  string
  -> executable_statement

(**
   Prepare a statement.
   @param connection Connection.
   @param statement Executable statement.
*)
val prepare :
  connection:connection
  -> statement:executable_statement
  -> executable_statement

(**
   Extract the statement from an executable statement.
*)
val get_created_statement :
  executable_statement
  -> string

(**
   Extract the prepared statement from an executable statement.
*)
val get_prepared_statement :
  executable_statement
  -> (string * prepare_result)

(**
   Execute a statement (prepared or not prepared) and return the result.
   @param connection Client connection.
   @param statement Executable statement.
   @param filter Optional function applied to each row of the result. If it returns true, the row is kept in the return result. Otherwise, the row is discarded from the result.
   @param iter Optional function applied to each row of the result. If there is also a filter function, this filter is applied _before_ the iter function. So a row will be processed by the iter function only if this row has passed the filter function.
   @param return_all_raw_mysql_data If true, the returned result will include MySQL raw data. It's useful if you need for instance to get the MySQL columns types and options. The default is false.
   @param params List of params for prepared statement. Must be in the same order than in the prepared statement.
   @param bind Specify if the params must be bound into the prepared statement. The default is to bind. A statement must be at least executed once with bind. After that, if you need to execute again the same statement with the same params, you can use No_bind.
   @param flag Cursor options. To use fetch to get the statement results, you must specify the Cursor_type_read_only option.
*)
val execute :
  connection:connection
  -> statement:executable_statement
  -> ?filter:((string * int) list
              -> Mp_data.t list -> bool) option
  -> ?iter:((string * int) list
            -> Mp_data.t list -> unit) option
  -> ?return_all_raw_mysql_data:bool
  -> ?params:Mp_data.t list
  -> ?bind:Mp_execute.bind
  -> ?flag:Mp_execute.flag
  -> unit
  -> execute_result

(**
   Extract the result part from an executed result.
*)
val get_result :
  execute_result
  -> result

(**
   Extract the multiple part from an executed result (for CALL result).
*)
val get_result_multiple :
  execute_result
  -> result list

(**
   Extract the set part from a result (for SELECT result).
*)
val get_result_set :
  result
  -> Mp_result_set_packet.result_select

(**
   Extract the ok part from a result (for INSERT, UPDATE, GRANT... result).
*)
val get_result_ok :
  result
  -> dml_dcl_result

(**
   Fetch row(s) from an executed result. It must be a prepared statement executed with a cursor.
   @param connection Client connection.
   @param statement Executable statement.
   @param nb_rows Number of row(s) to fetch. Default is 1.
   @param filter Optional function applied to each fetched rows. If it returns true, the row is kept. Otherwise, the row is discarded.
   @param iter Optional function applied to each fetched rows. If there is also a filter function, this filter is applied _before_ the iter function. So a row will be processed by the iter function only if this row has passed the filter function.
   @param return_all_raw_mysql_data If true, the fetch will include MySQL raw data. It's useful if you need for instance to get the MySQL columns types and options. The default is false.
*)
val fetch :
  connection:connection
  -> statement:execute_result
  -> ?nb_rows:int64
  -> ?filter:((string * int) list
              -> Mp_data.t list -> bool) option
  -> ?iter:((string * int) list
            -> Mp_data.t list -> unit) option
  -> ?return_all_raw_mysql_data:bool
  -> unit
  -> result

(**
   Extract the set part from a fetch.
*)
val get_fetch_result_set :
  result
  -> Mp_result_set_packet.result_select

(**
   Close and destroy the prepared statement. It will be unusable.
   @param connection Client connection.
   @param statement Executable statement.
*)
val close_statement :
  connection:connection
  -> statement:executable_statement
  -> unit

(**
   Close the connection to the server.
   @param connection Client connection.
*)
val disconnect :
  connection:connection
  -> unit
