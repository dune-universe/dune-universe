(** Module for the general Mssql exception type [Mssql_error]. This lets us
    raise nice exception that always have their source code position, and
    optionally contain a query string, parameters, formatted query string, and
    result rows *)
open Core_kernel

type t =
  { msg : string
  ; reraised_exn : Exn.t option
  ; here : Source_code_position.t
  ; query : string option
  ; params : Db_field.t option list
  ; formatted_query : string option
  ; results : Row.t list list }
[@@deriving sexp_of]

exception Mssql_error of t
[@@deriving sexp_of]

(** [failwith [%here] msg] raises a [Mssql_error] with the given options and
    message *)
val failwith
  : ?query:string
  -> ?params:Db_field.t option list
  -> ?formatted_query:string
  -> ?results:Row.t list list
  -> ?exn:Exn.t
  -> ?backtrace:Caml.Printexc.raw_backtrace
  -> Source_code_position.t
  -> string
  -> _

(** [failwithf [%here] "%..." msg] raises a [Mssql_error] with the given
    options and a sprintf formatted message *)
val failwithf
  : ?query:string
  -> ?params:Db_field.t option list
  -> ?formatted_query:string
  -> ?results:Row.t list list
  -> ?exn:Exn.t
  -> ?backtrace:Caml.Printexc.raw_backtrace
  -> Source_code_position.t
  -> ('a, unit, string, _) format4
  -> 'a

(** [with_wrap_dblib_error [%here] f] calls [f] and if it throws an exception,
    wraps the error in [Mssql_error]. There are special cases for if the exn
    thrown is already [Mssql_error] (we always preserve the [%here] and preserve
    the other info if not set), and for [Dblib.Error] (using the message
    directly, to make exceptions more readable -- although we also set
    [reraised_exn] so the original info is all there) *)
val with_wrap
  : ?query:string
  -> ?params:Db_field.t option list
  -> ?formatted_query:string
  -> ?results:Row.t list list
  -> Source_code_position.t
  -> (unit -> 'a)
  -> 'a
