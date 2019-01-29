(** A wrapper for Mssql data types.  Use this instead of format strings
    to protect yourself from SQL injection *)
open Core
open Freetds

type t =
  | Bignum of Bignum.t
  | Bool of bool
  | Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | String of string
  | Date of Time.t
[@@deriving sexp]

val of_data : month_offset:int -> Dblib.data -> t option

val to_string : t option -> string
val to_string_escaped : t option -> string

val bignum : ?column:string -> t -> Bignum.t
val float : ?column:string -> t -> float
val int : ?column:string -> t -> int
val int32 : ?column:string -> t -> int32
val int64 : ?column:string -> t -> int64
val bool : ?column:string -> t -> bool
val str : ?column:string -> t -> string
val date : ?column:string -> t -> Date.t
val datetime : ?column:string -> t -> Time.t
