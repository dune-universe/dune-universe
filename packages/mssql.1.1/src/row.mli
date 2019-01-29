(** A row of data accessible by column name with helper functions for
    conversions. *)
open Core
open Freetds

type t [@@deriving sexp_of]

val create_exn : month_offset:int -> Dblib.data list -> string list -> t

(** [to_alist t] converts t to a list of (colname, value) pairs *)
val to_alist : t -> (string * string) list

(** [bignum colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column can't be converted
    to a bignum or wasn't in the query. *)
val bignum : t -> string -> Bignum.t option

(** [bignum_exn colname] similar to [bignum] but asserts that the column
    named [colname] exists and the data is not null. *)
val bignum_exn : t -> string -> Bignum.t

(** [bool colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column can't be converted
    to a bool or wasn't in the query. *)
val bool : t -> string -> bool option

(** [bool_exn colname] similar to [bool] but asserts that the column
    named [colname] exists and the data is not null. *)
val bool_exn : t -> string -> bool

(** [float colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column can't be converted
    to a float or wasn't in the query.  *)
val float : t -> string -> float option

(** [float_exn colname] similar to [float] but asserts that the column
    named [colname] exists and the data is not null. *)
val float_exn : t -> string -> float

(** [int colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column can't be converted
    to an int or wasn't in the query.  *)
val int : t -> string -> int option

(** [int_exn colname] similar to [int] but asserts that the column
    named [colname] exists and the data is not null. *)
val int_exn : t -> string -> int

(** [int32 colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column can't be converted
    to an int32 or wasn't in the query.  *)
val int32 : t -> string -> int32 option

(** [int32_exn colname] similar to [int32] but asserts that the column
    named [colname] exists and the data is not null. *)
val int32_exn : t -> string -> int32

(** [int64 colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column can't be converted
    to an int64 or wasn't in the query.  *)
val int64 : t -> string -> int64 option

(** [int64_exn colname] similar to [int64] but asserts that the column
    named [colname] exists and the data is not null. *)
val int64_exn : t -> string -> int64

(** [str colname] returns the value of the given column, or None if the
    column is null. Throws an exception if the column wasn't in the query. *)
val str : t -> string -> string option

(** [str_exn colname] similar to [str] but asserts that the column
    named [colname] exists and the data is not null. *)
val str_exn : t -> string -> string

(** [date colname] returns the value of the given column, or None if
    the column is null. Throws an exception if the column can't be converted
    to a bool or wasn't in the query. *)
val date : t -> string -> Date.t option

(** [date_exn colname] similar to [date] but asserts that the column named
    [colname] exists and the data is not null. *)
val date_exn : t -> string -> Date.t

(** [datetime colname] returns the value of the given column, or None if
    the column is null. Throws an exception if the column can't be converted
    to a bool or wasn't in the query. *)
val datetime : t -> string -> Time.t option

(** [datetime_exn colname] similar to [datetime] but asserts that the column
    named [colname] exists and the data is not null. *)
val datetime_exn : t -> string -> Time.t
