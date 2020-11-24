(**
   SQL columns data types.
*)
type sign = Positive | Negative

type sql = private
  | Null
  | Tinyint of int
  | Smallint of int
  | Int of int
  | Longint of Int64.t
  | Longlongint of Big_int.big_int
  | Decimal of Num.num
  | Date of (int * int * int) (* year, month, day *)
  | Time of (sign * int * int * int * Int64.t) (* sign * hour, min, sec, microsec *)
  | Datetime of ((int * int * int) * (int * int * int * Int64.t)) (* (year, month, day), (hour, min, sec, microsec) *)
  | Timestamp of ((int * int * int) * (int * int * int * Int64.t)) (* (year, month, day), (hour, min, sec, microsec) *)
  | Float of float
  | Double of float
  | Int24 of int
  | Year of int
  | Varchar of string
  | String of string
  | Varstring of string
  | Blob of Buffer.t (* TODO : add a Text type ? *)
  | Binary of Buffer.t
  | Varbinary of Buffer.t
  | Enum of string
  | Set of string
  | Bit of Bitstring.t
  | Geometry of Bitstring.t

type t = sql

exception Wrong_type of string

(** Build MySQL NULL value *)
val data_null : t

(** Build MySQL TINYINT value *)
val data_tinyint : int -> t

(** Build MySQL SMALLINT value *)
val data_smallint : int -> t

(** Build MySQL INT value *)
val data_int : int -> t

(** Build MySQL INT value *)
val data_longint : Int64.t -> t

(** Build MySQL BIGINT value *)
val data_longlongint : Big_int.big_int -> t

(** Build MySQL DECIMAL value *)
val data_decimal : Num.num -> t

(** Build MySQL DATE (year, month, day) value *)
val data_date : (int * int * int) -> t

(** Build MySQL TIME (sign, hour, min, sec, microsec) value *)
val data_time : (sign * int * int * int * Int64.t) -> t

(** Build MySQL DATETIME ((year, month, day), (hour, min, sec, microsec)) value *)
val data_datetime : ((int * int * int) * (int * int * int * Int64.t)) -> t

(** Build MySQL TIMESTAMP ((year, month, day), (hour, min, sec, microsec)) value *)
val data_timestamp : ((int * int * int) * (int * int * int * Int64.t)) -> t

(** Build MySQL FLOAT value *)
val data_float : float -> t

(** Build MySQL DOUBLE value *)
val data_double : float -> t

(** Build MySQL MEDIUM INT value *)
val data_int24 : int -> t

(** Build MySQL YEAR value *)
val data_year : int -> t

(** Build MySQL VARCHAR value *)
val data_varchar : string -> t

(** Build MySQL CHAR value *)
val data_string : string -> t

(** Build MySQL VARCHAR value *)
val data_varstring : string -> t

(** Build MySQL BLOB value *) (* TODO : add a Text type ? *)
val data_blob : Buffer.t -> t

(** Build MySQL BINARY value *)
val data_binary : Buffer.t -> t

(** Build MySQL VARBINARY value *)
val data_varbinary : Buffer.t -> t

(** Build MySQL ENUM value *)
val data_enum : string -> t

(** Build MySQL SET value *)
val data_set : string -> t

(** Build MySQL BIT value *)
val data_bit : Bitstring.t -> t

(** Build MySQL GEOMETRY value *)
val data_geometry : Bitstring.t -> t

(**
   Convert column data to OCaml value (int). NULL is converted into None.
   @raise Wrong_type if the column data is not of int type.
*)
val to_ocaml_int: t -> int option

(**
   Convert column data to OCaml value (Int64). NULL is converted into None.
   @raise Wrong_type if the column data is not of Int64 type.
*)
val to_ocaml_int64: t -> Int64.t option

(**
   Convert column data to OCaml value (Big_int). NULL is converted into None.
   @raise Wrong_type if the column data is not of Big_int type.
*)
val to_ocaml_big_int: t -> Big_int.big_int option

(**
   Convert column data to OCaml value (Num). NULL is converted into None.
   @raise Wrong_type if the column data is not of Num type.
*)
val to_ocaml_num: t -> Num.num option

(**
   Convert column data to OCaml value (date). NULL is converted into None.
   @raise Wrong_type if the column data is not of date type.
*)
val to_ocaml_date: t -> (int * int * int) option

(**
   Convert column data to OCaml value (time). NULL is converted into None.
   @raise Wrong_type if the column data is not of time type.
*)
val to_ocaml_time: t -> (sign * int * int * int * Int64.t) option

(**
   Convert column data to OCaml value (datetime/timestamp). NULL is converted into None.
   @raise Wrong_type if the column data is not of datetime/timestamp type.
*)
val to_ocaml_dt_ts: t -> ((int * int * int) * (int * int * int * Int64.t)) option

(**
   Convert column data to OCaml value (float). NULL is converted into None.
   @raise Wrong_type if the column data is not of float type.
*)
val to_ocaml_float: t -> float option

(**
   Convert column data to OCaml value (string). NULL is converted into None.
   @raise Wrong_type if the column data is not of string type.
*)
val to_ocaml_string: t -> string option

(**
   Convert column data to OCaml value (Buffer). NULL is converted into None.
   @raise Wrong_type if the column data is not of Buffer type.
*)
val to_ocaml_buffer: t -> Buffer.t option

(**
   Convert column data to OCaml value (Bitstring). NULL is converted into None.
   @raise Wrong_type if the column data is not of Bitstring type.
*)
val to_ocaml_bitstring: t -> Bitstring.t option

(**
   eq v1 v2 tests for structural equality of v1 and v2
*)
val eq : t -> t -> bool

(**
   Convert column type to string
*)
val type_to_string : t -> string

(**
   Convert column data to string
*)
val to_string : t -> string option
