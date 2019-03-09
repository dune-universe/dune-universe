
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Generic-Purpose table}

    This table is designed for holding relational data over scalar
    values (strings, intergers, bools, etc.).
    It trades type-safety for flexibility, and should be well-suited to
    dealing with CSV-encoded data.

    {b STATUS: EXPERIMENTAL} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a sequence = ('a -> unit) -> unit

(** {2 Scalar Value} *)

type data =
  | S of string
  | I of int
  | B of bool
  | F of float

module Data : sig
  type t = data

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val print : t printer
  val to_string : t -> string
end

val int : int -> data
val float : float -> data
val bool : bool -> data
val string : string -> data

(** {2 A row of values} *)

type row = data array

exception IndexError

module Row : sig
  type t = row

  val empty : t
  val make1 : data -> t
  val make2 : data -> data -> t

  val of_array : data array -> t
  val of_list : data list -> t

  val size : t -> int

  val get : int -> t -> data option

  val get_exn : int -> t -> data
  (** @raise IndexError if the index is not valid *)

  val map : f:(data -> data) -> t -> t

  val append : t -> t -> t
  val remove_index : int -> t -> t
  val remove_index_l : int list -> t -> t

  val print : t printer
  val to_string : t -> string
end

(** {2 A Table, that is, an extensible list of Rows} *)

exception DimError
(** Raised in case dimensions don't match *)

type t

val create : ?size:int -> names:string array -> unit -> t
(** [create ~names ()] creates a new table with columns
    labelled with [names] *)

val init : names:string array -> int -> (int -> row) -> t
(** [init ~names n f] makes a table with [size] rows,
    each initialized from [f]
    @raise DimError if some row returned by [f] doesn't have
    the same size as [names] *)

val make : names:string array -> int -> row -> t
(** [init ~names n row] makes a table with [size] rows, all equal to [row]
    @raise DimError if [Row.size row <> length names] *)

val num_rows : t -> int

val num_cols : t -> int

val size : t -> int
(** Alias to {!num_rows} *)

val names : t -> string array
(** Access the column names.
    Should not be modified *)

val get : int -> t -> row option
(** [get n tbl] gets the [n]-th row *)

val get_exn : int -> t -> row
(** @raise IndexError if the index is invalid *)

val get_cell : int -> int -> t -> data option
(** [get_cell i j tbl] gets the [j]-th value of the [i]-th row *)

val get_cell_exn : int -> int -> t -> data
(** @raise IndexError if the index is invalid *)

val fold : f:('acc -> row -> 'acc) -> x:'acc -> t -> 'acc

val iter : f:(row -> unit) -> t -> unit

val iteri : f:(int -> row -> unit) -> t -> unit

val push : t -> row -> unit
(** Push a row into the table
    @raise DimError if the row has not the same dimension as table *)

val push_l : t -> row list -> unit
(** @raise DimError if some row has not the same dimension as table *)

val push_seq : t -> row sequence -> unit
(** @raise DimError if some row has not the same dimension as table *)

val to_seq : t -> row sequence

val to_list : t -> row list

val to_list_rev : t -> row list

val print : t printer


