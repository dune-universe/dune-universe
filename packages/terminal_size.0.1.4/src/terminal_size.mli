(** Get the dimensions of the terminal *)

(** Return the number of rows that can be displayed *)
val get_rows : unit -> int option

(** Return the number of columns that can be displayed *)
val get_columns : unit -> int option
