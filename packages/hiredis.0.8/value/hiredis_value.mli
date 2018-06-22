(** Hiredis_value provides access to the RESP value type *)

type t =
    | Nil
    | Error of string
    | Integer of int64
    | String of string
    | Array of t array
    | Status of string

(** Get a nil value *)
val nil : t

(** Convert an OCaml string to a Hiredis string *)
val string : string -> t

(** Convert an int64 to Hiredis number *)
val int64 : int64 -> t

(** Convert an int to Hiredis number *)
val int : int -> t

(** Convert an OCaml array to Hiredis array *)
val array : t array -> t

(** Create an error value *)
val error : string -> t

(** Create a status or "simple string" value *)
val status : string -> t

exception Invalid_value

val is_nil : t -> bool
val is_error : t -> bool

(** [to_string v] converts [v] to a string, otherwise [Invalid_value] is raised *)
val to_string : t -> string

(** [to_int64 v] converts [v] to an int64, otherwise [Invalid_value] is raised *)
val to_int64 : t -> int64

(** [to_int v] converts [v] to an int, otherwise [Invalid_value] is raised *)
val to_int : t -> int

(** [to_float v] converts [v] to a float, otherwise [Invalid_value] is raised *)
val to_float : t -> float

(** [to_array v] converts [v] to an array of values if [v] is an array value,
 *  otherwise [Invalid_value] is raised *)
val to_array : t -> t array

(** [to_list v] converts [v] to a list of values if [v] is an array value,
 *  otherwise [Invalid_value] is raised *)
val to_list : t -> t list

(** [to_hashtbl v] converts [v] to a Hashtbl.t if [v] is an array value and
 *  the array can be interpreted as a hash table, otherwise [Invalid_value] is
 *  raised*)
val to_hashtbl : t -> (string, t) Hashtbl.t

