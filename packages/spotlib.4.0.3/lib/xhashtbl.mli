val replace_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list -> unit

val of_list      : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
(** Double bindings can be found by [Hashtbl.find_all] *)

val to_list      : ('a, 'b) Hashtbl.t -> ('a * 'b) list

val find_opt     : ('a, 'b) Hashtbl.t -> 'a -> 'b option
val find_default : 'b -> ('a, 'b) Hashtbl.t -> 'a -> 'b
val find_or_add  : ('a -> 'b) -> ('a, 'b) Hashtbl.t -> 'a -> 'b
val alter        : ('a, 'b) Hashtbl.t -> 'a -> ('b option -> 'b option) -> unit

val concat : ('a, 'b) Hashtbl.t list -> ('a, 'b) Hashtbl.t
(** Keys bound more than once in the tables are multiply bound in the result.
    The order of adding key-value pair is the order of the table list.
*)

val create_with : int -> (('a, 'b) Hashtbl.t -> unit) -> ('a, 'b) Hashtbl.t
(** Same as [create] but with the initialization function *)
