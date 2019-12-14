open Ppxlib

(** Type for located errors *)
type t

(** Return the message for the given located error *)
val msg : t -> string

(** Return a located error with the given loc and error message wrapped in a result's [Error].
    @param loc the precise location of the unhandled part of the type declaration AST node.
    Should be more accurate than the [loc] provided by ppxlib to the generator function.
    @param msg the message to display when raising this error 
*)
val as_result : loc: Location.t -> msg: string -> ('a, t) result

(** Raise the given located error using [Raise.errorf] *)
val raise_ : t -> 'a

(** Return the value or raise the error from the given result *)
val ok_or_raise : ('a, t) result -> 'a
