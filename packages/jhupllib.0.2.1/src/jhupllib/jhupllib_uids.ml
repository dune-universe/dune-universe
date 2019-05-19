open Jhupllib_pp_utils;;

module type Uid_module =
sig
  type t
  type context
  val default_context : context
  val new_context : unit -> context
  val fresh : ?context:context -> unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t pretty_printer
  val show : t -> string
end;;

module Make () : Uid_module =
struct
  type t = int [@@deriving eq, ord, show]
  type context =
    { next_uid : int ref }
  let new_context () =
    { next_uid = ref 0 }
  ;;
  let default_context = new_context ();;
  let fresh ?context:(context=default_context) () =
    let x = !(context.next_uid) in
    (context.next_uid) := x + 1;
    x
  ;;
end;;
