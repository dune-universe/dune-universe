open Typerep_lib.Std

type obj = Obj.t [@@deriving typerep ]

exception Ill_typed of Typerep.packed * Obj.t * string
exception Not_supported of Typerep.packed * Obj.t * string
    
val tag_check : sharing:bool -> 'a Typerep.t -> Obj.t -> unit
(** [tag_check ~sharing (tyrep : t Typerep.t) o] checks whether [o] is safely
    used as a value of [t].  If the check fails it raises [Ill_typed].
    If it finds non supported data such as closures, it raises [Not_supported]. 

    If [sharing=true], it checks sharing and cycles. Generally it makes 
    [tag_check] VERY slow.

    If [sharing=false], [tag_check] hangs with cyclic data.

    Limitations and Todos:

    - The input [Obj.t] value must be created by proper funcitons such as 
      [Obj.repr], [output_value] and [Marshal.to_channel], etc..
    - Things are not really tail-recursive. It may stack-overflow for deep data.
    - Closures (functions, objects, lazy thunks) are not supported.
    - The target types are limited to what Typerep can handle: for example,
      numbers of tuple components and variant arguments are limited upto 5.
*)

val obj : sharing:bool -> 'a Typerep.t -> Obj.t -> 'a
(** Type safe version of [Obj.obj] using [tag_check].
    The same limitations of [tag_check] apply.
*)
