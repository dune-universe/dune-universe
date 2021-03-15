open Geometry

(* backend functions required to be able to draw abstract elements *)
module type T = sig
  type color

  (* backend specific function to be called at the end of execution *)
  val ending : unit -> unit

  val width : unit -> float

  val height : unit -> float

  val normalize : range -> range -> point -> point

  val rgb : int -> int -> int -> color

  val draw_text :
    color -> [< `Center | `Left | `Right] -> point -> string -> unit

  val draw_line : dashed:bool -> color -> point -> point -> unit

  val draw_circle : color -> point -> float -> unit

  val fill_circle : color -> point -> float -> unit

  val draw_poly : color -> point list -> unit

  val fill_poly : color -> point list -> unit
end
