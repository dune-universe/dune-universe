type point = Ctypes.point

type t = Ctypes.matrix = {
  mutable xx : float;
  mutable yx : float;
  mutable xy : float;
  mutable yy : float;
  mutable x0 : float;
  mutable y0 : float;
}

val scale : float -> t

val rotation : float -> t

val xscaled : float -> t

val yscaled : float -> t

val slanted : float -> t

val translation : point -> t

val zscaled : point -> t

val reflect : point -> point -> t

val rotate_around : point -> float -> t

val identity : t

val multiply : t -> t -> t

val xy_translation : float -> float -> t

val remove_translation : t -> t

val linear : float -> float -> float -> float -> t

val print : Format.formatter -> t -> unit
