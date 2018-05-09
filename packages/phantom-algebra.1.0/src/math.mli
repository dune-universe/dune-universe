(** Usual mathematical operations extended to tensors

    As general rules these functions operates element-wise.
*)

open Core

(** {1 Trignonometric functions} *)
val cos: ('dim,'rank) t -> ('dim,'rank) t
val sin: ('dim,'rank) t -> ('dim,'rank) t
val tan: ('dim,'rank) t -> ('dim,'rank) t

val acos: ('dim,'rank) t -> ('dim,'rank) t
val asin: ('dim,'rank) t -> ('dim,'rank) t
val atan: ('dim,'rank) t -> ('dim,'rank) t

(** {1 Hyperbolic functions} *)

val cosh: ('dim,'rank) t -> ('dim,'rank) t
val sinh: ('dim,'rank) t -> ('dim,'rank) t
val tanh: ('dim,'rank) t -> ('dim,'rank) t
val atanh: ('dim,'rank) t -> ('dim,'rank) t

(** {1 Exponential and logarithm } *)

val log: ('dim,'rank) t -> ('dim,'rank) t
val expm1 : ('dim,'rank) t -> ('dim,'rank) t

(** {1 Order functions}  *)

val max: ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t
val min: ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t

(** {1 Truncation function } *)

val ceil: ('dim,'rank) t -> ('dim,'rank) t
val floor: ('dim,'rank) t -> ('dim,'rank) t

(** {1 Bit manipulation} *)

val copysign: ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t
val ldexp: ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t

(** {1 Miscellaneous } *)

val sqrt: ('dim,'rank) t -> ('dim,'rank) t
val fmod: ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t
