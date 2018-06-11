(** Binding to CRlibm, a library of proved correctly-rounded
   mathematical functions.

   Version: 0.2
 *)


(** Common functions to the four rounding modules below.

   In order for the interface to be light, the functions with standard
   mathematical names are not documented. *)
module type S = sig
  val exp : float -> float

  val expm1 : float -> float
  (** [expm1 x] returns [exp x -. 1.] in a way that is accurate even
     for values of [x] near zero. *)

  val log : float -> float

  val log1p : float -> float
  (** [log1p x] returns [log(x +. 1.)] in a way that is accurate even
     for values of [x] near zero. *)

  val cos : float -> float

  val sin : float -> float

  val tan : float -> float

  val cospi : float -> float
  (** [cospi x] returns [cos(π·x)]. *)

  val sinpi : float -> float
  (** [sinpi x] returns [sin(π·x)]. *)

  val tanpi : float -> float
  (** [tanpi x] returns [tan(π·x)]. *)

  val asin : float -> float

  val acos : float -> float

  val atan : float -> float

  val asinpi : float -> float
  (** [asinpi x] returns [(asin x)/π] ∈ \[-0.5, 0.5\]. *)

  val acospi : float -> float
  (** [acospi x] returns [(acos x)/π] ∈ \[0., 1.\]. *)

  val atanpi : float -> float
  (** [atanpi x] returns [(atan x)/π] ∈ \[-0.5, 0.5\]. *)

  val cosh : float -> float

  val sinh : float -> float

  val log2 : float -> float

  val log10 : float -> float
end

(** {2 Round to the nearest}  *)

include S

val pow : float -> float -> float
(** [pow x y] computes [x] to the power [y].
    {b Beware} that this is not proved correctly rounded for all inputs. *)


(** {2 Round toward -∞}  *)

module Low : S

(** {2 Round toward +∞}  *)

module High : S

(** {2 Round toward zero}  *)

module Zero : S
