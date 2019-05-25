
(** Boolean algebra  *)
module type BA = sig
  type t
  val ctrue : t
  val cfalse : t
  val conj : t -> t -> t
  val disj : t -> t -> t
  val neg : t -> t -> t
end

(** Kleene algebra  *)
module type KA = sig
  type t
  val zero : t
  val one : t
  val seq : t -> t -> t
  val union : t -> t -> t
  val star : t -> t
end

(** Kleene algebra with tests  *)
module type KAT = sig
  type b
  type t
  val test : b -> t
  include BA with type t := b
  include KA with type t := t
end

(** Guarded Kleene algebra with tests *)
module type GKAT = sig
  type b
  type t
  val test : b -> t
  include BA with type t := b
  val zero : t
  val one : t
  val seq : t -> t -> t
  val ite : b -> t -> t
  val whl : b -> t
end
