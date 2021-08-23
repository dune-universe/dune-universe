(** Basic algebraic structures. The [_no_std] variants do not require
    defining the contents of [Intf_std.Std]. *)

open Intf_std

(** Abelian group *)
module type Abelian_group = sig
  type t

  val zero : t

  val add : t -> t -> t

  val neg : t -> t
end

module type Abelian_group_std = sig
  include Abelian_group

  include Std with type t := t
end

(** Monoid *)
module type Monoid = sig
  type t

  val one : t

  val mul : t -> t -> t
end

module type Monoid_std = sig
  include Monoid

  include Std with type t := t
end

(** Ring *)
module type Ring = sig
  include Abelian_group

  include Monoid with type t := t

  (* [sub x y] = [add x (neg y)] *)
  val sub : t -> t -> t

  val of_int : int -> t
end

module type Ring_std = sig
  include Ring

  include Std with type t := t
end

(** Field, assuming the ring is commutative. *)
module type Field = sig
  include Ring

  val div : t -> t -> t
end

(** Field, assuming the ring is commutative. *)
module type Field_std = sig
  include Field

  include Std with type t := t
end

(** Modules over a ring [R]. *)
module type Module = sig
  module R : Ring

  type t

  include Abelian_group with type t := t

  val smul : R.t -> t -> t
end

(** Module over a ring [R]. *)
module type Module_std = sig
  module R : Ring_std

  type t

  include Abelian_group_std with type t := t

  val smul : R.t -> t -> t
end

(** Algebra over a ring *)
module type Algebra = sig
  include Module

  (** [mul] should distribute over addition. Not necessarily
      associative, nor commutative, nor unital. *)
  val mul : t -> t -> t
end

(** Free module *)
module type Free_module = sig
  include Module

  type basis

  (** "Dirac" delta *)
  val delta : basis -> t

  (** Project the coefficient corresponding to a basis vector. *)
  val eval : t -> basis -> R.t

  (** [bind] = canonical, "multilinear" extension *)
  val bind : t -> (basis -> t) -> t

  (** [of_list [(x1,r1);...;(xn,rn)]] is equivalent to
       add (smul r1 (delta x1))
        (add (smul r2 (delta x2))
           (add ... (smul rn (delta xn)))) *)
  val of_list : (basis * R.t) list -> t

  (** Fold over the elements of a vector. Order is not specified. *)
  val fold : (basis -> R.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type Free_module_std = sig
  include Module_std

  module Basis : Std

  type basis = Basis.t

  (** "Dirac" delta *)
  val delta : basis -> t

  (** Project the coefficient corresponding to a basis vector. *)
  val eval : t -> basis -> R.t

  (** [bind] = canonical, "multilinear" extension *)
  val bind : t -> (basis -> t) -> t

  (** [of_list [(x1,r1);...;(xn,rn)]] is equivalent to
       add (smul r1 (delta x1))
        (add (smul r2 (delta x2))
           (add ... (smul rn (delta xn)))) *)
  val of_list : (basis * R.t) list -> t

  (** Fold over the elements of a vector. Order is not specified. *)
  val fold : (basis -> R.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type Finitely_generated_free_module = sig
  include Free_module

  val generators : basis list

  (** List coefficients in the order of [generators] *)
  val coefficients : t -> R.t list
end

(** Partial order. *)
module type Partial_order = sig
  type t

  val equal : t -> t -> bool

  val lt : t -> t -> bool
end
