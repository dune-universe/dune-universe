(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

(** *)

(** Module of comparison operators *)
module type Order =
sig
  type t

  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool

  val min : t -> t -> t
  val max : t -> t -> t
end

(** Module of operators, it defines the usual arithmetic operations. *)
module type OpS =
sig

  type t
  type elt
  (** Type of values: this is the type that the user should use with [make]
      and that will be returned by [get] *)

  type scalar
  (** Type of scalars *)
  (* (t, +, .) should be a commutative S-module where S is the set of scalars
     where + : t -> t -> t and . : t -> scalar -> t. *)

  val copy : t -> t
  val deepcopy : t -> t

  (** {1 Constructors} *)

  val create : unit -> t
  (* Create an arbitrary value from nothing *)

  val make : elt -> t
  (** Wrap a user-provided value *)

  val integer : int -> t
  (** Wrap an integer *)

  val zero : unit -> t
  (** Construct a fresh value corresponding to 0 *)

  val one : unit -> t
  (** Construct a fresh value corresponding to 1 *)

  val two : unit -> t
  (** Construct a fresh value corresponding to 2 *)

  (** {1 Destructors} *)

  val get : t -> elt
  (** Unwrap a value *)

  val ( !! ) : t -> elt
  (** Alias for [get] *)

  val to_string : t -> string

  val string_of_scalar : scalar -> string

  val string_of_elt : elt -> string

  (** {1 Arithmetic operators} *)

  val ( ~+ ) : t -> t
  (** unary plus (with copy) *)

  val ( ~- ) : t -> t
  (** unary minus (with copy) *)

  val ( + ) : t -> t -> t
  val ( += ) : t -> t -> t

  val ( - ) : t -> t -> t
  val ( -= ) : t -> t -> t

  val ( * ) : t -> t -> t
  val ( *= ) : t -> t -> t

  val ( / ) : t -> t -> t
  val ( /= ) : t -> t -> t

  val ( ** ) : t -> t -> t

  val inv : t -> t
  val sqr : t -> t

  val sqrt : t -> t
  val log : t -> t
  val exp : t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan : t -> t

  (** {1 Scalar operators} *)

  val scale : t -> scalar -> t
  (** Multiplication between a value and a scalar *)
  
  val translate : t -> scalar -> t
  (** Addition between a value and a scalar *)

  (** {1 Comparison operators} *)

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

(** Extends {!OpS} with comparison operators *)
module type OrderedOpS = sig
  include OpS
  include Order with type t := t
end
