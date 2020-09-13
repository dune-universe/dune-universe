(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2019  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The actual implementation of these API functions are given in
   primitives.ml as Michelson code.
*)

type ocaml_int = int
(** OCaml's int *)

type 'a const = 'a [@@deriving typerep]
(** To emphasize SCaml only accepts constants *)

type nat = Nat of ocaml_int const [@@deriving typerep]
(** Arbitrary length nat.

    [Nat] only takes a constant.
    Currently there is no way to write a literal beyond the range of OCaml's [int].
*)

type int = Int of ocaml_int const [@@deriving typerep]
(** Arbitrary length int

    [Int] only takes a constant.
    Currently there is no way to write a literal beyond the range of OCaml's [int].
*)

type tz = Tz of float const [@@deriving typerep]
(** Tezzies.  The smallest unit is micro tz, [Tz 0.000001].

    [Tz] only takes a constant.
*)

module Option : sig
  type 'a t = 'a option = None | Some of 'a [@@deriving typerep]

  val value : 'a option -> 'a -> 'a
  (** [value (Some x) def = x]
      [value None def = def]
  *)

  val get : 'a option -> 'a
  (** [get (Some x) = x]
      [get None -> error ]
  *)
end

type ('a, 'b) sum = Left of 'a | Right of 'b [@@deriving typerep]
(** Basic sum type corresponds with Michelson's [or] type. *)

module Sum : sig
  type ('a, 'b) t = ('a, 'b) sum = Left of 'a | Right of 'b [@@deriving typerep]

  val get_left : ('a, 'b) sum -> 'a
  (** [get_left (Left x) = x]
      [get_left (Right x) -> error]
  *)

  val get_right : ('a, 'b) sum -> 'b
  (** [get_right (Right x) = x]
      [get_right (Left x) -> error]
  *)
end


(** Arithmetics *)

val (+) : int -> int -> int
val (+^) : nat -> nat -> nat
val (+$) : tz -> tz -> tz

val (-) : int -> int -> int
val (-^) : nat -> nat -> int
val (-$) : tz -> tz -> tz

val ( * ) : int -> int -> int
val ( *^ ) : nat -> nat -> nat
val ( *$ ) : tz -> nat -> tz

val (~-) : int -> int
val (~-^) : nat -> int

val ediv_int_int : int -> int -> (int * nat) option
val ediv_int_nat : int -> nat -> (int * nat) option
val ediv_nat_int : nat -> int -> (int * nat) option
val ediv_nat_nat : nat -> nat -> (nat * nat) option
val ediv_tz_tz : tz -> tz -> (nat * tz) option
val ediv_tz_nat : tz -> nat -> (tz * tz) option

val (/) : int -> int -> int
(** Fail with [Int 0] if the divisor is [Int 0] *)

val (/^) : nat -> nat -> nat
(** Fail with [Nat 0] if the divisor is [Nat 0] *)

val (/$) : tz -> tz -> nat
(** Fail with [Tz 0.] if the divisor is [Tz 0.] *)

val (/$^) : tz -> nat -> tz
(** Fail with [Nat 0] if the divisor is [Nat 0] *)

val (lsl) : nat -> nat -> nat
val (lsr) : nat -> nat -> nat
val (lor) : nat -> nat -> nat
val (land) : nat -> nat -> nat
val land_int_nat : int -> nat -> nat (* not a binop *)
val (lxor) : nat -> nat -> nat
val lnot_nat : nat -> int (* not a binop *)
val lnot : int -> int (* not a binop *)

val abs : int -> nat
val isnat : int -> nat option

(** Comparisons

    They are fully polymorphic but only work for limited set
    of Michelson types.
*)

val compare : 'a -> 'a -> int
val (=)  : 'a -> 'a -> bool
val (<>) : 'a -> 'a -> bool
val (<)  : 'a -> 'a -> bool
val (<=) : 'a -> 'a -> bool
val (>)  : 'a -> 'a -> bool
val (>=) : 'a -> 'a -> bool

(** Logical operators *)

val (&&) : bool -> bool -> bool
val (||) : bool -> bool -> bool
val xor : bool -> bool -> bool
val not : bool -> bool

(** Tuples *)

val fst : ('a * 'b) -> 'a
val snd : ('a * 'b) -> 'b

(** Errors *)

val failwith : 'a -> 'b
(** Fail the execution of the smart contract.

    You can use [assert b] to fail the execution conditionally.
    [assert b] fails if and only if [b] is evaluated to [false].
*)

val raise : exn -> 'a
(** Another version of [failwith] with a better interface for OCaml simualtion *)

(** Loops *)
module Loop : sig
  val left : ('a -> ('a, 'b) sum) -> 'a -> 'b
  (** Keep calling the given function over values of type ['a]
      while the function returns [Left a].

      The loop stops when the function returns [Right b] and returns [b].
  *)
end

(** Data types *)

(** Lists *)
module List : sig
  type 'a t = 'a list [@@deriving typerep]
  val length : 'a t -> nat
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc

  val fold_left' : ('acc * 'a -> 'acc) -> 'acc -> 'a list -> 'acc
  (** A variant of [fold_left] which takes an uncurried function.
      This is useful when the curried function is rejected because of
      the unpackable type of ['acc].
  *)

  val rev : 'a t -> 'a t
  val rev_append : 'a t -> 'a t -> 'a t
end

(** Sets

    Set literal can be written using [Set [ x; .. ; x ]] expression.
*)
type 'a set = Set of 'a const list const [@@deriving typerep]

module Set : sig
  type 'a t = 'a set [@@deriving typerep]
  val empty : 'a t
  val length : 'a t -> nat
  val mem : 'a -> 'a t -> bool

  val update : 'a -> bool -> 'a t -> 'a t
  (** [update x b set] adds [x] when [b = true]
      and removes [x] when [b = false].

      Adding an element already exists in the set
      and removing an element  non existent in the set
      do not change the set.
  *)

  val fold : ('elt -> 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc

  val fold' : ('elt * 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc
  (** A variant of [fold] which takes an uncurried function.
      This is useful when the curried function is rejected because of
      the unpackable type of ['acc].
  *)
end


(** Maps

    Map literal can be writen using [Map [ (k, v); .. ; (k, v) ]] expression.
*)

type ('k, 'v) map = Map of ('k const * 'v const) list const [@@deriving typerep]

module Map : sig
  type ('k, 'v) t = ('k, 'v) map [@@deriving typerep]
  val empty : ('k, 'v) t
  val length : ('k, 'v) t -> nat
  val map : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  val map' : (('k * 'v) -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k, 'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
  (** [update k vopt map] adds [k=v] when [vopt = Some v]
      and removes [k] when [vopt = None].

      Adding a binding already exists in the set overrides
      the existing binding.

      Removing a binding non existent in the map does not change
      the map.
  *)

  val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

  val fold' : (('k * 'v * 'acc) -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
  (** A variant of [fold] which takes an uncurried function.
      This is useful when the curried function is rejected because of
      the unpackable type of ['v] and ['acc].
  *)
end

(** Big maps

    No way to write a literal of big maps.
*)

type ('k, 'v) big_map = BigMap of ('k const * 'v const) list const [@@deriving typerep]
(** Constructor [BigMap] is allowed to use only in the conversion mode
    to initialize smart contract storages.

    It is not usable in smart contract codes.
*)

module BigMap : sig
  type ('k, 'v) t = ('k, 'v) big_map = BigMap of ('k * 'v) list [@@deriving typerep]
  val empty : ('k, 'v) t
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k, 'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
  (** [update k vopt map] adds [k=v] when [vopt = Some v]
      and removes [k] when [vopt = None].

      Adding a binding already exists in the set overrides
      the existing binding.

      Removing a binding non existent in the map does not change
      the big map.
  *)
end

(** Strings *)

module String : sig
  val length : string -> nat
  val concat : string -> string -> string
  val slice : nat -> nat -> string -> string option
  (** Substring. [slice n1 n2 s] returns a substring of length [n2]
      from the position [n1] (zero based).

      If the specified region by [n1] and [n2] exceeds the string [s],
      it returns [None].
  *)
end

val (^) : string -> string -> string

(** Bytes

    Bytes literals are written like [Bytes "0123456789abcdef"].
    The string must be even number of hex characters.
*)

type bytes = Bytes of string const [@@deriving typerep]

module Bytes : sig
  type t = bytes [@@deriving typerep]
  val length : t -> nat
  val concat : t -> t -> t
  val slice : nat -> nat -> t -> t option
  (** Subbytes. [slice n1 n2 s] returns a subbytes of length [n2]
      from the position [n1] (zero based).

      If the specified region by [n1] and [n2] exceeds the bytes [s],
      it returns [None].
  *)
end

(** Addresses *)
type address = Address of string const [@@deriving typerep]

module Address : sig
  type t = address [@@deriving typerep]
end

(** Key hashes *)
type key_hash = Key_hash of string const [@@deriving typerep]

module Key_hash : sig
  type t = key_hash [@@deriving typerep]
end

(** Contract, entry points, and operation *)

type 'a contract
type operation
type operations = operation list

type ('param, 'storage) entry = 'param -> 'storage -> operations * 'storage

(** Contracts *)
module Contract : sig
  type 'a t = 'a contract
  (** Contract whose parameter is ['a] *)

  val self : 'a t
  (** The contract of the code itself.  The type parameter of [self]
      must agree with the actual contract parameter.

      Unlike Michelson's [SELF] operator, [self] can appear inside a function.
      Even if the function value is sent to another contract, [self] still
      points to the original contract which uses [self] in its code.
  *)

  val contract : address -> 'a t option

  val contract' : address -> string const -> 'a t option
  (** Address to contract with an entry point name.  The name must not start with '%'. *)

  val implicit_account : key_hash -> unit t
  (** [tz1], [tz2], [tz3] accounts *)

  val address : 'a t -> address

  val create_from_tz_code : string -> key_hash option -> tz -> 'storage -> operation * address
  (** Raw interface for CREATE_CONTRACT.

      Michelson code must be given as a string LITERAL.
      In Tezos you cannot generate contract code programically in a contract.

      The types of the contract and the initial storage are NOT checked
      by SCaml.
  *)

  val create_raw : string -> key_hash option -> tz -> 'storage -> operation * address
  (** Same as [create_from_tz_code] *)

  val create_from_tz_file : string -> key_hash option -> tz -> 'storage -> operation * address
  (** CREATE_CONTRACT from a michelson source file.

      Michelson file name must be given as a string literal.
      In Tezos you cannot generate contract code programically in a contract.

      The types of the contract and the initial storage are NOT checked
      by SCaml.
  *)
end

(** Operations *)
module Operation : sig
  type t = operation
  val transfer_tokens : 'a -> tz -> 'a contract -> t
  val set_delegate : key_hash option -> t
end

(** Timestamps

    Timestamp literals are [Timestamp s] where [s] is a valid
    RFC3339 string. ex. [Timestamp "2019-09-11T08:30:23Z"].
*)
type timestamp = Timestamp of string const [@@deriving typerep]

module Timestamp : sig
  type t = timestamp [@@deriving typerep]
  val add : t -> int -> t
  val sub : t -> int -> t
  val diff : t -> t -> int
end

(** Chain ids *)
type chain_id = Chain_id of string const [@@deriving typerep]

module Chain_id : sig
  type t = chain_id [@@deriving typerep]
end

(** Global values

    They are consts but have functional types in order to provide
    semantics in the native OCaml compilation in future.
*)
module Global : sig
  val get_now      : unit -> timestamp
  val get_amount   : unit -> tz
  val get_balance  : unit -> tz
  val get_source   : unit -> address
  val get_sender   : unit -> address
  val get_chain_id : unit -> chain_id
  val get_level    : unit -> nat
end

module Env : sig
  type t

  val get : unit -> t

  val now      : t -> timestamp
  val amount   : t -> tz
  val balance  : t -> tz
  val source   : t -> address
  val sender   : t -> address
  val chain_id : t -> chain_id
end

(** Keys *)
type key = Key of string const [@@deriving typerep]

module Key : sig
  type t = key [@@deriving typerep]
end

(** Signatures *)
type signature = Signature of string const [@@deriving typerep]

module Signature : sig
  type t = signature [@@deriving typerep]
end

(** Cryptographic algorithms *)
module Crypto : sig
  val check_signature : key -> signature -> bytes -> bool
  val blake2b : bytes -> bytes
  val sha256 : bytes -> bytes
  val sha512 : bytes -> bytes
  val hash_key  : key -> key_hash

  module Internal : sig
    val test : unit -> unit
  end
end

(** Serialization *)
module Obj : sig

  val pack : 'a -> bytes
  val unpack : bytes -> 'a option

  module TypeSafe : sig
    val pack : 'a Typerep_lib.Std.Typerep.t -> 'a -> bytes
    val unpack : 'a Typerep_lib.Std.Typerep.t -> bytes -> 'a option
  end

  module Internal : sig
    module type TypeSafePack = sig
      open Typerep_lib.Std
      val pack' : 'a Typerep.t -> 'a -> string
      val unpack' : 'a Typerep.t -> string -> 'a option
    end

    val type_safe_pack : (module TypeSafePack) option ref
  end
end
