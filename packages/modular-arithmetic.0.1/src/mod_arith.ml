(*
 * modular-arithmetic
 *
 * An ocaml library for operations on integers modulo some integer (the modulus)
 *
 * Copyright (c) 2018, Raphael Sousa Santos <contact@raphaelss.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 *)

exception No_mul_inverse of int * int

module type Modulus = sig
  val modulus : int
end

module type Mod_int = sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val of_int : int -> t

  val to_int : t -> int

  val of_string : string -> t

  val of_string_opt : string -> t option

  val to_string : t -> string

  val of_float : float -> t

  val to_float : t -> float

  val modulus : int

  val zero : t

  val one : t

  val minus_one : t

  val min_int : t

  val max_int : t

  val min : t -> t -> t

  val max : t -> t -> t

  val mul_inv : t -> t

  val pred : t -> t

  val succ : t -> t

  val ( + ) : t -> t -> t

  val add : t -> t -> t

  val ( - ) : t -> t -> t

  val sub : t -> t -> t

  val ( * ) : t -> t -> t

  val mul : t -> t -> t

  val ( / ) : t -> t -> t

  val div : t -> t -> t

  val ( ** ) : t -> t -> t

  val pow : t -> t -> t

  val rem : t -> t -> t
end

module Make (M : Modulus) : Mod_int = struct
  type t = int

  let equal a b =
    a = b

  let compare a b =
    compare a b

  let rec of_int x =
    if x < 0 then
      of_int (x + M.modulus)
    else
      x mod M.modulus

  let to_int x =
    x

  let of_string s =
    of_int (int_of_string s)

  let of_string_opt s =
    match int_of_string_opt s with
    | None -> None
    | Some x -> Some (of_int x)

  let to_string x =
    string_of_int x

  let of_float x =
    of_int (int_of_float x)

  let to_float =
    float_of_int

  let zero =
    of_int 0

  let one =
    of_int 1

  let minus_one =
    of_int (-1)

  let modulus =
    M.modulus

  let min_int =
    zero

  let max_int =
    minus_one

  let min a b =
    if a < b then a else b

  let max a b =
    if a > b then a else b

  let mul_inv x =
    let rec loop n = function
      | 0 -> raise (No_mul_inverse (x, modulus))
      | 1 -> (0, 1)
      | x ->
        let quot = n / x in
        let rem = n mod x in
        let (quot', rem') = loop x rem in
        (rem', quot' - rem' * quot)
    in
    snd (loop modulus x)

  let pred x =
    of_int (x - 1)

  let succ x =
    of_int (x + 1)

  let add a b =
    of_int (a + b)

  let (+) a b =
    add a b

  let sub a b =
    of_int (a - b)

  let (-) a b =
    sub a b

  let mul a b =
    of_int (a * b)

  let ( * ) a b =
    mul a b

  let div a b =
    of_int (a * mul_inv b)

  let (/) a b =
    div a b

  let pow a b =
    of_float (float_of_int a ** float_of_int b)

  let ( ** ) a b =
    pow a b

  let rem a b =
    ignore (mul_inv b); (* throws in case of no inverse *)
    0
end
