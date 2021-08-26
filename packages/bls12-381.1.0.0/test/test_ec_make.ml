(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

(* copied from G_SIG to remove the dependency on
   bls12-381-gen
*)
module type G_SIG = sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Ff_sig.PRIME

  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)
  val empty : unit -> t

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array of length [size_in_bytes]. *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array of length [size_in_bytes].
      Raise [Not_on_curve] if the point is not on the curve
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Allocates a new point from a byte of length [size_in_bytes / 2] array
      representing a point in compressed form.
  *)
  val of_compressed_bytes_opt : Bytes.t -> t option

  (** Allocates a new point from a byte array of length [size_in_bytes / 2]
      representing a point in compressed form.
      Raise [Not_on_curve] if the point is not on the curve.
  *)
  val of_compressed_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Return a compressed bytes representation *)
  val to_compressed_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element. The element is on the curve and in the prime
      subgroup.
  *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** [double g] returns [2g] *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t

  (** [fft ~domain ~points] performs a Fourier transform on [points] using [domain]
      The domain should be of the form [w^{i}] where [w] is a principal root of
      unity. If the domain is of size [n], [w] must be a [n]-th principal root
      of unity.
      The number of points can be smaller than the domain size, but not larger. The
      complexity is in [O(n log(m))] where [n] is the domain size and [m] the
      number of points.
   *)
  val fft : domain:Scalar.t array -> points:t array -> t array

  (** [ifft ~domain ~points] performs an inverse Fourier transform on [points]
      using [domain].
      The domain should be of the form [w^{-i}] (i.e the "inverse domain") where
      [w] is a principal root of
      unity. If the domain is of size [n], [w] must be a [n]-th principal root
      of unity.
      The domain size must be exactly the same than the number of points. The
      complexity is O(n log(n)) where [n] is the domain size.
  *)
  val ifft : domain:Scalar.t array -> points:t array -> t array

  val hash_to_curve : Bytes.t -> Bytes.t -> t
end

module MakeEquality (G : G_SIG) = struct
  (** Verify the equality is correct with the value zero *)
  let zero () = assert (G.eq G.zero G.zero)

  (** Verify the equality is correct with the value one *)
  let one () = assert (G.eq G.one G.one)

  (** Verify the equality of two random values created invidually *)
  let random_same_objects () =
    let random = G.random () in
    assert (G.eq random random)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "equality",
      [ test_case "zero" `Quick (repeat 1 zero);
        test_case "one" `Quick (repeat 1 one);
        test_case "random_same_objects" `Quick (repeat 100 random_same_objects)
      ] )
end

module MakeValueGeneration (G : G_SIG) = struct
  let random () = ignore @@ G.random ()

  let negation_with_random () =
    let random = G.random () in
    ignore @@ G.negate random

  let negation_with_zero () = ignore @@ G.negate G.zero

  let negation_with_one () = ignore @@ G.negate G.one

  let double_with_zero () = ignore @@ G.double G.zero

  let double_with_one () = ignore @@ G.double G.one

  let double_with_random () =
    let g = G.random () in
    ignore @@ G.double g

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "value generation",
      [ test_case "random" `Quick (repeat 100 random);
        test_case "negate_with_one" `Quick (repeat 1 negation_with_one);
        test_case "negate_with_zero" `Quick (repeat 1 negation_with_zero);
        test_case "negate_with_random" `Quick (repeat 100 negation_with_random);
        test_case "double_with_random" `Quick (repeat 100 double_with_random);
        test_case "double_with_one" `Quick (repeat 1 double_with_one);
        test_case "double_with_zero" `Quick (repeat 100 double_with_zero) ] )
end

module MakeIsZero (G : G_SIG) = struct
  let with_zero_value () = assert (G.is_zero G.zero = true)

  let with_one_value () = assert (G.is_zero G.one = false)

  let with_random_value () = assert (G.is_zero (G.random ()) = false)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "is_zero",
      [ test_case "with zero value" `Quick (repeat 1 with_zero_value);
        test_case "with one value" `Quick (repeat 1 with_one_value);
        test_case "with random value" `Quick (repeat 100 with_random_value) ] )
end

module MakeECProperties (G : G_SIG) = struct
  (** Verify that a random point is valid *)
  let check_bytes_random () = assert (G.(check_bytes @@ to_bytes @@ random ()))

  (** Verify that the zero point is valid *)
  let check_bytes_zero () = assert (G.(check_bytes @@ to_bytes @@ zero))

  (** Verify that the fixed generator point is valid *)
  let check_bytes_one () = assert (G.(check_bytes @@ to_bytes @@ one))

  (** Verify that doubling a random point gives a valid point *)
  let check_bytes_random_double () =
    assert (G.(check_bytes @@ to_bytes @@ double (random ())))

  (** Verify that the sum of random points is valid *)
  let check_bytes_random_sum () =
    assert (G.(check_bytes @@ to_bytes @@ add (random ()) (random ())))

  (** Verify that multiplying a random point by a scalar gives a valid point *)
  let check_bytes_random_multiplication () =
    assert (G.(check_bytes @@ to_bytes @@ mul (random ()) (Scalar.random ())))

  (** Verify 0_S * g_EC = 0_EC where 0_S is the zero of the scalar field, 0_EC
  is the point at infinity and g_EC is an element of the EC *)
  let zero_scalar_nullifier_random () =
    let random = G.random () in
    assert (G.is_zero (G.mul random G.Scalar.zero))

  (** Verify 0_S * 0_EC = 0_EC where 0_S is the zero of the scalar field and
  0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_zero () =
    assert (G.is_zero (G.mul G.zero G.Scalar.zero))

  (** Verify 0_S * 1_EC = 0_EC where 0_S is the 0 of the scalar field, 1_EC is a
  fixed generator and 0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_one () =
    assert (G.is_zero (G.mul G.one G.Scalar.zero))

  let multiply_by_one_does_nothing () =
    let g = G.random () in
    assert (G.(eq (mul g Scalar.one) g))

  (** Verify -(-g) = g where g is an element of the EC *)
  let opposite_of_opposite () =
    let random = G.random () in
    assert (G.eq (G.negate (G.negate random)) random)

  let opposite_of_opposite_using_scalar () =
    let r = G.random () in
    assert (G.(eq r (mul r (Scalar.negate (Scalar.negate Scalar.one)))))

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_zero_is_zero () = assert (G.eq (G.negate G.zero) G.zero)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_opposite_of_zero_is_zero () =
    assert (G.eq (G.negate (G.negate G.zero)) G.zero)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_opposite_of_one_is_one () =
    assert (G.eq (G.negate (G.negate G.one)) G.one)

  (** Verify g1 + (g2 + g3) = (g1 + g2) + g3 where g1, g2 and g3 are elements of the EC *)
  let additive_associativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    let g3 = G.random () in
    assert (G.eq (G.add (G.add g1 g2) g3) (G.add (G.add g2 g3) g1))

  (** Verify that g + (-g) = 0 *)
  let opposite_existential_property () =
    let g = G.random () in
    assert (G.(eq (add g (negate g)) zero))

  (** Verify a (g1 + g2) = a * g1 + a * g2 where a is a scalar, g1, g2 two
  elements of the EC *)
  let distributivity () =
    let s = G.Scalar.random () in
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.mul (G.add g1 g2) s) (G.add (G.mul g1 s) (G.mul g2 s)))

  (** Verify (a + -a) * g = a * g - a * g = 0 *)
  let opposite_equality () =
    let a = G.Scalar.random () in
    let g = G.random () in
    assert (G.(eq (mul g (Scalar.add a (Scalar.negate a))) zero)) ;
    assert (G.(eq zero (add (mul g a) (mul g (Scalar.negate a))))) ;
    assert (
      G.(
        eq
          (mul g (Scalar.add a (Scalar.negate a)))
          (add (mul g a) (mul g (Scalar.negate a)))) )

  (** a g + b + g = (a + b) g*)
  let additive_associativity_with_scalar () =
    let a = G.Scalar.random () in
    let b = G.Scalar.random () in
    let g = G.random () in
    let left = G.(add (mul g a) (mul g b)) in
    let right = G.(mul g (Scalar.add a b)) in
    assert (G.(eq left right))

  (** (a * b) g = a (b g) = b (a g) *)
  let multiplication_properties_on_base_field_element () =
    let a = G.Scalar.random () in
    let b = G.Scalar.random () in
    let g = G.random () in
    assert (G.(eq (mul g (Scalar.mul a b)) (mul (mul g a) b))) ;
    assert (G.(eq (mul g (Scalar.mul a b)) (mul (mul g b) a)))

  let random_is_in_prime_subgroup () =
    let g = G.random () in
    (* [order] g = 0 *)
    assert (G.(eq (mul g Scalar.(of_z order)) zero)) ;
    (* [order - 1 ] g = [-1] g *)
    assert (G.(eq (mul g Scalar.(of_z (Z.pred order))) (G.negate g)))

  (** Verify (-s) * g = s * (-g) *)
  let opposite_of_scalar_is_opposite_of_ec () =
    let s = G.Scalar.random () in
    let g = G.random () in
    let left = G.mul g (G.Scalar.negate s) in
    let right = G.mul (G.negate g) s in
    assert (G.eq left right)

  (** Verify 2*g = g + g *)
  let double () =
    let s = G.random () in
    assert (G.(eq (double s) (add s s)))

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "Field properties",
      [ test_case "check_bytes_random" `Quick (repeat 100 check_bytes_random);
        test_case "check_bytes_zero" `Quick (repeat 1 check_bytes_zero);
        test_case "check_bytes_one" `Quick (repeat 1 check_bytes_one);
        test_case
          "check_bytes_random_double"
          `Quick
          (repeat 100 check_bytes_random_double);
        test_case
          "check_bytes_random_sum"
          `Quick
          (repeat 100 check_bytes_random_sum);
        test_case
          "check_bytes_random_multiplication"
          `Quick
          (repeat 100 check_bytes_random_multiplication);
        test_case
          "zero_scalar_nullifier_one"
          `Quick
          (repeat 1 zero_scalar_nullifier_one);
        test_case
          "zero_scalar_nullifier_zero"
          `Quick
          (repeat 1 zero_scalar_nullifier_zero);
        test_case
          "zero_scalar_nullifier_random"
          `Quick
          (repeat 100 zero_scalar_nullifier_random);
        test_case
          "multiply_by_one_does_nothing"
          `Quick
          (repeat 100 multiply_by_one_does_nothing);
        test_case
          "opposite_of_opposite"
          `Quick
          (repeat 100 opposite_of_opposite);
        test_case
          "opposite_of_opposite_using_scalar"
          `Quick
          (repeat 100 opposite_of_opposite_using_scalar);
        test_case
          "opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_zero_is_zero);
        test_case
          "opposite_of_opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_opposite_of_zero_is_zero);
        test_case
          "opposite_of_opposite_of_one_is_one"
          `Quick
          (repeat 1 opposite_of_opposite_of_one_is_one);
        test_case "opposite_equality" `Quick (repeat 1 opposite_equality);
        test_case "distributivity" `Quick (repeat 100 distributivity);
        test_case
          "opposite_of_scalar_is_opposite_of_ec"
          `Quick
          (repeat 100 opposite_of_scalar_is_opposite_of_ec);
        test_case
          "opposite_existential_property"
          `Quick
          (repeat 100 opposite_existential_property);
        test_case
          "multiplication_properties_on_base_field_element"
          `Quick
          (repeat 100 multiplication_properties_on_base_field_element);
        test_case "double" `Quick (repeat 100 double);
        test_case
          "additive_associativity_with_scalar"
          `Quick
          (repeat 100 additive_associativity_with_scalar);
        test_case
          "random elements are generated in the prime subgroup"
          `Quick
          (repeat 100 random_is_in_prime_subgroup);
        test_case
          "additive_associativity"
          `Quick
          (repeat 100 additive_associativity) ] )
end

module MakeCompressedRepresentation (G : G_SIG) = struct
  let test_recover_correct_point_uncompressed () =
    let g = G.random () in
    let compressed_bytes = G.to_compressed_bytes g in
    let uncompressed_g = G.of_compressed_bytes_exn compressed_bytes in
    assert (G.eq g uncompressed_g)

  (* it is correct to test this for BLS12-381 *)
  let test_compressed_version_is_half_the_size () =
    let g = G.random () in
    assert (Bytes.length (G.to_compressed_bytes g) = G.size_in_bytes / 2)

  let test_most_significant_bit_is_set_to_1 () =
    let g = G.random () in
    let compressed_g_bytes = G.to_compressed_bytes g in
    let first_byte = int_of_char @@ Bytes.get compressed_g_bytes 0 in
    assert (first_byte land 0b10000000 = 0b10000000)

  let test_check_second_most_significant_bit_is_set_to_1_for_zero () =
    let g = G.zero in
    let compressed_g_bytes = G.to_compressed_bytes g in
    let first_byte = int_of_char @@ Bytes.get compressed_g_bytes 0 in
    assert (first_byte land 0b01000000 = 0b01000000)

  let test_compressed_version_x_in_big_endian () =
    (* The compressed version fully carries the x coordinate. For G1, the
       compressed
       version is 384 bits with the 3 most significant bits being used to carry
       information to compute the y coordinate.
       For G2, as it is built on Fp2, the compressed version is (384 * 2) bits
       and the most significant 3 bits carries the same information. The bits
       385, 386 and 387 (i.e. the last 3 bits of the constant coefficient of the
       x coordinate) are set to zero/unused.
    *)
    let g = G.random () in
    let g_bytes = G.to_bytes g in
    let x_bytes_be = Bytes.sub g_bytes 0 (G.size_in_bytes / 2) in
    let compressed_g_bytes = G.to_compressed_bytes g in
    let compressed_g_bytes_first_byte = Bytes.get compressed_g_bytes 0 in
    (* Get rid of the last 3 bits as it carries information unrelated to x *)
    let compressed_g_bytes_first_byte =
      int_of_char compressed_g_bytes_first_byte land 0b00011111
    in
    Bytes.set compressed_g_bytes 0 (char_of_int compressed_g_bytes_first_byte) ;
    assert (Bytes.equal x_bytes_be compressed_g_bytes)

  let get_tests () =
    let open Alcotest in
    ( "Compressed representation",
      [ test_case
          "Recover correct point"
          `Quick
          test_recover_correct_point_uncompressed;
        test_case
          "Most significant bit is set to 1"
          `Quick
          (repeat 100 test_most_significant_bit_is_set_to_1);
        test_case
          "Second most significant bit is set to 1 for the identity element"
          `Quick
          test_check_second_most_significant_bit_is_set_to_1_for_zero;
        test_case
          "Verify x is fully in the compressed version and in big endian"
          `Quick
          (repeat 100 test_compressed_version_x_in_big_endian);
        test_case
          "Compressed version is half the size"
          `Quick
          test_compressed_version_is_half_the_size ] )
end
