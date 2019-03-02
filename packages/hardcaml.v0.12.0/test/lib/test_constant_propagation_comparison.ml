open! Import
open Signal.Const_prop.Comb
open Test_constant_propagation.Trace

let%expect_test "less than" =
  print_s @@ binary_op_tests "<:" (<:) (<:.);
  [%expect {|
    (<:
      (all_1_bit (
        (1'b0 <: 1'b0 = 1'b0)
        (1'b0 <: 1'b1 = 1'b1)
        (1'b1 <: 1'b0 = 1'b0)
        (1'b1 <: 1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 <: 2'b00 = 1'b0)
        (2'b00 <: 2'b01 = 1'b1)
        (2'b00 <: 2'b10 = 1'b1)
        (2'b00 <: 2'b11 = 1'b1)
        (2'b01 <: 2'b00 = 1'b0)
        (2'b01 <: 2'b01 = 1'b0)
        (2'b01 <: 2'b10 = 1'b1)
        (2'b01 <: 2'b11 = 1'b1)
        (2'b10 <: 2'b00 = 1'b0)
        (2'b10 <: 2'b01 = 1'b0)
        (2'b10 <: 2'b10 = 1'b0)
        (2'b10 <: 2'b11 = 1'b1)
        (2'b11 <: 2'b00 = 1'b0)
        (2'b11 <: 2'b01 = 1'b0)
        (2'b11 <: 2'b10 = 1'b0)
        (2'b11 <: 2'b11 = 1'b0)))
      (misc (
        (8'b00010110 <: 8'b00100001 = 1'b1)
        (123'h0000000000000000000000000005749
         <:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         1'b1)))
      (int_on_right (
        (7'b0011011 <: 12  = 1'b0)
        (7'b0011011 <: -12 = 1'b1)))) |}]

let%expect_test "greater than" =
  print_s @@ binary_op_tests ">:" (>:) (>:.);
  [%expect {|
    (>:
      (all_1_bit (
        (1'b0 >: 1'b0 = 1'b0)
        (1'b0 >: 1'b1 = 1'b0)
        (1'b1 >: 1'b0 = 1'b1)
        (1'b1 >: 1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 >: 2'b00 = 1'b0)
        (2'b00 >: 2'b01 = 1'b0)
        (2'b00 >: 2'b10 = 1'b0)
        (2'b00 >: 2'b11 = 1'b0)
        (2'b01 >: 2'b00 = 1'b1)
        (2'b01 >: 2'b01 = 1'b0)
        (2'b01 >: 2'b10 = 1'b0)
        (2'b01 >: 2'b11 = 1'b0)
        (2'b10 >: 2'b00 = 1'b1)
        (2'b10 >: 2'b01 = 1'b1)
        (2'b10 >: 2'b10 = 1'b0)
        (2'b10 >: 2'b11 = 1'b0)
        (2'b11 >: 2'b00 = 1'b1)
        (2'b11 >: 2'b01 = 1'b1)
        (2'b11 >: 2'b10 = 1'b1)
        (2'b11 >: 2'b11 = 1'b0)))
      (misc (
        (8'b00010110 >: 8'b00100001 = 1'b0)
        (123'h0000000000000000000000000005749
         >:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         1'b0)))
      (int_on_right (
        (7'b0011011 >: 12  = 1'b1)
        (7'b0011011 >: -12 = 1'b0)))) |}]

let%expect_test "less than or equal to" =
  print_s @@ binary_op_tests "<=:" (<=:) (<=:.);
  [%expect {|
    (<=:
      (all_1_bit (
        (1'b0 <=: 1'b0 = 1'b1)
        (1'b0 <=: 1'b1 = 1'b1)
        (1'b1 <=: 1'b0 = 1'b0)
        (1'b1 <=: 1'b1 = 1'b1)))
      (all_2_bits (
        (2'b00 <=: 2'b00 = 1'b1)
        (2'b00 <=: 2'b01 = 1'b1)
        (2'b00 <=: 2'b10 = 1'b1)
        (2'b00 <=: 2'b11 = 1'b1)
        (2'b01 <=: 2'b00 = 1'b0)
        (2'b01 <=: 2'b01 = 1'b1)
        (2'b01 <=: 2'b10 = 1'b1)
        (2'b01 <=: 2'b11 = 1'b1)
        (2'b10 <=: 2'b00 = 1'b0)
        (2'b10 <=: 2'b01 = 1'b0)
        (2'b10 <=: 2'b10 = 1'b1)
        (2'b10 <=: 2'b11 = 1'b1)
        (2'b11 <=: 2'b00 = 1'b0)
        (2'b11 <=: 2'b01 = 1'b0)
        (2'b11 <=: 2'b10 = 1'b0)
        (2'b11 <=: 2'b11 = 1'b1)))
      (misc (
        (8'b00010110 <=: 8'b00100001 = 1'b1)
        (123'h0000000000000000000000000005749
         <=:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         1'b1)))
      (int_on_right (
        (7'b0011011 <=: 12  = 1'b0)
        (7'b0011011 <=: -12 = 1'b1)))) |}]

let%expect_test "greater than or equal to" =
  print_s @@ binary_op_tests ">=:" (>=:) (>=:.);
  [%expect {|
    (>=:
      (all_1_bit (
        (1'b0 >=: 1'b0 = 1'b1)
        (1'b0 >=: 1'b1 = 1'b0)
        (1'b1 >=: 1'b0 = 1'b1)
        (1'b1 >=: 1'b1 = 1'b1)))
      (all_2_bits (
        (2'b00 >=: 2'b00 = 1'b1)
        (2'b00 >=: 2'b01 = 1'b0)
        (2'b00 >=: 2'b10 = 1'b0)
        (2'b00 >=: 2'b11 = 1'b0)
        (2'b01 >=: 2'b00 = 1'b1)
        (2'b01 >=: 2'b01 = 1'b1)
        (2'b01 >=: 2'b10 = 1'b0)
        (2'b01 >=: 2'b11 = 1'b0)
        (2'b10 >=: 2'b00 = 1'b1)
        (2'b10 >=: 2'b01 = 1'b1)
        (2'b10 >=: 2'b10 = 1'b1)
        (2'b10 >=: 2'b11 = 1'b0)
        (2'b11 >=: 2'b00 = 1'b1)
        (2'b11 >=: 2'b01 = 1'b1)
        (2'b11 >=: 2'b10 = 1'b1)
        (2'b11 >=: 2'b11 = 1'b1)))
      (misc (
        (8'b00010110 >=: 8'b00100001 = 1'b0)
        (123'h0000000000000000000000000005749
         >=:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         1'b0)))
      (int_on_right (
        (7'b0011011 >=: 12  = 1'b1)
        (7'b0011011 >=: -12 = 1'b0)))) |}]

(* equality *)

let%expect_test "equals" =
  print_s @@ binary_op_tests "==:" (==:) (==:.);
  [%expect {|
    (==:
      (all_1_bit (
        (1'b0 ==: 1'b0 = 1'b1)
        (1'b0 ==: 1'b1 = 1'b0)
        (1'b1 ==: 1'b0 = 1'b0)
        (1'b1 ==: 1'b1 = 1'b1)))
      (all_2_bits (
        (2'b00 ==: 2'b00 = 1'b1)
        (2'b00 ==: 2'b01 = 1'b0)
        (2'b00 ==: 2'b10 = 1'b0)
        (2'b00 ==: 2'b11 = 1'b0)
        (2'b01 ==: 2'b00 = 1'b0)
        (2'b01 ==: 2'b01 = 1'b1)
        (2'b01 ==: 2'b10 = 1'b0)
        (2'b01 ==: 2'b11 = 1'b0)
        (2'b10 ==: 2'b00 = 1'b0)
        (2'b10 ==: 2'b01 = 1'b0)
        (2'b10 ==: 2'b10 = 1'b1)
        (2'b10 ==: 2'b11 = 1'b0)
        (2'b11 ==: 2'b00 = 1'b0)
        (2'b11 ==: 2'b01 = 1'b0)
        (2'b11 ==: 2'b10 = 1'b0)
        (2'b11 ==: 2'b11 = 1'b1)))
      (misc (
        (8'b00010110 ==: 8'b00100001 = 1'b0)
        (123'h0000000000000000000000000005749
         ==:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         1'b0)))
      (int_on_right (
        (7'b0011011 ==: 12  = 1'b0)
        (7'b0011011 ==: -12 = 1'b0)))) |}]

let%expect_test "not equals" =
  print_s @@ binary_op_tests "<>:" (<>:) (<>:.);
  [%expect {|
    (<>:
      (all_1_bit (
        (1'b0 <>: 1'b0 = 1'b0)
        (1'b0 <>: 1'b1 = 1'b1)
        (1'b1 <>: 1'b0 = 1'b1)
        (1'b1 <>: 1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 <>: 2'b00 = 1'b0)
        (2'b00 <>: 2'b01 = 1'b1)
        (2'b00 <>: 2'b10 = 1'b1)
        (2'b00 <>: 2'b11 = 1'b1)
        (2'b01 <>: 2'b00 = 1'b1)
        (2'b01 <>: 2'b01 = 1'b0)
        (2'b01 <>: 2'b10 = 1'b1)
        (2'b01 <>: 2'b11 = 1'b1)
        (2'b10 <>: 2'b00 = 1'b1)
        (2'b10 <>: 2'b01 = 1'b1)
        (2'b10 <>: 2'b10 = 1'b0)
        (2'b10 <>: 2'b11 = 1'b1)
        (2'b11 <>: 2'b00 = 1'b1)
        (2'b11 <>: 2'b01 = 1'b1)
        (2'b11 <>: 2'b10 = 1'b1)
        (2'b11 <>: 2'b11 = 1'b0)))
      (misc (
        (8'b00010110 <>: 8'b00100001 = 1'b1)
        (123'h0000000000000000000000000005749
         <>:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         1'b1)))
      (int_on_right (
        (7'b0011011 <>: 12  = 1'b1)
        (7'b0011011 <>: -12 = 1'b1)))) |}]
