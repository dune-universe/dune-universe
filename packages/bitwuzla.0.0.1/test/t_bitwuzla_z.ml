open Bitwuzla_c
open Bitwuzla_z
open T_bitwuzla_c
open Format

let dump_z128 z = print_string @@ Z.format "#032x" z

let with_bv128_of_z f z =
  with_t (fun t ->
      let bv128 = mk_bv_sort t 128 in
      let str = Z.format "032x" z in
      f @@ mk_bv_value t bv128 str Hex)

let%test "small values" =
  with_bv8_sort (fun (t, bv8) ->
      Array.init 256 (fun i -> Z.of_int i, mk_bv_value_int t bv8 i)
      |> Array.for_all (fun (z, bv) -> z = value_get_bv_bits bv))

let%expect_test "multi-limbs even" =
  with_bv128_of_z (fun bv -> dump_z128 @@ value_get_bv_bits bv)
    (Z.shift_left Z.one 96);
  [%expect {| 0x000001000000000000000000000000 |}]

let%expect_test "multi-limbs odd" =
  with_bv128_of_z (fun bv -> dump_z128 @@ value_get_bv_bits bv)
    (Z.pred @@ Z.shift_left Z.one 128);
  [%expect {| 0xffffffffffffffffffffffffffffffff |}]
