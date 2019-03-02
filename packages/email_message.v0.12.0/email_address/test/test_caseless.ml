open Core
open Email_address

module Compare_result = struct
  type t =
    { a : string
    ; b : string
    ; compare_equal : bool
    ; hash_equal : bool
    ; caseless_compare_equal : bool
    ; caseless_hash_equal : bool
    }
  [@@deriving fields]

  let columns =
    let c = Ascii_table.Column.create in
    [ c "A" a
    ; c "B" b
    ; c "compare =" (Fn.compose Bool.to_string compare_equal)
    ; c "hash =" (Fn.compose Bool.to_string hash_equal)
    ; c "caseless compare =" (Fn.compose Bool.to_string caseless_compare_equal)
    ; c "caseless hash =" (Fn.compose Bool.to_string caseless_hash_equal)
    ]
  ;;
end

let%expect_test "compare" =
  let compare (a, b) =
    let a_address = of_string_exn a in
    let b_address = of_string_exn b in
    { Compare_result.a
    ; b
    ; compare_equal = [%compare.equal: t] a_address b_address
    ; hash_equal = Int.equal (hash a_address) (hash b_address)
    ; caseless_compare_equal = [%compare.equal: Caseless.t] a_address b_address
    ; caseless_hash_equal = Int.equal (Caseless.hash a_address) (Caseless.hash b_address)
    }
  in
  let results =
    List.map
      ~f:compare
      [ "a@b.com", "a@B.COM"; "a@b.com", "A@b.com"; "a@b.com", "A@B.COM" ]
  in
  print_endline (Ascii_table.to_string Compare_result.columns results);
  [%expect
    {|
    ┌─────────┬─────────┬───────────┬────────┬────────────────────┬─────────────────┐
    │ A       │ B       │ compare = │ hash = │ caseless compare = │ caseless hash = │
    ├─────────┼─────────┼───────────┼────────┼────────────────────┼─────────────────┤
    │ a@b.com │ a@B.COM │ true      │ true   │ true               │ true            │
    │ a@b.com │ A@b.com │ false     │ false  │ true               │ true            │
    │ a@b.com │ A@B.COM │ false     │ false  │ true               │ true            │
    └─────────┴─────────┴───────────┴────────┴────────────────────┴─────────────────┘ |}]
;;
