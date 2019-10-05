open Core
module P = Test_pc

module Strings = struct
  let tests =
    Utils.invariant_suite (module P.String') "String"
    @@ List.map ["just a string"] ~f:(fun field -> P.String'.{field})
end

module Bytes = struct
  let tests =
    Utils.invariant_suite (module P.Bytes) "Bytes"
    @@ List.map ["just some bytes"] ~f:(fun field -> P.Bytes.{field})
end

module Integers = struct
  let common_non_negative_test_values =
    [
      0;
      1;
      42;
      127;
      128;
      255;
      256;
      32767;
      32768;
      65535;
      65536;
      Int32.(to_int max_value) |> Option.value ~default:Int.max_value;
    ]

  let common_negative_test_values = [-1; -42; -32768; -32769; -2147483648]

  let signed_32_bit_test_values =
    List.concat [common_non_negative_test_values; common_negative_test_values]

  let unsigned_32_bit_test_values =
    List.concat
      [
        common_non_negative_test_values;
        [
          Int32.(to_int max_value)
          |> Option.map ~f:(fun i -> (2 * i) + 1)
          |> Option.value ~default:Int.max_value;
          3_000_000_000;
        ];
      ]

  let signed_64_bit_test_values =
    List.concat
      [
        common_non_negative_test_values;
        common_negative_test_values;
        [Int.max_value; Int.min_value];
      ]

  let unsigned_64_bit_test_values =
    List.concat [common_non_negative_test_values; [Int.max_value]]

  let int32_tests =
    Utils.invariant_suite (module P.Int32') "Int32"
    @@ List.map signed_32_bit_test_values ~f:(fun field -> P.Int32'.{field})

  let int64_tests =
    Utils.invariant_suite (module P.Int64') "Int64"
    @@ List.map signed_64_bit_test_values ~f:(fun field -> P.Int64'.{field})

  let sint32_tests =
    Utils.invariant_suite (module P.Sint32) "Sint32"
    @@ List.map signed_32_bit_test_values ~f:(fun field -> P.Sint32.{field})

  let sint64_tests =
    Utils.invariant_suite (module P.Sint64) "Sint64"
    @@ List.map signed_64_bit_test_values ~f:(fun field -> P.Sint64.{field})

  let uint32_tests =
    Utils.invariant_suite (module P.Uint32) "Uint32"
    @@ List.map unsigned_32_bit_test_values ~f:(fun field -> P.Uint32.{field})

  let uint64_tests =
    Utils.invariant_suite (module P.Uint64) "Uint64"
    @@ List.map unsigned_64_bit_test_values ~f:(fun field -> P.Uint64.{field})

  let fixed32_tests =
    Utils.invariant_suite (module P.Fixed32) "Fixed32"
    @@ List.map unsigned_32_bit_test_values ~f:(fun field -> P.Fixed32.{field})

  let fixed64_tests =
    Utils.invariant_suite (module P.Fixed64) "Fixed64"
    @@ List.map unsigned_64_bit_test_values ~f:(fun field -> P.Fixed64.{field})

  let sfixed32_tests =
    Utils.invariant_suite (module P.Sfixed32) "Sfixed32"
    @@ List.map signed_32_bit_test_values ~f:(fun field -> P.Sfixed32.{field})

  let sfixed64_tests =
    Utils.invariant_suite (module P.Sfixed64) "Sfixed64"
    @@ List.map signed_64_bit_test_values ~f:(fun field -> P.Sfixed64.{field})
end

module Floats = struct
  let common_test_values = [0.0; 1.1; -2.8; Float.atan 0.5; 1.234e7; -1.937465623e-6]

  let float_tests =
    Utils.invariant_suite (module P.Float') "Float"
    @@ List.map common_test_values ~f:(fun field ->
           let field = field |> Int32.bits_of_float |> Int32.float_of_bits in
           P.Float'.{field})

  let double_tests =
    Utils.invariant_suite (module P.Double) "Double"
    @@ List.map common_test_values ~f:(fun field -> P.Double.{field})
end

module Bools = struct
  let tests =
    Utils.invariant_suite (module P.Bool') "Bool"
    @@ List.map [true; false] ~f:(fun field -> P.Bool'.{field})
end

module Messages = struct
  let two_fields_tests =
    Utils.invariant_suite
      (module P.Two_fields)
      "TwoFields"
      [
        {int_field = 42; string_field = "hey there!"};
        {int_field = -1; string_field = ""};
        {
          int_field = 0;
          string_field =
            {|\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
               "a rather problematic string"
              \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|};
        };
      ]

  let with_nested_submessage_tests =
    Utils.invariant_suite
      (module P.With_nested_submessage)
      "WithNestedSubmessage"
      [P.With_nested_submessage.{field = Some Nested.{field = "something"}}]

  let mutual_references_tests =
    Utils.invariant_suite
      (module P.Mutual_references)
      "MutualReferences"
      [
        P.Mutual_references.{field = Some Nested1.{field1 = None}};
        P.Mutual_references.
          {
            field =
              Some
                Nested1.{field1 = Some Nested2.{field2 = Some Nested1.{field1 = None}}};
          };
      ]
end

module Enums = struct
  let tests =
    Utils.invariant_suite
      (module P.With_enum)
      "WithEnum"
      [P.With_enum.{field = Day}; P.With_enum.{field = Night}]
end

module Repeated = struct
  let repeated_string_tests =
    Utils.invariant_suite
      (module P.Repeated_string)
      "RepeatedString"
      [P.Repeated_string.{field = ["aaa"; "bbb"]}]

  let repeated_int64_unpacked_tests =
    Utils.invariant_suite
      (module P.Repeated_int64_unpacked)
      "RepeatedInt64Unpacked"
      [P.Repeated_int64_unpacked.{field = [1; 2; 3]}]

  let repeated_int64_packed_tests =
    Utils.invariant_suite
      (module P.Repeated_int64_packed)
      "RepeatedInt64Packed"
      [P.Repeated_int64_packed.{field = [1; 2; 3]}]
end

module Oneof = struct
  let tests =
    Utils.invariant_suite
      (module P.With_one_of)
      "WithOneOf"
      [
        P.With_one_of.{choice = Some (Apples "42"); bananas = 47};
        P.With_one_of.{choice = Some (Oranges 42); bananas = 47};
        P.With_one_of.{choice = Some (Fruit_is_overrated {field = Day}); bananas = 47};
      ]
end

module Empty = struct
  let tests = Utils.invariant_suite (module P.Empty) "Empty" [()]
end

module Import = struct
  let tests =
    Utils.invariant_suite
      (module P.With_imported_message)
      "WithImportedMessage"
      [
        P.With_imported_message.{imported = None};
        P.With_imported_message.{imported = Some {field = 74}};
      ]
end

module Multiple_serialized_values = struct
  let string_testable : P.String'.t Alcotest.testable =
    Alcotest.testable P.String'.pp P.String'.equal

  type test_data = {
    input : P.Repeated_string.t;
    expected : P.String'.t;
  }

  let binary_format_roundtrip {input; expected} () =
    let open Result.Let_syntax in
    match input |> P.Repeated_string.to_binary >>= P.String'.of_binary with
    | Ok actual ->
        Alcotest.(
          check string_testable "to_binary |> of_binary mismatch" expected actual)
    | Error e -> Alcotest.fail (Utils.binary_error_to_string e)

  let text_format_roundtrip {input; expected} () =
    let open Result.Let_syntax in
    match input |> P.Repeated_string.to_text >>= P.String'.of_text with
    | Ok actual ->
        Alcotest.(check string_testable "to_text |> of_text mismatch" expected actual)
    | Error e -> Alcotest.fail (Utils.text_error_to_string e)

  let test_data =
    [
      {input = {field = ["first"; "second"]}; expected = {field = "second"}};
      {input = {field = ["first"; "second"; "third"]}; expected = {field = "third"}};
    ]

  let tests =
    ( "MultipleWireValues",
      List.concat
        [
          Utils.make_tests
            "Last binary value is retained"
            binary_format_roundtrip
            test_data;
          Utils.make_tests "Last text value is retained" text_format_roundtrip test_data;
        ] )
end

let () =
  Alcotest.run
    "Protocell test suite"
    [
      Strings.tests;
      Bytes.tests;
      Integers.int32_tests;
      Integers.int64_tests;
      Integers.sint32_tests;
      Integers.sint64_tests;
      Integers.uint32_tests;
      Integers.uint64_tests;
      Integers.fixed32_tests;
      Integers.fixed64_tests;
      Integers.sfixed32_tests;
      Integers.sfixed64_tests;
      Floats.float_tests;
      Floats.double_tests;
      Bools.tests;
      Messages.two_fields_tests;
      Messages.with_nested_submessage_tests;
      Messages.mutual_references_tests;
      Enums.tests;
      Repeated.repeated_string_tests;
      Repeated.repeated_int64_unpacked_tests;
      Repeated.repeated_int64_packed_tests;
      Oneof.tests;
      Empty.tests;
      Import.tests;
      Multiple_serialized_values.tests;
    ]
