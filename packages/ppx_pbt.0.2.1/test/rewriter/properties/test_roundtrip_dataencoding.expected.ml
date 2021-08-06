open Data_encoding

type t = (string * int) list

let arb = QCheck.(list (pair string int))

include struct
  let encoding =
    list (tup2 string int31)
    [@@pbt {| roundtrip_data_encoding[arb] |}]

  let test_encoding_is_roundtrip_data_encoding =
    QCheck.Test.make
      ~name:"encoding_is_roundtrip_data_encoding"
      arb
      (fun arb_0 -> Pbt.Properties.roundtrip_data_encoding encoding arb_0)

  let () = Runner.add_tests [ test_encoding_is_roundtrip_data_encoding ]
end
