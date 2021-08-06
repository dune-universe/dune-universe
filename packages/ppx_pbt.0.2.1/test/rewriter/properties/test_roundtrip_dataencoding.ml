open Data_encoding

type t = (string * int) list

let arb = QCheck.(list (pair string int))

let encoding =
  list (tup2 string int31)
  [@@pbt {| roundtrip_data_encoding[arb] |}]
