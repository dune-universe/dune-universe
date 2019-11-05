type t = Idle | Working | Short_break | Long_break

let to_string = function
  | Idle ->
      "Idle"
  | Working ->
      "Work"
  | Short_break ->
      "Short"
  | Long_break ->
      "Long"
