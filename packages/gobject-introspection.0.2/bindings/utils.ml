
let unexpected_value_for (enum : string) : (int64 -> 'a)=
  (fun x ->
     let value = " - Unexpexted value " ^ Int64.to_string x in
     let message = enum ^ value in failwith message)
