open Js

class type bigInt = object
  method toLocaleString : js_string t optdef -> _ t optdef -> js_string t meth
  method toString : js_string t meth
  method valueOf : bigInt t meth
end

type nonrec t = bigInt t

let bigInt : (Unsafe.any -> t) = Unsafe.variable "BigInt"

let of_string s = bigInt (Unsafe.inject @@ string s)
let of_int (i : int) = bigInt (Unsafe.inject @@ i)
let of_int64 (i : int64) = of_string (Int64.to_string i)
let of_native (i : nativeint) = of_string (Nativeint.to_string i)
let of_float f = bigInt (Unsafe.inject @@ number_of_float f)

let to_locale_string ?fmt ?options (n : t) =
  to_string (n##toLocaleString (optdef string fmt) (Optdef.option options))
let to_string (n : t) = to_string n##toString
