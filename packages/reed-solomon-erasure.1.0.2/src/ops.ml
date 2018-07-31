let ( .%()   ) = Bytes.get
let ( .%()<- ) = Bytes.set

let ( .%{}   ) = Bytes.unsafe_get
let ( .%{}<- ) = Bytes.unsafe_set

let ( .%[]   ) = String.unsafe_get

let bigstr_unsafe_get_uint8 b n = Core_kernel.Bigstring.unsafe_get_uint8 ~pos:n b

let bigstr_unsafe_set_uint8 b n v = Core_kernel.Bigstring.unsafe_set_uint8 ~pos:n b v

let ( .&{}   ) = bigstr_unsafe_get_uint8
let ( .&{}<- ) = bigstr_unsafe_set_uint8
