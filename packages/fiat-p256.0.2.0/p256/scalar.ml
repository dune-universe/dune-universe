type t = Scalar of Cstruct.t

let is_in_range cs =
  let zero = Cstruct.create 32 in
  let n = Hex.to_cstruct Parameters.n in
  Cstruct_util.compare_be cs zero > 0 && Cstruct_util.compare_be n cs > 0

let of_cstruct cs =
  if Cstruct.len cs <> 32 then Error `Invalid_length
  else if is_in_range cs then Ok (Scalar (Cstruct.rev cs))
  else Error `Invalid_range

let bit_at (Scalar s) i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Cstruct.get_uint8 s byte_num in
  byte land (1 lsl bit_num) <> 0
