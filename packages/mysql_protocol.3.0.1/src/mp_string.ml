
let rec null_terminated_string bits acc =
  let length_rest = (Bitstring.bitstring_length bits) - 8 in
  match%bitstring bits with
  | {| c : 1*8 : int, unsigned, bigendian;
      rest : length_rest : bitstring |} ->
    if c = 0 then
      (rest, acc)
    else 
      let acc = acc ^ (String.make 1 (Char.chr c)) in
      null_terminated_string rest acc

let length_coded_string bits = 
  let (length, rest) = Mp_binary.length_coded_binary bits in
  let length_rest = (Bitstring.bitstring_length rest) - (8 * (Int64.to_int length)) in
  match%bitstring rest with
  | {| s : 8 * (Int64.to_int length) : string;
      rest : length_rest : bitstring |} -> (s, rest)

let make_null_terminated_string s = 
  let null = String.make 1 (Char.chr 0) in
  s ^ null
