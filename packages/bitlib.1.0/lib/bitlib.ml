let rec _pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = _pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let to_bits i =
  let rec aux = function
  | 0 -> []
  | n -> n mod 2 :: aux (n / 2)
  in
  List.rev (aux i)

let pad i bits =
  let rec aux = function
  | 0 -> bits
  | n -> 0 :: aux (n - 1)
  in
  let len = List.length bits in
  if i - len > 0 then aux (i - len) else bits

let to_bytes bits n =
  let rec to_byte bits cur last =
    match cur with
    | i when i = last -> 0
    | _ ->
      let bit = List.hd !bits in
      bits := List.tl !bits;
      let acc = bit * _pow 2 cur in
      acc + to_byte bits (cur + 1) last
  in
  let rec aux bits = function
  | i when i = n -> []
  | i ->
    let sum = to_byte bits (i * 8) (i * 8 + 8) in
    let acc = sum / _pow 2 (i * 8) in
    acc :: aux bits (i + 1)
  in
  let low_fst = ref (List.rev bits) in
  List.rev (aux low_fst 0)

let little_endian_of_int num bytes =
  let num_of_bits = bytes * 8 in
  let bits = pad num_of_bits (to_bits num) in
  to_bytes bits bytes

let big_endian_of_int num bytes =
  let num_of_bits = bytes * 8 in
  let bits = pad num_of_bits (to_bits num) in
  List.rev (to_bytes bits bytes)

let write_binary_file bytes filename =
  let rec aux ch instrs =
    match instrs with
    | h :: t -> output_byte ch h; aux ch t
    | [] -> close_out ch
  in
  let channel = open_out_bin filename in
  aux channel bytes
