type t = string

let to_int_list bs =
  let len = String.length bs in
  let rec f i res =
    if i >= len then List.rev res
    else f (i + 1) ((Char.code bs.[i])::res)
  in 
    f 0 []

let to_hex_string ?(sep=" ") bs =
  to_int_list bs
  |> List.map (fun v -> Printf.sprintf "%02X" v)
  |> String.concat sep

let to_bit_list bs = 
  let masks = [ 0x80; 0x40; 0x20; 0x10; 0x08; 0x04; 0x02; 0x01 ] in
  let octet_bits off octet =
    List.mapi (fun i m -> if (octet land m) > 0 then off + i else -1) masks
    |> List.filter (fun v -> v >= 0)
  in
    to_int_list bs
    |> List.mapi (fun i v -> octet_bits (i*8) v)
    |> List.concat

let to_string bs =
  let hex = to_hex_string bs in
  let bits = to_bit_list bs
    |> List.map (Printf.sprintf "%d")
    |> String.concat " "
  in
    if String.length bits > 0 then hex ^ " BITS(" ^ bits ^ ")"
    else hex
