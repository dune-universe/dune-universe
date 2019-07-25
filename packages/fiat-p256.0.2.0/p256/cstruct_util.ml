let compare_be a b =
  let first_diff = ref None in
  let a_len = Cstruct.len a in
  let b_len = Cstruct.len b in
  if a_len <> b_len then invalid_arg "compare_be";
  for i = a_len - 1 downto 0 do
    let a_i = Cstruct.get_uint8 a i in
    let b_i = Cstruct.get_uint8 b i in
    match Pervasives.compare a_i b_i with
    | 0 ->
        ()
    | d ->
        first_diff := Some d
  done;
  match !first_diff with
  | None ->
      0
  | Some d ->
      d
