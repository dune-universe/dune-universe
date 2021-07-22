let zero_cstruct (x : Cstruct.t) : unit = Cstruct.memset x 0

let zero_cstruct_array (arr : Cstruct.t array) : unit =
  Array.iter zero_cstruct arr

let xor_into ~(src : Cstruct.t) ~(into : Cstruct.t) : unit =
  assert (Cstruct.length src = Cstruct.length into);
  let src = Cstruct.to_bigarray src in
  let into = Cstruct.to_bigarray into in
  Lt_code_stubs.xor_into src into

let memcpy ~src ~dst =
  assert (Cstruct.length src = Cstruct.length dst);
  let src = Cstruct.to_bigarray src in
  let dst = Cstruct.to_bigarray dst in
  Lt_code_stubs.memcpy src dst

let cstruct_array_is_consistent (arr : Cstruct.t array) : bool =
  Array.length arr = 0
  ||
  let len = Cstruct.length arr.(0) in
  Array.for_all (fun x -> Cstruct.length x = len) arr

let fill_array (v : 'a) (arr : 'a array) : unit =
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- v
  done
