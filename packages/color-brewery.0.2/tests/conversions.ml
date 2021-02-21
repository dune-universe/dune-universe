open Printf
module C = Color_brewery

let () =
  let test_int i = assert(C.to_int (C.of_int_exn i) = i) in
  test_int 0x5e0063;
  test_int 0xffebaa;

  let test_string i s0 =
    let s = C.to_string (C.of_int_exn i) in
    if s <> s0 then failwith(sprintf "to_string: expected %s, got %s" s0 s) in
  test_string 0x5e0063 "#5e0063";
  test_string 0xffebaa "#ffebaa"
