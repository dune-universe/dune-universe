module To_test = struct
  let escape letter = Char.escaped letter
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

let test_capit () =
  Alcotest.(check string) "same chars"  "a" (To_test.escape 'a')

let test_plus () =
  Alcotest.(check int) "same ints" 7 (To_test.plus [1;1;2;3])

let test_capit_another4 () =
  Alcotest.(check string) "same chars"  "a" (To_test.escape 'a')
