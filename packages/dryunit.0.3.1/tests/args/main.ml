

module To_test = struct
  let escape letter = Char.escaped letter
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

let test_capit_in_main () =
  Alcotest.(check string) "same chars"  "a" (To_test.escape 'a')

let test_plus_in_main () =
  Alcotest.(check int) "same ints" 7 (To_test.plus [1;1;2;3])


let _ =
  [%dryunit
    { cache_dir = ".dryunit"
    ; cache     = true
    ; framework = "alcotest"
    ; ignore    = "capita"
    ; filter    = ""
    ; detection = "file"
    }
  ]
