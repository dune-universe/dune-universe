open OUnit2
open Test_utils

let test_double_metaphone =
  List.map
    begin fun x ->
      let (fst, _snd) = Phonetic.DoubleMetaphone.double_metaphone x.input in
      let fst = if String.length fst > 4 then String.sub fst 0 4 else fst in
      x.input >:: fun _ctx -> assert_equal ~printer:(fun x -> x) x.double_metaphone fst
    end
    data

let test_soundex =
  List.fold_left
    begin fun acc x ->
      if x.soundex <> "" then
        begin x.input >:: fun _ctx ->
            assert_equal ~printer:(fun x -> x) x.soundex
              (Bytes.unsafe_to_string @@ Phonetic.Soundex.soundex @@ String.uppercase_ascii x.input)
        end :: acc
      else acc
    end
    [] data

let _ =
  run_test_tt_main begin
    "Phonetic" >::: [ "Double Metaphone" >::: test_double_metaphone
                    ; "Soundex" >::: test_soundex
                    ]
  end
