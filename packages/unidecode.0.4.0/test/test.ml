open OUnit2
open Test_data

let test_decode_string exp inp =
  assert_equal ~printer:(fun x -> x) exp (Unidecode.decode_string inp)

let _ =
  run_test_tt_main begin
    "Unidecode" >:::

    [ "decode_string french" >:: begin fun _ ->
        test_decode_string french_expected french_input
        end

    ; "decode_string vietnamese" >:: begin fun _ ->
        test_decode_string vietnamese_expected vietnamese_input
      end

    ; "decode_string russian" >:: begin fun _ ->
        test_decode_string russian_expected russian_input
      end

    ]
  end
