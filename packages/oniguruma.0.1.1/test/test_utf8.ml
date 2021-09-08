let test_search =
  Test_util.test_search
    Oniguruma.Options.none Oniguruma.Options.none Oniguruma.Encoding.utf8
let neg_test_search =
  Test_util.neg_test_search
    Oniguruma.Options.none Oniguruma.Options.none Oniguruma.Encoding.utf8
let test_search_out_of_bounds =
  Test_util.test_search_out_of_bounds
    Oniguruma.Options.none Oniguruma.Options.none Oniguruma.Encoding.utf8
let test_match =
  Test_util.test_match
    Oniguruma.Options.none Oniguruma.Options.none Oniguruma.Encoding.utf8
let neg_test_match =
  Test_util.neg_test_match
    Oniguruma.Options.none Oniguruma.Options.none Oniguruma.Encoding.utf8
let test_match_out_of_bounds =
  Test_util.test_match_out_of_bounds
    Oniguruma.Options.none Oniguruma.Options.none Oniguruma.Encoding.utf8

let () =
  test_search "a|b" "a" [0, 1];
  test_search "あ" "あ" [0, 3];
  test_search "あ*" "" [0, 0];
  test_search "あ*" "あ" [0, 3];
  test_search "あ*" "ああ" [0, 6];
  test_search "あ|a" "a" [0, 1];
  test_search "あ|a" "あ" [0, 3];
  test_search "a" "あa" [3, 4];
  test_search "大?好き" "好き" [0, 6];
  test_search "大?好き" "大好き" [0, 9];
  test_search "大.+" "大好き" [0, 9];
  test_search "大.+" "大学" [0, 6];
  test_search "大.+" "大人" [0, 6];
  neg_test_search "あ" "a";
  neg_test_search "a" "あ";
  neg_test_search "大.+" "大";
  test_match "a" 3 "あa" [3, 4];
  test_match "い" 3 "あいう" [3, 6];
  test_match "好き" 3 "大好き" [3, 9];
  test_match "大?好き" 0 "好き" [0, 6];
  test_match "大?好き" 0 "大好き" [0, 9];
  test_match "大.+" 0 "大好き" [0, 9];
  test_match "大.+" 0 "大学" [0, 6];
  test_match "大.+" 0 "大人" [0, 6];
  neg_test_match "あa" 3 "あa";
  test_search_out_of_bounds "a" "a" 1 2;
  test_search_out_of_bounds "a" "a" (-1) 1
