(** MeCab --- A MeCab binding for OCaml

    Copyright (c) 2017 Akinori ABE

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE. *)

open OUnit2
open Test_utils
open Mecab.Unicode.UTF8

let test_trim ctxt =
  let expected = "こんにちは" in
  let actual = trim " 　こんにちは " in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "" in
  let actual = trim " 　 " in
  assert_equal ~ctxt ~printer:string expected actual

let test_uppercase_ascii ctxt =
  let expected = "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234569" in
  let actual = uppercase_ascii "abcdefghijklmnopqrstuvwxyz01234569" in
  assert_equal ~ctxt ~printer:string expected actual

let test_lowercase_ascii ctxt =
  let expected = "abcdefghijklmnopqrstuvwxyz01234569" in
  let actual = lowercase_ascii "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234569" in
  assert_equal ~ctxt ~printer:string expected actual

let test_latinize_number_form ctxt =
  let expected = "1/7 1/ I M i m 0/3 2 3" in
  let actual = latinize_number_form "⅐ ⅟ Ⅰ Ⅿ ⅰ ⅿ ↉ ↊ ↋" in
  assert_equal ~ctxt ~printer:string expected actual

let test_latinize_enclosed_alpha ctxt =
  let expected = "(1) (20) (1) (20) 1. 20. (a) (z) (A) (Z) (a) (z) \
                  (0) (11) (20) (1) (10) (0)" in
  let actual = latinize_enclosed_alpha "① ⑳ ⑴ ⒇ ⒈ ⒛ ⒜ ⒵ Ⓐ Ⓩ ⓐ ⓩ ⓪ ⓫ ⓴ ⓵ ⓾ ⓿" in
  assert_equal ~ctxt ~printer:string expected actual

let test_latinize_blank ctxt =
  let expected = "               " in
  let actual = latinize_blank
      "\t \194\160\226\128\130\226\128\131\226\128\132\
       \226\128\133\226\128\134\226\128\135\226\128\136\
       \226\128\137\226\128\138\226\128\139\227\128\128\239\191\175" in
  assert_equal ~ctxt ~printer:string expected actual

let test_halve_ascii ctxt =
  let expected = "0123456789" in
  let actual = halve_ascii "０１２３４５６７８９" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let actual = halve_ascii "ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "abcdefghijklmnopqrstuvwxyz" in
  let actual = halve_ascii "ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "!\"#$%&'()*+,-./:;<>?@[\\\\]^_`{|}" in
  let actual = halve_ascii "！＂＃＄％＆＇（）＊＋，－．／：；＜＞？＠［＼￥］＾＿｀｛｜｝" in
  assert_equal ~ctxt ~printer:string expected actual

let test_widen_kana ctxt =
  let expected = "ァィゥェォャュョッ。「」、・" in
  let actual = widen_kana "ｧｨｩｪｫｬｭｮｯ｡｢｣､･" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "アイウエオカキクケコサシスセソタチツテトナニヌネノ\
                  ハヒフヘホマミムメモヤユヨラリルレロワヲン" in
  let actual = widen_kana "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポ" in
  let actual = widen_kana "ｶﾞｷﾞｸﾞｹﾞｺﾞｻﾞｼﾞｽﾞｾﾞｿﾞﾀﾞﾁﾞﾂﾞﾃﾞﾄﾞﾊﾞﾋﾞﾌﾞﾍﾞﾎﾞﾊﾟﾋﾟﾌﾟﾍﾟﾎﾟ" in
  assert_equal ~ctxt ~printer:string expected actual

let test_neologd_normalize ctxt =
  let expected = "スーパー" in
  let actual = neologd_normalize "スーーーーーーパーーーーーー" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "元気?" in
  let actual = neologd_normalize "元気〜？" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "こんにちは" in
  let actual = neologd_normalize " こ ん 　 に　　ち は　" in
  assert_equal ~ctxt ~printer:string expected actual ;
  let expected = "検索エンジン自作入門を買いました!!!" in
  let actual = neologd_normalize "検索 エンジン 自作 入門 を 買い ました ！！！" in
  assert_equal ~ctxt ~printer:string  expected actual ;
  let expected = "Coding the Matrix" in
  let actual = neologd_normalize "Coding the Matrix" in
  assert_equal ~ctxt ~printer:string  expected actual ;
  let expected = "アルゴリズムC" in
  let actual = neologd_normalize "アルゴリズム C" in
  assert_equal ~ctxt ~printer:string  expected actual ;
  let expected = "南アルプスの天然水Sparking Lemonレモン一絞" in
  let actual = neologd_normalize "南アルプスの　天然水　Ｓｐａｒｋｉｎｇ　Ｌｅｍｏｎ　レモン一絞" in
  assert_equal ~ctxt ~printer:string  expected actual ;
  let expected = "この商品は3123280円です。" in
  let actual = neologd_normalize "この商品は 3,123,280 円です。" in
  assert_equal ~ctxt ~printer:string  expected actual

let suite =
  "Unicode" >::: [
    "trim" >:: test_trim;
    "uppercase_ascii" >:: test_uppercase_ascii;
    "lowercase_ascii" >:: test_lowercase_ascii;
    "latinize_number_form" >:: test_latinize_number_form;
    "latinize_enclosed_alpha" >:: test_latinize_enclosed_alpha;
    "latinize_blank" >:: test_latinize_blank;
    "halve_ascii" >:: test_halve_ascii;
    "widen_kana" >:: test_widen_kana;
    "neologd_normalize" >:: test_neologd_normalize;
  ]
