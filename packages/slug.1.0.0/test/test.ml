open Slug 

let expect_string expect equal =
  Alcotest.(check string) "same string" equal expect

let replace_whitespaces () =
  expect_string (slugify "foo bar baz") "foo-bar-baz";
  expect_string (slugify ~sep:"_" "foo bar baz") "foo_bar_baz" 

let no_duplicated_seps () = 
  expect_string (slugify "foo , bar")  "foo-bar"

let remove_non_alpha_num () = 
  expect_string (slugify "foo] - -- [ bar") "foo-bar"

let retain_upper_cases () = 
  expect_string (slugify "foo , bar") "foo-bar"

let locale () =
  expect_string (
    slugify
      ~charmap:(Charmap.mk_charmap [ Slug_data.base; Slug_data.vi ])
      "Việt Nam"
  ) "viet-nam"

let () =
  let open Alcotest in
  run "Slug" [
    "slugify", [
        test_case "replace whitespaces" `Quick replace_whitespaces;
        test_case "no duplicated seps" `Quick no_duplicated_seps;
        test_case "remote non alphabets and numbers characters" `Quick remove_non_alpha_num;
        test_case "retain word cases" `Quick retain_upper_cases;
        test_case "custom map (locale)" `Quick locale;
      ];
  ]