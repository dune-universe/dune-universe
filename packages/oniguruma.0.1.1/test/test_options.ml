let test_search_ascii coptions soptions =
  Test_util.test_search coptions soptions Oniguruma.Encoding.ascii
let neg_test_search_ascii coptions soptions =
  Test_util.neg_test_search coptions soptions Oniguruma.Encoding.ascii
let test_search_utf8 coptions soptions =
  Test_util.test_search coptions soptions Oniguruma.Encoding.utf8
let neg_test_search_utf8 coptions soptions =
  Test_util.neg_test_search coptions soptions Oniguruma.Encoding.utf8
let test_match_ascii coptions soptions =
  Test_util.test_match coptions soptions Oniguruma.Encoding.ascii
let neg_test_match_ascii coptions soptions =
  Test_util.neg_test_match coptions soptions Oniguruma.Encoding.ascii

let () =
  let open Oniguruma.Options in
  test_search_ascii multiline none "." "\n" [0, 1];
  neg_test_search_ascii none none "." "\n";

  test_search_ascii ignorecase none "a" "A" [0, 1];
  neg_test_search_ascii none none "a" "A";
  test_search_ascii ignorecase none "A" "a" [0, 1];

  neg_test_search_ascii (none <+> none) none "ML" "OCaml";
  test_search_ascii ignorecase none "ML" "OCaml" [3, 5];
  test_search_ascii ignorecase none "ml" "OCaml" [3, 5];
  test_search_ascii (none <+> ignorecase) none "ML" "SML" [1, 3];
  test_search_ascii (ignorecase <+> none) none "ml" "SML" [1, 3];

  neg_test_search_ascii none none "A" "a";
  test_search_utf8 ignorecase none "a" "A" [0, 1];
  neg_test_search_utf8 none none "a" "A";
  test_search_utf8 ignorecase none "A" "a" [0, 1];
  neg_test_search_utf8 none none "A" "a";

  test_match_ascii none none "^" 0 "" [0, 0];
  test_match_ascii none (none <+> none) "^" 0 "" [0, 0];
  neg_test_match_ascii none none "^" 1 "a";
  neg_test_match_ascii none notbol "^" 0 "";
  test_match_ascii none noteol "^" 0 "" [0, 0];
  neg_test_match_ascii none (notbol <+> noteol) "^" 0 "";
  neg_test_match_ascii none (none <+> noteol <+> notbol) "^" 0 "";

  test_match_ascii none none "$" 0 "" [0, 0];
  test_match_ascii none notbol "$" 0 "" [0, 0];
  neg_test_match_ascii none noteol "$" 0 "";
  neg_test_match_ascii none (notbol <+> noteol) "$" 0 "";
  neg_test_match_ascii none (noteol <+> notbol) "$" 0 "";

  test_match_ascii none none "$" 1 "a" [1, 1];
  neg_test_match_ascii none noteol "$" 1 "a";
  neg_test_match_ascii none (notbol <+> noteol) "$" 1 "a";
  neg_test_match_ascii none (noteol <+> notbol) "$" 1 "a";

  test_match_ascii none none "^$" 0 "" [0, 0];
  neg_test_match_ascii none (notbol <+> noteol) "^$" 0 "";

  test_match_ascii none none "^a$" 0 "a" [0, 1];
  neg_test_match_ascii none notbol "^a$" 0 "a";
  neg_test_match_ascii none noteol "^a$" 0 "a";
  neg_test_match_ascii none (notbol <+> noteol) "^a$" 0 "a";
  neg_test_match_ascii none (noteol <+> notbol) "^a$" 0 "a"
