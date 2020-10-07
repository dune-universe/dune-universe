open Opam_compiler
open Helpers

let error =
  let pp_error ppf = function `Unknown -> Format.fprintf ppf "Unknown" in
  let equal_error = ( = ) in
  Alcotest.testable pp_error equal_error

let parse_tests =
  let test name s expected =
    ( name,
      `Quick,
      fun () ->
        let got = Source.parse s in
        Alcotest.(check (result (module Source) error) __LOC__ expected got) )
  in
  [
    test "full branch syntax" "user/repo:branch"
      (Ok (Github_branch { user = "user"; repo = "repo"; branch = "branch" }));
    test "branches can have dashes" "user/repo:my-great-branch"
      (Ok
         (Github_branch
            { user = "user"; repo = "repo"; branch = "my-great-branch" }));
    test "repo can be omitted and defaults to ocaml" "user:branch"
      (Ok (Github_branch { user = "user"; repo = "ocaml"; branch = "branch" }));
    test "repo with PR number" "user/repo#1234"
      (Ok (Github_PR { user = "user"; repo = "repo"; number = 1234 }));
    test "defaults to main repo" "#1234"
      (Ok (Github_PR { user = "ocaml"; repo = "ocaml"; number = 1234 }));
    test "something that does not parse" "a-random-string" (Error `Unknown);
    test "users can have dashes" "user-with-dashes/repo#1234"
      (Ok
         (Github_PR { user = "user-with-dashes"; repo = "repo"; number = 1234 }));
    test "repos can have dashes" "user/repo-with-dashes#1234"
      (Ok
         (Github_PR { user = "user"; repo = "repo-with-dashes"; number = 1234 }));
  ]

let switch_target_tests =
  let test name source ~expectations ~expected =
    ( name,
      `Quick,
      fun () ->
        let$ pr_info = Mock.create (module Pull_request) __LOC__ expectations in
        let github_client = { Github_client.pr_info } in
        let got = Source.switch_target source github_client in
        Alcotest.check Alcotest.(result string error) __LOC__ expected got )
  in
  let pr = { Pull_request.user = "USER"; repo = "REPO"; number = 1234 } in
  [
    test "PR error" (Github_PR pr)
      ~expectations:[ Mock.expect pr ~and_return:(Error `Unknown) ]
      ~expected:(Error `Unknown);
    test "PR ok" (Github_PR pr)
      ~expectations:
        [
          Mock.expect pr
            ~and_return:
              (Ok
                 {
                   Github_client.source_branch =
                     {
                       Branch.user = "SRC_USER";
                       repo = "SRC_REPO";
                       branch = "SRC_BRANCH";
                     };
                   title = "TITLE";
                 });
        ]
      ~expected:(Ok "git+https://github.com/SRC_USER/SRC_REPO#SRC_BRANCH");
    test "branch"
      (Github_branch { user = "USER"; repo = "REPO"; branch = "BRANCH" })
      ~expectations:[] ~expected:(Ok "git+https://github.com/USER/REPO#BRANCH");
  ]

let switch_description_tests =
  let test name source ~github_expectations ~expected =
    ( name,
      `Quick,
      fun () ->
        let$ pr_info =
          Mock.create (module Pull_request) __LOC__ github_expectations
        in
        let github_client = { Github_client.pr_info } in
        let got = Source.switch_description source github_client in
        Alcotest.check Alcotest.(string) __LOC__ expected got )
  in
  let pr = { Pull_request.user = "USER"; repo = "REPO"; number = 1234 } in
  [
    test "Github branch"
      (Github_branch { user = "USER"; repo = "REPO"; branch = "BRANCH" })
      ~github_expectations:[] ~expected:"[opam-compiler] USER/REPO:BRANCH";
    test "Github PR (successful)" (Github_PR pr)
      ~github_expectations:
        [
          Mock.expect pr
            ~and_return:
              (Ok
                 {
                   Github_client.title = "TITLE";
                   source_branch =
                     {
                       user = "SRC_USER";
                       repo = "SRC_REPO";
                       branch = "SRC_BRANCH";
                     };
                 });
        ]
      ~expected:"[opam-compiler] USER/REPO#1234 - TITLE";
    test "Github PR (error)" (Github_PR pr)
      ~github_expectations:[ Mock.expect pr ~and_return:(Error `Unknown) ]
      ~expected:"[opam-compiler] USER/REPO#1234";
  ]

let tests =
  [
    ("Source parse", parse_tests);
    ("Source git_url", switch_target_tests);
    ("Source switch_description", switch_description_tests);
  ]
