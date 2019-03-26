open OUnit2

open Uritemplate

let assert_string_equal = assert_equal ~printer:(fun a -> a)


let test_list_of_examples ~variables ~cases =
  List.map
    (fun (template, expected) ->
       template >:: (fun _ ->
           let result = template_uri ~template ~variables in
           match expected with
           | `String expected -> assert_string_equal expected result
           | `List lst -> (
               match (List.exists ((=) result) lst)
               with
               | false -> assert_failure ("expected one of: [" ^ List.fold_left (fun s a -> a ^ " " ^ s) "" lst ^ "] \nbut got " ^ result)
               | true -> ()
             )
         )
    ) cases


(* Test Fixture *)
let test_fixture = "UriTemplate" >::: [
    "README samples" >::: [
      test_case (fun _ ->
          assert_string_equal
            "https://example.com/a/b?b=b#e,f"
            (template_uri_with_strings
               ~template:"https://example.com{/a,b}{?b}{#e,f}"
               ~variables:[("a",  "a"); ("b",  "b"); ("e",  "e"); ("f", "f")])
        )
    ];

    "Tests with the prefix modifier (:)" >::: (
      test_list_of_examples
        ~variables:[
          ("var", `String "value");
          ("hello", `String "Hello World!");
          ("path", `String "/foo/bar");
        ]
        ~cases:[
          ("{var:3}", `String "val");
          ("{var:30}", `String "value");
          ("{+path:6}/here", `String "/foo/b/here");
          ("{#path:6}/here", `String "#/foo/b/here");
          ("{;hello:5}", `String ";hello=Hello");
          ("{;hello:5}", `List [";hello=Hello"]);
        ]
    );

    "Tests with a list attached to a variable" >::: (
      test_list_of_examples
        ~variables:[
          ("var", `String "value");
          ("hello", `String "Hello World!");
          ("path", `String "/foo/bar");
          ("list", `List ["red"; "green"; "blue"]);
        ]
        ~cases:[
          ("{list}", `String "red,green,blue");
          ("{+list}", `String "red,green,blue");
          ("{#list}", `String "#red,green,blue");
          ("X{.list}", `String "X.red,green,blue");
          ("{/list}", `String "/red,green,blue");
          ("{/list,path:4}", `String "/red,green,blue/%2Ffoo");
          ("{;list}", `String ";list=red,green,blue");
          ("{?list}", `String "?list=red,green,blue");
          ("{&list}", `String "&list=red,green,blue");
        ]
    );

    "Example tests" >::: (
      let open Example_tests_t in
      List.map (fun (name, { variables; testcases; _ }) ->
          name >::: test_list_of_examples ~variables ~cases:testcases
        ) (Example_tests_j.file_of_string Samples.json)
    )
  ]


let _ = run_test_tt_main test_fixture
