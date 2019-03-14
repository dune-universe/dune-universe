open OUnit2

open Uritemplate

let assert_string_equal = assert_equal ~printer:(fun a -> a)

(* Test Fixture *)
let test_fixture = "UriTemplate" >::: [
    "README samples" >::: [
      test_case (fun _ ->
          assert_string_equal
            "https://example.com/a/b?b=b#e,f"
            (template_uri ~template:"https://example.com{/a,b}{?b}{#e,f}" ~variables:[("a", "a"); ("b", "b"); ("e", "e"); ("f", "f")])
        )
    ];

    "Level 1 Examples" >::: (
      let variables = [
        ("var", "value");
        ("hello", "Hello World!");
      ] in
      List.map
        (fun (template, expected) ->
           template >:: (fun _ -> assert_string_equal expected (template_uri ~template ~variables))
        )
        [
          ("{var}", "value");
          ("{hello}", "Hello%20World%21");
        ]
    );
    "Level 2 Examples" >::: (
      let variables = [
        ("var", "value");
        ("hello", "Hello World!");
        ("path", "/foo/bar");
      ] in
      List.map
        (fun (template, expected) ->
           template >:: (fun _ -> assert_string_equal expected (template_uri ~template ~variables))
        )
        [
          ("{+var}", "value");
          ("{+hello}", "Hello%20World!");
          ("{+path}/here", "/foo/bar/here");
          ("here?ref={+path}", "here?ref=/foo/bar");
        ]
    );
    "Level 3 Examples" >::: (
      let variables = [
        ("var",   "value");
        ("hello", "Hello World!");
        ("empty", "");
        ("path",  "/foo/bar");
        ("x",     "1024");
        ("y",     "768")
      ] in
      List.map
        (fun (template, expected) ->
           template >:: (fun _ -> assert_string_equal expected (template_uri ~template ~variables))
        )
        [
          ("map?{x,y}", "map?1024,768");
          ("{x,hello,y}", "1024,Hello%20World%21,768");
          ("{+x,hello,y}", "1024,Hello%20World!,768");
          ("{+path,x}/here", "/foo/bar,1024/here");
          ("{#x,hello,y}", "#1024,Hello%20World!,768");
          ("{#path,x}/here", "#/foo/bar,1024/here");
          ("X{.var}", "X.value");
          ("X{.x,y}", "X.1024.768");
          ("{/var}", "/value");
          ("{/var,x}/here", "/value/1024/here");
          ("{;x,y}", ";x=1024;y=768");
          ("{;x,y,empty}", ";x=1024;y=768;empty");
          ("{?x,y}", "?x=1024&y=768");
          ("{?x,y,empty}", "?x=1024&y=768&empty=");
          ("?fixed=yes{&x}", "?fixed=yes&x=1024");
          ("{&x,y,empty}", "&x=1024&y=768&empty=");
        ]
    );
  ]


let _ = run_test_tt_main test_fixture
