let errored = Alcotest.testable Rresult.R.pp_msg (fun _ _ -> true)
let str = Alcotest.testable (fun ppf -> Fmt.pf ppf "%S") String.equal
let ( <.> ) f g = fun x -> f (g x)

let valid_unstructured_string input expect =
  Alcotest.test_case (Fmt.strf "%S" expect) `Quick @@ fun () ->
  let res = let open Rresult.R in Unstrctrd.of_string input >>| fun (_, t) -> Unstrctrd.to_utf_8_string t in
  Alcotest.(check (result str errored)) "expect" res (Ok expect)

let valid_unstructured_string_without_comment input expect =
  Alcotest.test_case (Fmt.strf "%S" expect) `Quick @@ fun () ->
  let res =
    let open Rresult.R in
    Unstrctrd.of_string input
    >>= fun (_, t) -> Unstrctrd.without_comments t
    >>| Unstrctrd.to_utf_8_string in
  Alcotest.(check (result str errored)) "expect" res (Ok expect)

let split_at input index (e0, e1) =
  Alcotest.test_case (Fmt.strf "@[<1>(%S,@ %S)@]" e0 e1) `Quick @@ fun () ->
  let res =
    let open Rresult.R in
    Unstrctrd.of_string input
    >>| (fun (_, t) -> Unstrctrd.split_at ~index t)
    >>| (fun (a, b) -> Unstrctrd.(to_utf_8_string a, to_utf_8_string b)) in
  Alcotest.(check (result (pair str str) errored)) "expect" res (Ok (e0, e1))

let split_on input v expect =
  Alcotest.test_case (Fmt.strf "%S" input) `Quick @@ fun () ->
  let res =
    let open Rresult.R in
    Unstrctrd.of_string input
    >>| (fun (_, t) -> Unstrctrd.split_on ~on:v t)
    >>| (function Some (a, b) -> Some Unstrctrd.(to_utf_8_string a, to_utf_8_string b)
                | None -> None) in
  Alcotest.(check (result (option (pair str str)) errored)) "expect" res (Ok expect)

let complex_0 =
{|To:A Group(Some people)
     :Chris Jones <c@(Chris's host.)public.example>,
         joe@example.org,
  John <jdoe@one.test> (my dear friend); (the end of the group)
|}, "To:A Group :Chris Jones <c@public.example>, joe@example.org, John <jdoe@one.test> ; "

let complex_1 =
{|To    : Mary Smith
  
          <mary@example.net>
|}, "To    : Mary Smith  <mary@example.net>"

let complex_2 =
{|Date: Thu,
      13
        Feb
          1969
      23:32
               -0330 (Newfoundland Time)
|}, "Date: Thu, 13 Feb 1969 23:32 -0330 "

let complex (input, expect) =
  Alcotest.test_case (Fmt.strf "%S" expect) `Quick @@ fun () ->
  let fws = function `FWS _ -> Unstrctrd.wsp ~len:1 | x -> x in
  let res =
    let open Rresult.R in
    Unstrctrd.of_string input
    >>| (fun (_, t) -> Unstrctrd.map ~f:fws t)
    >>= Unstrctrd.without_comments
    >>| Unstrctrd.to_utf_8_string in
  Alcotest.(check (result str errored)) "expect" res (Ok expect)

let () =
  Alcotest.run "unstrctrd"
    [ "valid", [ valid_unstructured_string "Hello\r\n" "Hello"
               ; valid_unstructured_string "Hello\r\n World\r\n" "Hello\r\n World"
               ; valid_unstructured_string "Hello\r\n \r\n \r\n World\r\n" "Hello\r\n \r\n \r\n World"
               ; valid_unstructured_string " \r\n \r\n" " \r\n "
               ; valid_unstructured_string "\r\nHello World!" ""
               ; valid_unstructured_string " \r\nHello World!" " "
               ; valid_unstructured_string " \r\n \r\nHello World!" " \r\n "
               ; valid_unstructured_string "Hello\r\nWorld\r\n" "Hello"
               ; valid_unstructured_string "\r\n" ""
               ; valid_unstructured_string "\r\r\n" "\r"
               ; valid_unstructured_string "\r\r\r\n" "\r\r"
               ; valid_unstructured_string "\r\n \r\r\n" "\r\n \r"
               ; valid_unstructured_string "\n\r\n" "\n"
               ; valid_unstructured_string "\n\n\r\n" "\n\n"
               ; valid_unstructured_string "\n\n\r\n \r\n" "\n\n\r\n "
               ; valid_unstructured_string "\n\r\n Hello\r\n World\r\n !\r\n" "\n\r\n Hello\r\n World\r\n !" ]
    ; "comments", [ valid_unstructured_string_without_comment "Hello(World)\r\n" "Hello"
                  ; valid_unstructured_string_without_comment "Hello (a\r\n b)World!\r\n" "Hello World!"
                  ; valid_unstructured_string_without_comment "(a\r\n (b \r\n c))\r\n" ""
                  ; valid_unstructured_string_without_comment "(a)(b)Hello(c)\r\n" "Hello"
                  ; valid_unstructured_string_without_comment "\\(Hello\\)\r\n" "(Hello)"
                  ; valid_unstructured_string_without_comment "(Hi! \\) hidden)Hello\r\n" "Hello" ]
    ; "complex", [ complex complex_0
                 ; complex complex_1
                 ; complex complex_2 ]
    ; "split_at", [ split_at "\r\n" 0 ("", "")
                  ; split_at "Hello\r\n" 0 ("", "Hello")
                  ; split_at "Hello\r\n World!\r\n" 5 ("Hello", "\r\n World!") ]
    ; "split_on", [ split_on "\r\n" `FWS None
                  ; split_on "Hello\r\n" `FWS None
                  ; split_on "Hello\r\n World!\r\n" `FWS (Some ("Hello", "World!"))
                  ; split_on "Hello World!\r\n" `WSP (Some ("Hello", "World!")) ] ]
