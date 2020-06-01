open! Core
open! Async
open! Import
open Text_block
module Expect_test_config = Core.Expect_test_config

let yoyoma : t list = [ text "yo"; text "yo"; text "ma" ]

let test t =
  invariant t;
  print_endline (render t)
;;

let example =
  let return_address =
    vcat
      [ text "Kel Varnsen"
      ; text "Vandelay Industries"
      ; text "67 Lantern Dr."
      ; text "Brooklyn, NY 11224"
      ; vsep
      ; text "August 3, 1998"
      ]
  in
  let salutation =
    vcat
      [ text "Sincerely,"
      ; vstrut 4
      ; text "Kel Varnsen"
      ; text "Chief Procurement Officer"
      ]
  in
  let [ return_address; salutation ] =
    With_static_lengths.halign `Left [ return_address; salutation ]
  in
  vcat
    ~align:`Right
    [ return_address
    ; vstrut 4
    ; vcat
        [ text "H.E. Pennypacker"
        ; text "Kramerica Industries"
        ; text "129 W 81st St, Apt 5B"
        ; text "Manhattan, NY 10024"
        ; vsep
        ; text "Dear Mr. Pennypacker:"
        ; vsep
        ; text
            "It has come to my attention that your revolutionary oil tanker\n\
             bladder system makes extensive use of latex and latex products."
        ; vsep
        ; text
            "We at Vandelay Industries are happy to supply you these materials\n\
             at a discounted rate. If you would like to pursue this matter,\n\
             please contact our head of sales, George Costanza at 555-6893."
        ]
    ; vsep
    ; salutation
    ]
;;

let%expect_test "example" =
  print_endline (render example);
  [%expect
    {|
                                            Kel Varnsen
                                            Vandelay Industries
                                            67 Lantern Dr.
                                            Brooklyn, NY 11224

                                            August 3, 1998




    H.E. Pennypacker
    Kramerica Industries
    129 W 81st St, Apt 5B
    Manhattan, NY 10024

    Dear Mr. Pennypacker:

    It has come to my attention that your revolutionary oil tanker
    bladder system makes extensive use of latex and latex products.

    We at Vandelay Industries are happy to supply you these materials
    at a discounted rate. If you would like to pursue this matter,
    please contact our head of sales, George Costanza at 555-6893.

                                            Sincerely,




                                            Kel Varnsen
                                            Chief Procurement Officer
  |}]
;;

let%expect_test _ =
  test (hcat yoyoma);
  [%expect {|
    yoyoma
  |}]
;;

let%expect_test _ =
  test (hcat ~sep:(hstrut 1) yoyoma);
  [%expect {|
    yo yo ma
  |}]
;;

let%expect_test _ =
  test (hcat ~sep:(hstrut 2) yoyoma);
  [%expect {|
    yo  yo  ma
  |}]
;;

let%expect_test _ =
  test (vcat yoyoma);
  [%expect {|
    yo
    yo
    ma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep:(vstrut 1) yoyoma);
  [%expect {|
    yo

    yo

    ma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep:(vstrut 2) yoyoma);
  [%expect {|
    yo


    yo


    ma
  |}]
;;

let sep = text "."

let%expect_test _ =
  test (hcat ~sep [ vcat yoyoma; hcat yoyoma ]);
  [%expect {|
    yo.yoyoma
    yo
    ma
  |}]
;;

let%expect_test _ =
  test (hcat ~sep [ hcat yoyoma; vcat yoyoma ]);
  [%expect {|
    yoyoma.yo
           yo
           ma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep [ vcat yoyoma; hcat yoyoma ]);
  [%expect {|
    yo
    yo
    ma
    .
    yoyoma
  |}]
;;

let%expect_test _ =
  test (vcat ~sep [ hcat yoyoma; vcat yoyoma ]);
  [%expect {|
    yoyoma
    .
    yo
    yo
    ma
  |}]
;;

let%expect_test "word wrap" =
  let test ~width str =
    let t = text ~max_width:width str in
    let vline = fill '|' ~width:1 ~height:(height t) in
    let hline = hcat [ text "+"; fill '-' ~width ~height:1; text "+" ] in
    test (vcat [ hline; hcat [ vline; vcat [ hstrut width; t ]; vline ]; hline ])
  in
  test
    ~width:30
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \
     exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \
     irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \
     pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui \
     officia deserunt mollit anim id est laborum.";
  [%expect
    {|
    +------------------------------+
    |Lorem ipsum dolor sit amet,   |
    |consectetur adipiscing elit,  |
    |sed do eiusmod tempor         |
    |incididunt ut labore et dolore|
    |magna aliqua. Ut enim ad minim|
    |veniam, quis nostrud          |
    |exercitation ullamco laboris  |
    |nisi ut aliquip ex ea commodo |
    |consequat. Duis aute irure    |
    |dolor in reprehenderit in     |
    |voluptate velit esse cillum   |
    |dolore eu fugiat nulla        |
    |pariatur. Excepteur sint      |
    |occaecat cupidatat non        |
    |proident, sunt in culpa qui   |
    |officia deserunt mollit anim  |
    |id est laborum.               |
    +------------------------------+
  |}];
  test
    ~width:33
    "(Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor\n\
    \ incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud\n\
    \ exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute\n\
    \ irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla\n\
    \ pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui\n\
    \ officia deserunt mollit anim id est laborum.)";
  [%expect
    {|
    +---------------------------------+
    |(Lorem ipsum dolor sit amet,     |
    |consectetur adipiscing elit, sed |
    |do eiusmod tempor incididunt ut  |
    |labore et dolore magna aliqua. Ut|
    |enim ad minim veniam, quis       |
    |nostrud exercitation ullamco     |
    |laboris nisi ut aliquip ex ea    |
    |commodo consequat. Duis aute     |
    |irure dolor in reprehenderit in  |
    |voluptate velit esse cillum      |
    |dolore eu fugiat nulla pariatur. |
    |Excepteur sint occaecat cupidatat|
    |non proident, sunt in culpa qui  |
    |officia deserunt mollit anim id  |
    |est laborum.)                    |
    +---------------------------------+
  |}]
;;

(* lines with trailing whitespace used to tickle a bug *)

let%expect_test _ =
  test (vcat [ hcat [ text "a"; text " " ]; hcat [ text "b" ] ]);
  [%expect {|
    a
    b
  |}]
;;

let%expect_test _ =
  test (vcat [ hcat [ text "a"; text "    " ]; hcat [ text "b" ] ]);
  [%expect {|
    a
    b
  |}]
;;

let yellow = ansi_escape ~prefix:"[33m" ~suffix:"[39m"

let%expect_test _ =
  test (yellow (vcat yoyoma));
  [%expect {|
    [33myo[39m
    [33myo[39m
    [33mma[39m
  |}]
;;
