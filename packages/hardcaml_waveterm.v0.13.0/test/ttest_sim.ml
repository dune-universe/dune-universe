open! Import

let testbench = lazy (Example.testbench ())

let test ?display_rules ?display_width ?display_height ?wave_width ?wave_height () =
  Waveform.print
    (Lazy.force testbench)
    ?display_rules
    ?display_width
    ?display_height
    ?wave_width
    ?wave_height
;;

let%expect_test "default" =
  test ();
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0000   │0017   │002D                              │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0000           │0018   │002E                      │
    │               ││────────────────┴───────┴───────────────           │
    │vdd            ││────────────────────────────────────────           │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "display height" =
  test () ~display_height:6;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "display width" =
  test () ~display_width:40;
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │clk     ││┌───┐   ┌───┐   ┌───┐   ┌───│
    │        ││    └───┘   └───┘   └───┘   │
    │clr     ││────────┐                   │
    │        ││        └───────────────────│
    │        ││────────┬───────┬───────────│
    │a       ││ 0000   │0017   │002D       │
    │        ││────────┴───────┴───────────│
    │        ││────────────────┬───────┬───│
    │b       ││ 0000           │0018   │002│
    │        ││────────────────┴───────┴───│
    │vdd     ││────────────────────────────│
    │        ││                            │
    │        ││                            │
    │        ││                            │
    │        ││                            │
    │        ││                            │
    │        ││                            │
    │        ││                            │
    └────────┘└────────────────────────────┘ |}]
;;

let%expect_test "wave height" =
  test () ~wave_height:0 ~display_height:11;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │a              ││────────┬───────┬───────────────────────           │
    │               ││────────┴───────┴───────────────────────           │
    │b              ││────────────────┬───────┬───────────────           │
    │               ││────────────────┴───────┴───────────────           │
    │vdd            ││────────────────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "negative wave width" =
  test () ~wave_width:(-1) ~display_height:13;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │               ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clr            ││─┐                                                 │
    │               ││ └───                                              │
    │               ││─┬┬──                                              │
    │a              ││ ││0.                                              │
    │               ││─┴┴──                                              │
    │               ││──┬┬─                                              │
    │b              ││ .││.                                              │
    │               ││──┴┴─                                              │
    │vdd            ││─────                                              │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "display rules" =
  let module Rule = Display_rules.Rule in
  let display_rules =
    [ Rule.port_name_is "clk" ~wave_format:Binary
    ; Rule.port_name_matches
        (Re.Posix.compile (Re.Posix.re ".d.*"))
        ~wave_format:Unsigned_int
    ; Rule.port_name_is_one_of [ "b"; "a" ] ~wave_format:Int
    ; Rule.port_name_is
        "clr"
        ~wave_format:(Custom (fun b -> if Bits.to_int b = 1 then "clear" else "run"))
    ]
  in
  let display_rules = Display_rules.of_list display_rules in
  print_s [%message "" (display_rules : Display_rules.t)];
  [%expect
    {|
    (display_rules (
      (Names
        (names (clk))
        (wave_format Binary)
        (alignment   Left))
      (Regexp
        (re          <opaque>)
        (wave_format Unsigned_int)
        (alignment   Left))
      (Names
        (names (b a))
        (wave_format Int)
        (alignment   Left))
      (Names (names (clr)) (wave_format (Custom <fun>)) (alignment Left)))) |}];
  test () ~display_rules ~display_height:16;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────────────────────────────────────           │
    │vdd            ││ 1                                                 │
    │               ││────────────────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0      │23     │45                                │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0              │24     │46                        │
    │               ││────────────────┴───────┴───────────────           │
    │               ││────────┬───────────────────────────────           │
    │clr            ││ clear  │run                                       │
    │               ││────────┴───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "config with outputs then inputs" =
  let names (module X : Interface.S) = X.t |> X.map ~f:fst |> X.to_list in
  let map_format wave_format =
    List.map ~f:(fun name -> Display_rules.Rule.port_name_is name ~wave_format)
  in
  let o = names (module Example.O) |> map_format Unsigned_int |> Display_rules.of_list in
  let i = names (module Example.I) |> map_format Hex |> Display_rules.of_list in
  let display_rules = Display_rules.combine ~above:o ~below:i in
  test () ~display_rules ~display_height:13;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0              │24     │46                        │
    │               ││────────────────┴───────┴───────────────           │
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────┬───────────────────────────────           │
    │clr            ││ 1      │0                                         │
    │               ││────────┴───────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0000   │0017   │002D                              │
    │               ││────────┴───────┴───────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "single bits" =
  let display_rules =
    Display_rules.(of_list [ Rule.port_name_is "clr" ~wave_format:Binary ])
  in
  test () ~display_rules ~display_height:5;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────┬───────────────────────────────           │
    │clr            ││ 1      │0                                         │
    │               ││────────┴───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}];
  let display_rules =
    Display_rules.(of_list [ Rule.port_name_is "clr" ~wave_format:Bit ])
  in
  test () ~display_rules ~display_height:4;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Bit_or constructor" =
  let display_rules =
    Display_rules.(
      of_list
        [ Rule.port_name_is "clr" ~wave_format:(Bit_or Hex)
        ; Rule.port_name_is "a" ~wave_format:(Bit_or Hex)
        ; Rule.port_name_is "b" ~wave_format:(Bit_or Unsigned_int)
        ])
  in
  test () ~display_rules ~display_height:10;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0000   │0017   │002D                              │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0              │24     │46                        │
    │               ││────────────────┴───────┴───────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Alignment" =
  let display_rules alignment =
    Display_rules.(
      of_list
        [ Rule.port_name_is "clr" ~wave_format:(Bit_or Hex)
        ; Rule.port_name_is "a" ~wave_format:Hex ~alignment
        ; Rule.port_name_is "b" ~wave_format:(Bit_or Unsigned_int)
        ])
  in
  test () ~display_rules:(display_rules Right) ~display_height:10 ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────┐                                              │
    │               ││    └───────────────                               │
    │               ││────┬───┬───────────                               │
    │a              ││ .00│.17│002D                                      │
    │               ││────┴───┴───────────                               │
    │               ││────────┬───┬───────                               │
    │b              ││ 0      │24 │46                                    │
    │               ││────────┴───┴───────                               │
    └───────────────┘└───────────────────────────────────────────────────┘ |}];
  test () ~display_rules:(display_rules Left) ~display_height:10 ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────┐                                              │
    │               ││    └───────────────                               │
    │               ││────┬───┬───────────                               │
    │a              ││ 00.│00.│002D                                      │
    │               ││────┴───┴───────────                               │
    │               ││────────┬───┬───────                               │
    │b              ││ 0      │24 │46                                    │
    │               ││────────┴───┴───────                               │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "minimum display size" =
  test () ~display_width:7 ~display_height:3;
  [%expect {|
    ┌S┐┌Wa┐
    │c││┌─│
    └─┘└──┘ |}]
;;

let%expect_test "configuration exceptions" =
  show_raise (fun () -> test () ~wave_height:(-1));
  [%expect {| (raised ("Invalid wave height.  Must be >= 0." (wave_height -1))) |}];
  show_raise (fun () -> test () ~display_height:2);
  [%expect {| (raised ("Invalid display height.  Must be >= 3." (display_height 2))) |}];
  show_raise (fun () -> test () ~display_width:6);
  [%expect {| (raised ("Invalid display width.  Must be >= 7." (display_width 6))) |}]
;;
