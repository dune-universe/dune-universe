module Decoder = Json_decoder

let check_result checkable label got expected () =
  Alcotest.(check @@ result checkable string)
    label
    got
    expected

let test_case
    ~label
    ?(speed=`Quick)
    ?(descr="")
    checkable
    got
    expected =
  Alcotest.(
    test_case
      label
      speed
      (check_result checkable descr got expected)
  )

let succeed
    ?(speed=`Quick)
    ?(descr="")
    checkable
    decoder
    source
    output =
  test_case
    ~speed
    ~descr
    checkable
    Decoder.(decode_string decoder source)
    (Result.Ok output)

let primitives =
  [ succeed
      ~label:"int"
      ~descr:"decode a natural int"
      Alcotest.int
      Decoder.int
      "23" 23
  ; succeed
      ~label:"int"
      ~descr:"decode a negative int"
      Alcotest.int
      Decoder.int
      "-46" ~-46
  ; succeed
      ~label:"float"
      ~descr:"decode a float with some precision"
      Alcotest.(float 1.)
      Decoder.float
      "2.3" 2.3
  ; succeed
      ~label:"string"
      ~descr:"decodes a plain string"
      Alcotest.string
      Decoder.string
      "\"twenty-three\""
      "twenty-three"
  ; succeed
      ~label:"true"
      ~descr:"decodes a true bool"
      Alcotest.bool
      Decoder.bool
      "true" true
  ; succeed
      ~label:"false"
      ~descr:"decodes a false bool"
      Alcotest.bool
      Decoder.bool
      "false" false
  ]

let containers =
  [ succeed
      ~label:"int list index"
      Alcotest.int
      Decoder.(index 1 int)
      "[1,48,3]" 48
  ; succeed
      ~label:"dict field"
      Alcotest.(float 1.)
      Decoder.(field "lat" float)
      "{\"lat\": 52.3}" 52.3
  ; test_case
      ~label:"dict field"
      Alcotest.(float 1.)
      Decoder.(decode_string (field "lng" float) "{\"lat\": 52.3}")
      ( Result.Error "key lng does not exist in object lat " )
  ; succeed
      ~label:"infix dict selector"
      ~descr:"select an item inside an object with infix field selector"
      Alcotest.string
      Decoder.("name" @= string)
      "{\"name\": \"Frederick\"}"
      "Frederick"
  ; succeed
      ~label:"list"
      Alcotest.(list int)
      Decoder.(list int)
      "[1,48,3]" [1;48;3]
  ; succeed
      ~label:"array"
      Alcotest.(array int)
      Decoder.(array int)
      "[1,48,3]" [|1;48;3|]
  ; succeed
      ~label:"pairs"
      Alcotest.(list (pair string int))
      Decoder.(pairs int)
      "{\"lat\": 5, \"lng\": 15}" ["lat",5; "lng",15]
  ]

let inconsistents =
  [ succeed
      ~label:"optional"
      ~descr:"decode a value if it exists"
      Alcotest.(option string)
      Decoder.(option @@ "name" @= string)
      "{\"name\": \"Frederick\", \"age\": 32}"
      (Some "Frederick")
  ; succeed
      ~label:"one_of"
      ~descr:"try a set of decoders"
      Alcotest.(list string)
      Decoder.(
        list @@
        one_of
          [ string
          ; mapN (Printf.sprintf "%s %s")
            ||> "name" @= string
            ||> "last_name" @= string
          ; "name" @= string
          ]
      )
      "[
      \"Jennifer\",
      {\"name\": \"Christine\", \"last_name\": \"Ableton\"},
      {\"name\": \"Frederick\", \"age\": 32}
      ]
      "
      ["Jennifer"; "Christine Ableton"; "Frederick"]
  ; succeed
      ~label:"one_of with errors ignored"
      ~descr:"silently drop badly decoded fields"
      Alcotest.(list string)
      Decoder.(
        list @@
        one_of
          [ string
          ; mapN (Printf.sprintf "%s %s")
            ||> "name" @= string
            ||> "last_name" @= string
          ; "name" @= string
          ]
      )
      "[
      \"Jennifer\",
      {\"name\": \"Christine\", \"last_name\": \"Ableton\"},
      {\"name\": \"Frederick\", \"age\": 32}
      ]
      "
      ["Jennifer"; "Christine Ableton"; "Frederick"]
  ]

let combinators =
  [ succeed
      ~label:"map decoded value"
      Alcotest.string
      Decoder.(map string_of_int int)
      "32" "32"
  ; succeed
      ~label:"mapN"
      Alcotest.string
      Decoder.(
        mapN (Printf.sprintf "{lat:%d, lng:%d}")
        ||> "lat" @= int
        ||> "lng" @= int
      )
      "{\"lat\": 5, \"lng\": 15}"
      "{lat:5, lng:15}"
  ; succeed
      ~label:"and_then"
      ~descr:"select a second field when an initial one is present"
      Alcotest.int
      Decoder.("name" @= string >>= fun _ -> "age" @= int)
      "{\"name\": \"Frederick\", \"age\": 32}"
      32
  ]

let () =
  Alcotest.run "json_decoder"
    [ "primitives", primitives
    ; "containers", containers
    ; "inconsistent structure", inconsistents
    ; "combinators", combinators
    ]
