open Webtest.Suite
open Js_of_ocaml
open Jsonoo

let equal x y = Jsonoo.stringify x = Jsonoo.stringify y

let test_stringify () =
  assert_equal ~label:"null" "null" @@ Jsonoo.stringify Encode.null

let test_try_parse_opt () =
  assert_equal ~label:"some" (Some (Encode.int 1))
  @@ Jsonoo.try_parse_opt {| 1 |};
  assert_equal ~label:"none" None @@ Jsonoo.try_parse_opt {| [ |}

let test_try_parse_exn () =
  assert_equal ~label:"no exception" (Encode.int 1)
  @@ Jsonoo.try_parse_exn {| 1 |};
  assert_raises
    ~label:"exception"
    (Decode_error "Failed to parse JSON string \" [ \"")
    (fun () -> ignore @@ Jsonoo.try_parse_exn {| [ |})

module TestEncode = struct
  open Jsonoo.Encode

  let test_id () = assert_equal ~label:"id" ~equal null @@ id null

  let test_null () =
    assert_equal ~label:"null" ~equal (Obj.magic Js.null : t) @@ null

  let test_bool () =
    assert_equal ~label:"bool" ~equal (Obj.magic (Js.bool true) : t)
    @@ bool true

  let test_float () =
    assert_equal ~label:"float" ~equal (try_parse_exn {| 1.23 |}) @@ float 1.23

  let test_int () =
    assert_equal ~label:"int" ~equal (try_parse_exn {| 23 |}) @@ int 23

  let test_string () =
    assert_equal ~label:"string" ~equal (try_parse_exn {| "foo" |})
    @@ string "foo"

  let test_char () =
    assert_equal ~label:"char" ~equal (try_parse_exn {| "a" |}) @@ char 'a'

  let test_nullable () =
    assert_equal ~label:"none" ~equal (try_parse_exn {| null |})
    @@ nullable string None;
    assert_equal ~label:"some" ~equal (try_parse_exn {| "success" |})
    @@ nullable string (Some "success")

  let test_array () =
    assert_equal ~label:"array int" ~equal (try_parse_exn {| [1, 2, 3] |})
    @@ array int [| 1; 2; 3 |]

  let test_list () =
    assert_equal ~label:"list int" ~equal (try_parse_exn {| [1, 2, 3] |})
    @@ list int [ 1; 2; 3 ]

  let test_pair () =
    assert_equal ~label:"pair" ~equal (try_parse_exn {| ["hello", 1.2] |})
    @@ pair string float ("hello", 1.2)

  let test_tuple2 () =
    assert_equal ~label:"tuple2" ~equal (try_parse_exn {| ["hello", 1.2] |})
    @@ tuple2 string float ("hello", 1.2)

  let test_tuple3 () =
    assert_equal ~label:"tuple3" ~equal (try_parse_exn {| ["hello", 1.2, 4] |})
    @@ tuple3 string float int ("hello", 1.2, 4)

  let test_tuple4 () =
    assert_equal
      ~label:"tuple4"
      ~equal
      (try_parse_exn {| ["hello", 1.2, 4, true] |})
    @@ tuple4 string float int bool ("hello", 1.2, 4, true)

  let test_dict () =
    let equal x y = Jsonoo.stringify x = Jsonoo.stringify y in

    assert_equal ~label:"dict - empty" ~equal (try_parse_exn {| {} |})
    @@ dict id (Hashtbl.create 0);

    let tbl = Hashtbl.of_seq (List.to_seq [ ("x", int 42) ]) in
    assert_equal ~label:"dict - simple" ~equal (try_parse_exn {| { "x": 42 } |})
    @@ dict id tbl

  let test_object () =
    let equal x y = Jsonoo.stringify x = Jsonoo.stringify y in

    assert_equal ~label:"object - empty" ~equal (try_parse_exn {| {} |})
    @@ object_ [];

    assert_equal
      ~label:"object - simple"
      ~equal
      (try_parse_exn {| { "x": 42 } |})
    @@ object_ [ ("x", int 42) ]

  let suite =
    "Encode"
    >::: [ "test_id" >:: test_id
         ; "test_null" >:: test_null
         ; "test_bool" >:: test_bool
         ; "test_float" >:: test_float
         ; "test_int" >:: test_int
         ; "test_string" >:: test_string
         ; "test_char" >:: test_char
         ; "test_nullable" >:: test_nullable
         ; "test_array" >:: test_array
         ; "test_list" >:: test_list
         ; "test_pair" >:: test_pair
         ; "test_tuple2" >:: test_tuple2
         ; "test_tuple3" >:: test_tuple3
         ; "test_tuple4" >:: test_tuple4
         ; "test_dict" >:: test_dict
         ; "test_object" >:: test_object
         ]
end

module TestDecode = struct
  open Jsonoo.Decode

  module Test = struct
    type types =
      | Float
      | Int
      | String
      | Null
      | Array
      | Object
      | Bool
      | Char

    let value =
      let open Jsonoo.Encode in
      function
      | Float  -> float 1.23
      | Int    -> int 23
      | String -> string "test"
      | Null   -> null
      | Array  -> array id [||]
      | Object -> object_ []
      | Bool   -> bool true
      | Char   -> char 'a'

    let label = function
      | Float  -> "float"
      | Int    -> "int"
      | String -> "string"
      | Null   -> "null"
      | Array  -> "array"
      | Object -> "object"
      | Bool   -> "bool"
      | Char   -> "char"

    let throws decode types =
      let test t =
        let label = label t ^ " exception not raised" in
        match ignore (decode (value t)) with
        | exception Decode_error _ -> assert_true ~label true
        | _                        -> assert_true ~label false
      in
      List.iter test types
  end

  let test_id () = assert_equal ~label:"id" 0 (int (Decode.id (Encode.int 0)))

  let test_null () = assert_equal ~label:"null" Js.null @@ null Encode.null

  let test_bool () =
    assert_equal ~label:"true" true @@ bool (Encode.bool true);
    assert_equal ~label:"false" false @@ bool (Encode.bool false);
    Test.throws bool Test.[ Float; Int; String; Null; Array; Object; Char ]

  let test_float () =
    assert_equal ~label:"float" 1.23 @@ float (Encode.float 1.23);
    assert_equal ~label:"int" 23. @@ float (Encode.int 23);
    Test.throws float Test.[ Bool; String; Null; Array; Object; Char ]

  let test_int () =
    assert_equal ~label:"int" 23 @@ int (Encode.int 23);
    assert_raises
      ~label:"infinity"
      (Decode_error "Expected integer, got null")
      (fun () ->
        let i : Jsonoo.t = Js.Unsafe.get Js.Unsafe.global "Infinity" in
        let (_ : int) = int i in
        ());
    Test.throws int Test.[ Bool; Float; String; Null; Array; Object; Char ]

  let test_string () =
    assert_equal ~label:"string" "test" @@ string (Encode.string "test");
    assert_equal ~label:"single-character string" "a"
    @@ string (Encode.char 'a');
    Test.throws string Test.[ Bool; Float; Int; Null; Array; Object ]

  let test_char () =
    assert_equal ~label:"character" 'a' @@ char (Encode.char 'a');

    assert_equal ~label:"single-character string" 'a'
    @@ char (Encode.string "a");

    assert_raises
      ~label:"empty string"
      (Decode_error "Expected single-character string, got \"\"")
      (fun () -> ignore @@ char (Encode.string ""));

    assert_raises
      ~label:"multiple-character string"
      (Decode_error "Expected single-character string, got \"abc\"")
      (fun () -> ignore @@ char (Encode.string "abc"));

    Test.throws char Test.[ Bool; Float; Int; Null; Array; Object ]

  let test_nullable () =
    assert_equal ~label:"int -> int" (Some 23) @@ nullable int (Encode.int 23);
    assert_equal ~label:"null -> int" None @@ nullable int Encode.null;

    assert_equal ~label:"boolean -> boolean" (Some true)
    @@ nullable bool (Encode.bool true);

    assert_equal ~label:"float -> float" (Some 1.23)
    @@ nullable float (Encode.float 1.23);

    assert_equal ~label:"string -> string" (Some "test")
    @@ nullable string (Encode.string "test");

    assert_equal ~label:"null -> null" None @@ nullable id Encode.null;

    Test.throws (nullable int) Test.[ Bool; Float; String; Array; Object; Char ];
    Test.throws (nullable bool) Test.[ Int; Float; String; Array; Object; Char ]

  let test_array () =
    assert_equal ~label:"array" [||] @@ (array int) (Encode.array id [||]);

    assert_equal ~label:"boolean" [| true; false; true |]
    @@ array bool (try_parse_exn {| [true, false, true] |});
    assert_equal ~label:"float" [| 1.; 2.; 3. |]
    @@ array float (try_parse_exn {| [1, 2, 3] |});
    assert_equal ~label:"int" [| 1; 2; 3 |]
    @@ array int (try_parse_exn {| [1, 2, 3] |});
    assert_equal ~label:"string" [| "a"; "b"; "c" |]
    @@ array string (try_parse_exn {| ["a", "b", "c"] |});

    assert_raises
      ~label:"array int -> array boolean"
      (Decode_error "Expected boolean, got 1\n\tin array at index 0")
      (fun () -> ignore @@ (array bool) (try_parse_exn "[1, 2, 3]"));

    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (array (fun _ -> failwith "fail")) (Encode.array Encode.int [| 1 |]));

    Test.throws
      (array int)
      Test.[ Bool; Float; Int; String; Null; Object; Char ]

  let test_list () =
    assert_equal ~label:"array" [] @@ (list int) (Encode.array id [||]);

    assert_equal ~label:"boolean" [ true; false; true ]
    @@ list bool (try_parse_exn {| [true, false, true] |});
    assert_equal ~label:"float" [ 1.; 2.; 3. ]
    @@ list float (try_parse_exn {| [1, 2, 3] |});
    assert_equal ~label:"int" [ 1; 2; 3 ]
    @@ list int (try_parse_exn {| [1, 2, 3] |});

    assert_equal ~label:"string" [ "a"; "b"; "c" ]
    @@ list string (try_parse_exn {| ["a", "b", "c"] |});

    assert_raises
      ~label:"array int -> list boolean"
      (Decode_error "Expected boolean, got 1\n\tin array at index 0")
      (fun () -> ignore @@ (list bool) (try_parse_exn {| [1, 2, 3] |}));

    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (list (fun _ -> failwith "fail")) (Encode.list Encode.int [ 1 ]));

    Test.throws (list int) Test.[ Bool; Float; Int; String; Null; Object; Char ]

  let test_pair () =
    assert_equal ~label:"heterogenous" ("a", 3)
    @@ pair string int (try_parse_exn {| ["a", 3] |});
    assert_equal ~label:"int int" (4, 3)
    @@ pair int int (try_parse_exn {| [4, 3] |});
    assert_raises
      ~label:"too small"
      (Decode_error "Expected array of length 2, got array of length 1")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| [4] |}));
    assert_raises
      ~label:"too large"
      (Decode_error "Expected array of length 2, got array of length 3")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| [3, 4, 5] |}));
    assert_raises
      ~label:"bad type a"
      (Decode_error "Expected number, got \"3\"\n\tin array at index 0")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| ["3", 4] |}));
    assert_raises
      ~label:"bad type b"
      (Decode_error "Expected string, got 4\n\tin array at index 1")
      (fun () -> ignore @@ (pair string string) (try_parse_exn {| ["3", 4] |}));
    assert_raises
      ~label:"not array"
      (Decode_error "Expected array, got 4")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| 4 |}));
    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (pair (fun _ -> failwith "fail") int) (try_parse_exn {| [4, 3] |}))

  let test_tuple2 () =
    assert_equal ~label:"heterogenous" ("a", 3)
    @@ pair string int (try_parse_exn {| ["a", 3] |});
    assert_equal ~label:"int int" (4, 3)
    @@ pair int int (try_parse_exn {| [4, 3] |});
    assert_raises
      ~label:"too small"
      (Decode_error "Expected array of length 2, got array of length 1")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| [4] |}));
    assert_raises
      ~label:"too large"
      (Decode_error "Expected array of length 2, got array of length 3")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| [3, 4, 5] |}));
    assert_raises
      ~label:"bad type a"
      (Decode_error "Expected number, got \"3\"\n\tin array at index 0")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| ["3", 4] |}));
    assert_raises
      ~label:"bad type b"
      (Decode_error "Expected string, got 4\n\tin array at index 1")
      (fun () -> ignore @@ (pair string string) (try_parse_exn {| ["3", 4] |}));
    assert_raises
      ~label:"not array"
      (Decode_error "Expected array, got 4")
      (fun () -> ignore @@ (pair int int) (try_parse_exn {| 4 |}));
    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (pair (fun _ -> failwith "fail") int) (try_parse_exn {| [4, 3] |}))

  let test_tuple3 () =
    assert_equal ~label:"heterogenous" ("a", 3, 4.5)
    @@ tuple3 string int float (try_parse_exn {| ["a", 3, 4.5] |});
    assert_raises
      ~label:"too small"
      (Decode_error "Expected array of length 3, got array of length 1")
      (fun () -> ignore @@ (tuple3 int int int) (try_parse_exn {| [4] |}));
    assert_raises
      ~label:"too large"
      (Decode_error "Expected array of length 3, got array of length 5")
      (fun () ->
        ignore @@ (tuple3 int int int) (try_parse_exn {| [3, 4, 5, 6, 7] |}));
    assert_raises
      ~label:"bad type a"
      (Decode_error "Expected number, got \"3\"\n\tin array at index 0")
      (fun () ->
        ignore @@ (tuple3 int int int) (try_parse_exn {| ["3", 4, 5] |}));
    assert_raises
      ~label:"bad type b"
      (Decode_error "Expected string, got 4\n\tin array at index 1")
      (fun () ->
        ignore
        @@ (tuple3 string string string) (try_parse_exn {| ["3", 4, "5"] |}));
    assert_raises
      ~label:"not array"
      (Decode_error "Expected array, got 4")
      (fun () -> ignore @@ (tuple3 int int int) (try_parse_exn {| 4 |}));
    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (tuple3 (fun _ -> failwith "fail") int int)
             (try_parse_exn {| [4, 3, 5] |}))

  let test_tuple4 () =
    assert_equal ~label:"heterogenous" ("a", 3, 4.5, true)
    @@ tuple4 string int float bool (try_parse_exn {| ["a", 3, 4.5, true] |});
    assert_raises
      ~label:"too small"
      (Decode_error "Expected array of length 4, got array of length 1")
      (fun () -> ignore @@ (tuple4 int int int int) (try_parse_exn {| [4] |}));
    assert_raises
      ~label:"too large"
      (Decode_error "Expected array of length 4, got array of length 6")
      (fun () ->
        ignore
        @@ (tuple4 int int int int) (try_parse_exn {| [3, 4, 5, 6, 7, 8] |}));
    assert_raises
      ~label:"bad type a"
      (Decode_error "Expected number, got \"3\"\n\tin array at index 0")
      (fun () ->
        ignore @@ (tuple4 int int int int) (try_parse_exn {| ["3", 4, 5, 6] |}));
    assert_raises
      ~label:"bad type b"
      (Decode_error "Expected string, got 4\n\tin array at index 1")
      (fun () ->
        ignore
        @@ (tuple4 string string string string)
             (try_parse_exn {| ["3", 4, "5", "6"] |}));
    assert_raises
      ~label:"not array"
      (Decode_error "Expected array, got 4")
      (fun () -> ignore @@ (tuple4 int int int int) (try_parse_exn {| 4 |}));
    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (tuple4 (fun _ -> failwith "fail") int int int)
             (try_parse_exn {| [4, 3, 5, 6] |}))

  let test_dict () =
    let table xs = Hashtbl.of_seq (List.to_seq xs) in

    assert_equal ~label:"object" (Hashtbl.create 0)
    @@ dict int (Encode.object_ []);

    assert_equal ~label:"boolean" (table [ ("a", true); ("b", false) ])
    @@ dict bool (try_parse_exn {| { "a": true, "b": false } |});
    assert_equal ~label:"float" (table [ ("a", 1.2); ("b", 2.3) ])
    @@ dict float (try_parse_exn {| { "a": 1.2, "b": 2.3 } |});
    assert_equal ~label:"int" (table [ ("a", 1); ("b", 2) ])
    @@ dict int (try_parse_exn {| { "a": 1, "b": 2 } |});
    assert_equal ~label:"string" (table [ ("a", "x"); ("b", "y") ])
    @@ dict string (try_parse_exn {| { "a": "x", "b": "y" } |});

    assert_raises
      ~label:"null -> dict string"
      (Decode_error "Expected string, got null\n\tin object at field 'a'")
      (fun () ->
        ignore @@ (dict string) (try_parse_exn {| { "a": null, "b": null } |}));
    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (dict (fun _ -> failwith "fail")) (try_parse_exn {| { "a": 0 } |}));

    Test.throws (dict int) Test.[ Bool; Float; Int; String; Null; Array; Char ]

  let test_field () =
    assert_equal ~label:"boolean" false
    @@ field "b" bool (try_parse_exn {| { "a": true, "b": false } |});
    assert_equal ~label:"float" 2.3
    @@ field "b" float (try_parse_exn {| { "a": 1.2, "b": 2.3 } |});
    assert_equal ~label:"int" 2
    @@ field "b" int (try_parse_exn {| { "a": 1, "b": 2 } |});
    assert_equal ~label:"string" "y"
    @@ field "b" string (try_parse_exn {| { "a": "x", "b": "y" } |});

    assert_raises
      ~label:"missing key"
      (Decode_error "Expected field 'c'")
      (fun () ->
        ignore
        @@ (field "c" string) (try_parse_exn {| { "a": null, "b": null } |}));
    assert_raises
      ~label:"decoder error"
      (Decode_error "Expected string, got null\n\tin object at field 'b'")
      (fun () ->
        ignore
        @@ (field "b" string) (try_parse_exn {| { "a": null, "b": null } |}));
    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore
        @@ (field "a" (fun _ -> failwith "fail"))
             (try_parse_exn {| { "a": 0 } |}));

    Test.throws
      (field "foo" int)
      Test.[ Bool; Float; Int; String; Null; Array; Object; Char ]

  let test_at () =
    assert_equal ~label:"boolean" false
    @@ at
         [ "a"; "x"; "y" ]
         bool
         (try_parse_exn {| { "a": { "x" : { "y" : false } }, "b": false } |});
    assert_equal ~label:"nullable_try_default" None
    @@ at
         [ "a"; "x" ]
         (nullable id)
         (try_parse_exn {| { "a": { "x" : null }, "b": null } |});

    assert_raises
      ~label:"missing key"
      (Decode_error "Expected field 'y'\n\tin object at field 'a'")
      (fun () ->
        ignore
        @@ at
             [ "a"; "y" ]
             (nullable id)
             (try_parse_exn {| { "a": { "x" : null }, "b": null } |}));
    assert_raises
      ~label:"decoder error"
      (Decode_error
         "Expected number, got \"foo\"\n\
          \tin object at field 'y'\n\
          \tin object at field 'x'\n\
          \tin object at field 'a'")
      (fun () ->
        ignore
        @@ at
             [ "a"; "x"; "y" ]
             int
             (try_parse_exn {| { "a": { "x" : { "y": "foo" } }, "b": 1 } |}));
    assert_raises
      ~label:"empty list of keys should raise Invalid_argument"
      (Invalid_argument "Expected key_path to contain at least one element")
      (fun () -> ignore @@ (at []) int);

    Test.throws
      (at [ "foo"; "bar" ] int)
      Test.[ Bool; Float; Int; String; Null; Array; Object; Char ]

  let test_try_optional () =
    assert_equal ~label:"boolean -> int" None
    @@ (try_optional int) (Encode.bool true);
    assert_equal ~label:"float -> int" None
    @@ (try_optional int) (Encode.float 1.23);
    assert_equal ~label:"int -> int" (Some 23)
    @@ (try_optional int) (Encode.int 23);
    assert_equal ~label:"string -> int" None
    @@ (try_optional int) (Encode.string "test");
    assert_equal ~label:"null -> int" None @@ (try_optional int) Encode.null;
    assert_equal ~label:"array -> int" None
    @@ (try_optional int) (Encode.array id [||]);
    assert_equal ~label:"object -> int" None
    @@ (try_optional int) (Encode.object_ []);

    assert_equal ~label:"boolean -> boolean" (Some true)
    @@ try_optional bool (Encode.bool true);
    assert_equal ~label:"float -> float" (Some 1.23)
    @@ try_optional float (Encode.float 1.23);
    assert_equal ~label:"string -> string" (Some "test")
    @@ try_optional string (Encode.string "test");
    assert_equal ~label:"null -> null" (Some None)
    @@ try_optional (nullable id) Encode.null;
    assert_equal ~label:"int -> boolean" None
    @@ (try_optional bool) (Encode.int 1);

    assert_equal ~label:"optional field" (Some 2)
    @@ try_optional (field "x" int) (try_parse_exn {| { "x": 2} |});
    assert_equal ~label:"optional field - incorrect type" None
    @@ try_optional (field "x" int) (try_parse_exn {| { "x": 2.3} |});
    assert_equal ~label:"optional field - no such field" None
    @@ try_optional (field "y" int) (try_parse_exn {| { "x": 2} |});
    assert_equal ~label:"field optional" (Some 2)
    @@ field "x" (try_optional int) (try_parse_exn {| { "x": 2} |});
    assert_equal ~label:"field optional - incorrect type" None
    @@ field "x" (try_optional int) (try_parse_exn {| { "x": 2.3} |});
    assert_raises
      ~label:"field optional - no such field"
      (Decode_error "Expected field 'y'")
      (fun () ->
        ignore @@ (field "y" (try_optional int)) (try_parse_exn {| { "x": 2} |}));

    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore @@ (try_optional (fun _ -> failwith "fail")) Encode.null)

  let test_try_default () =
    assert_equal ~label:"boolean" 0 @@ (try_default 0 int) (Encode.bool true);
    assert_equal ~label:"float" 0 @@ (try_default 0 int) (Encode.float 1.23);
    assert_equal ~label:"int" 23 @@ (try_default 0 int) (Encode.int 23);
    assert_equal ~label:"string" 0 @@ (try_default 0 int) (Encode.string "test");
    assert_equal ~label:"null" 0 @@ (try_default 0 int) Encode.null;
    assert_equal ~label:"array" 0 @@ (try_default 0 int) (Encode.array id [||]);
    assert_equal ~label:"object" 0 @@ (try_default 0 int) (Encode.object_ []);

    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () ->
        ignore @@ (try_default 4 (fun _ -> failwith "fail")) (Encode.int 0))

  let test_any () =
    assert_equal ~label:"object with field" 2
    @@ (any [ int; field "x" int ]) (try_parse_exn {| { "x": 2} |});
    assert_equal ~label:"int" 23 @@ (any [ int; field "x" int ]) (Encode.int 23);

    assert_raises
      ~label:"non-Decode_error exceptions in decoder should pass through"
      (Failure "fail")
      (fun () -> ignore @@ (any [ (fun _ -> failwith "fail") ]) Encode.null);

    Test.throws
      (any [ int; field "x" int ])
      Test.[ Bool; Float; String; Null; Array; Object; Char ]

  let test_either () =
    assert_equal ~label:"object with field" 2
    @@ (either int (field "x" int)) (try_parse_exn {| { "x": 2} |});
    assert_equal ~label:"int" 23 @@ (either int (field "x" int)) (Encode.int 23);

    Test.throws
      (either int (field "x" int))
      Test.[ Bool; Float; String; Null; Array; Object; Char ]

  let test_map () =
    assert_equal ~label:"int" 25 @@ (int |> map (( + ) 2)) (Encode.int 23);

    Test.throws
      (int |> map (( + ) 2))
      Test.[ Bool; Float; String; Null; Array; Object; Char ]

  let test_bind () =
    assert_equal ~label:"int -> int" 23
    @@ (int |> bind (fun _ -> int)) (Encode.int 23);

    assert_equal ~label:"int -> int andThen float" 23.
    @@ (int |> bind (fun _ -> float)) (Encode.int 23);
    assert_equal ~label:"int -> float andThen int" 23
    @@ (float |> bind (fun _ -> int)) (Encode.int 23);

    Test.throws
      (int |> bind (fun _ -> int))
      Test.[ Bool; Float; String; Null; Array; Object; Char ];
    Test.throws (float |> bind (fun _ -> int)) Test.[ Float ];
    Test.throws (int |> bind (fun _ -> float)) Test.[ Float ]

  let test_composite_expressions () =
    let table xs = Hashtbl.of_seq (List.to_seq xs) in

    assert_equal
      ~label:"dict array array int"
      (dict
         (array (array int))
         (try_parse_exn {| { "a": [[1, 2], [3]], "b": [[4], [5, 6]] } |}))
      (table
         [ ("a", [| [| 1; 2 |]; [| 3 |] |]); ("b", [| [| 4 |]; [| 5; 6 |] |]) ]);
    assert_raises
      ~label:"dict array array int - heterogenous structure"
      (Decode_error
         "Expected number, got true\n\
          \tin array at index 0\n\
          \tin array at index 1\n\
          \tin object at field 'a'")
      (fun () ->
        ignore
        @@ (dict (array (array int)))
             (try_parse_exn {| { "a": [[1, 2], [true]], "b": [[4], [5, 6]] } |}));
    assert_raises
      ~label:"dict array array int - heterogenous structure 2"
      (Decode_error
         "Expected array, got \"foo\"\n\
          \tin array at index 1\n\
          \tin object at field 'a'")
      (fun () ->
        ignore
        @@ (dict (array (array int)))
             (try_parse_exn {| { "a": [[1, 2], "foo"], "b": [[4], [5, 6]] } |}));

    let json = try_parse_exn {| { "foo": [1, 2, 3], "bar": "baz" } |} in
    assert_equal ~label:"field" ([| 1; 2; 3 |], "baz")
    @@ (field "foo" (array int) json, field "bar" string json)

  let suite =
    "Encode"
    >::: [ "test_id" >:: test_id
         ; "test_null" >:: test_null
         ; "test_bool" >:: test_bool
         ; "test_float" >:: test_float
         ; "test_int" >:: test_int
         ; "test_string" >:: test_string
         ; "test_char" >:: test_char
         ; "test_nullable" >:: test_nullable
         ; "test_array" >:: test_array
         ; "test_list" >:: test_list
         ; "test_pair" >:: test_pair
         ; "test_tuple2" >:: test_tuple2
         ; "test_tuple3" >:: test_tuple3
         ; "test_tuple4" >:: test_tuple4
         ; "test_dict" >:: test_dict
         ; "test_field" >:: test_field
         ; "test_at" >:: test_at
         ; "test_optional" >:: test_try_optional
         ; "test_try_default" >:: test_try_default
         ; "test_any" >:: test_any
         ; "test_either" >:: test_either
         ; "test_map" >:: test_map
         ; "test_bind" >:: test_bind
         ; "test_composite_expressions" >:: test_composite_expressions
         ]
end

let suite =
  "Jsonoo"
  >::: [ "test_stringify" >:: test_stringify
       ; "test_try_parse_opt" >:: test_try_parse_opt
       ; "test_try_parse_exn" >:: test_try_parse_exn
       ; TestEncode.suite
       ; TestDecode.suite
       ]

let () = Webtest_js.Runner.run suite
