open OUnit2
open Core
open Format

module String = struct
  include String

  let of_byte_list list =
    list
    |> List.map ~f:Char.of_int_exn
    |> of_char_list

end

let result_error_string str =
  Result.error str
  |> Option.value
       ~default:(Error.of_string (Random.float 1000. |> Float.to_string))
  |> Error.to_string_hum

module TestV0 = struct

  open AMF__Amf_v0

  let is_supported_test =
    "Supported?" >:: fun _ -> assert_bool "No" true

  let in_out_check type_name in_val =
    type_name >::
      fun ctxt ->
      let msg =
        sexp_of_t in_val
        |> Sexp.to_string
        |> sprintf "Failed to serialize/unserialize value: %s" in
      let buffer = to_buffer in_val in
      let out_val = of_buffer buffer in
      assert_equal ~ctxt ~msg (Result.Ok in_val) out_val

  let in_out_failure type_name in_val exn =
    type_name >::
      fun _ ->
      let msg =
        sexp_of_t in_val
        |> Sexp.to_string
        |> sprintf "Succeeded in serializing/unserializing value: %s" in
      assert_raises
        ~msg exn
        (fun () ->
         let buffer = to_buffer in_val in
         let _ = of_buffer buffer in
         ())

  let test_byte_tag type_name in_val tag =
    type_name >::
      fun ctxt ->
      assert_equal ~ctxt ~msg:(sprintf "%s didn't match byte tag %x"
                                       type_name
                                       tag)
                   tag
                   ((Bytes.to_string (marker_buffer in_val)).[0] |> Char.to_int)

  let test =
    "V0" >:::
      [is_supported_test] @

      ["Unserializable" >::
         fun ctxt ->
         assert_equal ~ctxt
                      ObjectEnd
                      (String.of_byte_list [
                                  0x9; (* object end tag *)
                         ]
                       |> of_buffer
                       |> Result.ok
                       |> fun opt -> Option.value_exn opt)
      ] @

      ["Serializable" >:::
         (List.map
            ~f:(Tuple2.uncurry in_out_check)
            ["Number",       Number 3.;
             "TrueBoolean",  Boolean true;
             "FalseBoolean", Boolean false;
             "String",       String "hey this is a string";
             "Object",       Object ["foo", Number 42.; "bar", String "yo"];
             "Null",         Null;
             "Undefined",    Undefined;
             "Reference",    Reference 42;
             "ECMAArray",    ECMAArray ["0", String "yo"];
             "StrictArray",  StrictArray [|String "another"; String "array"|];
             "Date",         Date (Time.now ());
             "LongString",   LongString "yet another string";
             "XMLDocument",  XMLDocument "<foo>bar</foo>";
             "TypedObject",
             TypedObject {
                 class_name = "MyClass";
                 properties = [
                   "aprop", String "value";
                   "yo", Number 4.5;
                 ];
               };
            ])] @

      ["Unserializable" >:::
         (List.map
            ~f:(Tuple3.uncurry in_out_failure)
            ["MovieClip",     MovieClip,     UnsupportedType;
             "Unsupported",   Unsupported,   UnsupportedType;
             "RecordSet",     RecordSet,     UnsupportedType;
             "AVMPlusObject", AVMPlusObject, UpgradeToAMFV3;
            ])] @

      ["UnserializableMarkerBuffers" >:::
         (List.map
            ~f:(Tuple3.uncurry test_byte_tag)
            ["MovieClip",     MovieClip,     0x04;
             "Unsupported",   Unsupported,   0x0d;
             "RecordSet",     RecordSet,     0x0e;
             "AVMPlusObject", AVMPlusObject, 0x12;])] @

      ["EOBError" >::
         fun ctxt ->
         assert_equal ~ctxt ~msg:"Didn't get error on empty buffer"
                      "Premature end of buffer"
                      (of_buffer ""
                       |> Result.error
                       |> Option.value
                            ~default:(Error.of_string "something else")
                       |> Error.to_string_hum)] @

      ["BufferToList" >::
         fun ctxt ->
         let first = Number 3. in
         let second = String "foo" in
         let before = Result.Ok [first; second;] in
         assert_equal ~ctxt ~msg:"Convert buffer to list of items"
                      before
                      (to_buffer first ^ to_buffer second
                       |> list_of_buffer)] @

      ["BufferToArray" >::
         fun ctxt ->
         let first = Number 3. in
         let second = String "bar" in
         let before = Result.Ok (StrictArray [|first; second;|]) in
         assert_equal ~ctxt ~msg:"Convert buffer to an array of items"
                      before
                      (to_buffer first ^ to_buffer second
                       |> array_of_buffer)] @

      ["MalformedBuffers" >:::
         (List.map
            ~f:(fun (prefix, value) ->
                prefix >::
                  fun ctxt ->
                  let encoded = to_buffer value in
                  let encoded_wrong = String.sub
                                        ~pos:0 ~len:(String.length encoded - 1)
                                        encoded in
                  assert_equal ~ctxt
                               "Premature end of buffer"
                               (of_buffer encoded_wrong |> result_error_string)
               )
            ["String",     String "";
             "LongString", LongString "";
             "String",     String "a";
             "LongString", LongString "a";
             "Date",       Date (Time.now ());
             "Number",     Number 3.;
            ]
         ) @

         ["MovieClip" >::
            fun ctxt ->
            assert_equal ~ctxt
                         "Unsupported marker: MovieClip"
                         (String.of_byte_list [0x04]
                          |> of_buffer
                          |> result_error_string)
         ] @

         ["Unsupported" >::
            fun ctxt ->
            assert_equal ~ctxt
                         "Unsupported marker: Unsupported"
                         (String.of_byte_list [0x0d]
                          |> of_buffer
                          |> result_error_string)
         ] @

         ["RecordSet" >::
            fun ctxt ->
            assert_equal ~ctxt
                         "Unsupported marker: RecordSet"
                         (String.of_byte_list [0x0e]
                          |> of_buffer
                          |> result_error_string)
         ] @

         ["MadeUpTag" >::
            fun ctxt ->
            assert_equal ~ctxt
                         "Unrecognized marker: 0xaa"
                         (String.of_byte_list [0xaa]
                          |> of_buffer
                          |> result_error_string)
         ] @

         ["Date" >::
            fun ctxt ->
            assert_equal ~ctxt
                         "Timezone not empty"
                         (String.of_byte_list [
                              0x0b;       (* date tag *)
                              0; 0; 0; 0; (* ms, double *)
                              0; 0; 0; 0;
                              0; 235;     (* nonzero timezone *)
                            ]
                          |> of_buffer
                          |> result_error_string)
         ] @

         ["ECMAArray" >:::
            ["MissingProperty" >::
               (fun ctxt ->
                assert_equal ~ctxt
                             "Expecting another property, came up empty"
                             (String.of_byte_list [
                                  0x8; (* ecma array tag *)
                                  0x0; 0x0; 0x0; 0x1; (* one property *)
                                  0; 0; (* empty property name *)
                                  0x9; (* ObjectEnd tag *)
                                ]
                              |> of_buffer
                              |> result_error_string));
             "MissingObjectEnd" >::
               fun ctxt ->
               assert_equal ~ctxt
                            "Object didn't end with ObjectEnd"
                            (String.of_byte_list [
                                 0x8; (* ecma array tag *)
                                 0x0; 0x0; 0x0; 0x1; (* one property *)
                                 0; 0; 0; 0;
                                 0x9; (* ObjectEnd tag *)
                               ]
                             |> of_buffer
                             |> result_error_string)]]
      ] @

      ["MultipleItemsInBuffer" >::
         fun ctxt ->
         assert_equal
           ~ctxt
           "Not end of buffer"
           (let num = to_buffer (Number 3.) in
            (of_buffer (num ^ num)
             |> result_error_string))] @

      ["UpgradeToV3" >::
         fun _ ->
         assert_raises ~msg:"Make sure upgrade exception is raised"
                       UpgradeToAMFV3
                       (fun () -> String.of_byte_list [0x12] |> of_buffer)
      ]

end

module TestV3 = struct

  let is_supported_test =
    "Supported?" >:: fun _ -> assert_bool "No" false

  let test =
    "V3" >:::
      [is_supported_test;
      ]

end

let test =
  "AMF" >:::
    [TestV0.test;
     (* TestV3.test; not supporting v3 yet *)
    ]
