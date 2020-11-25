module F13 = Ff.MakeFp (struct
  let prime_order = Z.of_string "13"
end)

module F13ValueGeneration = Ff_pbt.MakeValueGeneration (F13)
module F13IsZero = Ff_pbt.MakeIsZero (F13)
module F13Equality = Ff_pbt.MakeEquality (F13)
module F13FieldProperties = Ff_pbt.MakeFieldProperties (F13)
module F13MemoryRepresentation = Ff_pbt.MakeMemoryRepresentation (F13)

module F1073740201 = Ff.MakeFp (struct
  let prime_order = Z.of_string "1073740201"
end)

module FF1073740201ValueGeneration = Ff_pbt.MakeValueGeneration (F1073740201)
module FF1073740201IsZero = Ff_pbt.MakeIsZero (F1073740201)
module FF1073740201Equality = Ff_pbt.MakeEquality (F1073740201)
module FF1073740201FieldProperties = Ff_pbt.MakeFieldProperties (F1073740201)
module FF1073740201MemoryRepresentation =
  Ff_pbt.MakeMemoryRepresentation (F1073740201)

module FFLong = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
end)

module FFLongValueGeneration = Ff_pbt.MakeValueGeneration (FFLong)
module FFLongIsZero = Ff_pbt.MakeIsZero (FFLong)
module FFLongEquality = Ff_pbt.MakeEquality (FFLong)
module FFLongFieldProperties = Ff_pbt.MakeFieldProperties (FFLong)
module FFLongMemoryRepresentation = Ff_pbt.MakeMemoryRepresentation (FFLong)

(* This is the base field of the Curve 25519, the name comes from its order: p**255 - 19*)
module FFBaseCurve25519 = Ff.MakeFp (struct
  let prime_order = Z.(pow (succ one) 255 - of_int 19)
end)

module FFBaseCurve25519ValueGeneration =
  Ff_pbt.MakeValueGeneration (FFBaseCurve25519)
module FFBaseCurve25519IsZero = Ff_pbt.MakeIsZero (FFBaseCurve25519)
module FFBaseCurve25519Equality = Ff_pbt.MakeEquality (FFBaseCurve25519)
module FFBaseCurve25519FieldProperties =
  Ff_pbt.MakeFieldProperties (FFBaseCurve25519)
module FFBaseCurve25519MemoryRepresentation =
  Ff_pbt.MakeMemoryRepresentation (FFBaseCurve25519)

module F13_2 =
  Ff.MakeFp2
    (F13)
    (struct
      let nsr = F13.(negate (of_string "2"))
    end)

module F13_2FieldProperties = Ff_pbt.MakeFieldProperties (F13_2)
module F13_2ValueGeneration = Ff_pbt.MakeValueGeneration (F13_2)
module F13_2IsZero = Ff_pbt.MakeIsZero (F13_2)
module F13_2Equality = Ff_pbt.MakeEquality (F13_2)
module F13_2MemoryRepresentation = Ff_pbt.MakeMemoryRepresentation (F13_2)

let test_size_in_bytes () =
  let open Alcotest in
  ( "Test size in bytes computation for different orders",
    [ test_case "Test vectors" `Quick (fun () ->
          let l =
            [ ("2", 1);
              ("13", 1);
              ("257", 2);
              ("1024", 2);
              ("65536", 3);
              ( "52435875175126190479447740508185965837690552500527637822603658699938581184513",
                32 );
              ( "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787",
                48 ) ]
          in
          List.iter
            (fun (order, expected_nb_bytes) ->
              let module P = Ff.MakeFp (struct
                let prime_order = Z.of_string order
              end) in
              assert (P.size_in_bytes = expected_nb_bytes))
            l) ] )

let () =
  let open Alcotest in
  run
    "Random fields"
    [ (* F13 *)
      F13IsZero.get_tests ();
      F13ValueGeneration.get_tests ();
      F13Equality.get_tests ();
      F13FieldProperties.get_tests ();
      F13MemoryRepresentation.get_tests ();
      (* F1073740201 *)
      FF1073740201IsZero.get_tests ();
      FF1073740201ValueGeneration.get_tests ();
      FF1073740201Equality.get_tests ();
      FF1073740201FieldProperties.get_tests ();
      FF1073740201MemoryRepresentation.get_tests ();
      (* FF Long *)
      FFLongIsZero.get_tests ();
      FFLongValueGeneration.get_tests ();
      FFLongEquality.get_tests ();
      FFLongFieldProperties.get_tests ();
      FFLongMemoryRepresentation.get_tests ();
      (* Base field of Curve25519 *)
      FFBaseCurve25519ValueGeneration.get_tests ();
      FFBaseCurve25519IsZero.get_tests ();
      FFBaseCurve25519Equality.get_tests ();
      FFBaseCurve25519FieldProperties.get_tests ();
      FFBaseCurve25519MemoryRepresentation.get_tests ();
      (* GF(13²)*)
      F13_2FieldProperties.get_tests ();
      F13_2IsZero.get_tests ();
      F13_2ValueGeneration.get_tests ();
      F13_2MemoryRepresentation.get_tests ();
      F13_2Equality.get_tests ();
      (* test size in bytes *)
      test_size_in_bytes () ]
