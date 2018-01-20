open Core
open Prettiest
open Prettiest.Infix

module Gen = Quickcheck.Generator

let mk_assert check = List.iter ~f:check [1; 10; 20; 40; 60; 80; 100; 120]

let assert_equal a b =
  let check w = 
    if Option.equal String.equal (render w a) (render w b)
    then ()
    else failwith ("Unequal at width: " ^ string_of_int w)
  in mk_assert check

let assert_height a b =
  let height s = s |> String.split_lines |> List.length in
  let check w = 
    let a' = render w a and b' = render w b in
    if Option.equal Int.equal (Option.map ~f:height a') (Option.map ~f:height b')
    then ()
    else failwith ("Unequal at width: " ^ string_of_int w)
  in mk_assert check

let string_gen = Gen.list Gen.char_alphanum |> Gen.map ~f:String.of_char_list

let text_gen = Gen.map string_gen ~f:text

let doc_gen : Prettiest.t Quickcheck.Generator.t =
  let open Gen.Let_syntax in
  Gen.recursive (fun self ->
      match%bind Gen.size with
      | 0 -> text_gen
      | n -> 
        let cat =
          Gen.with_size ~size:(n - 1) self >>= fun l ->
          Gen.with_size ~size:(n - 1) self >>| fun r ->
          l <> r
        and flush =
          Gen.with_size ~size:(n - 1) self >>| fun a ->
          flush a
        in
        Gen.union [ text_gen; cat; flush ]
    )

let%test_unit "empty left unit of <>" =
  Quickcheck.test doc_gen ~f:(fun a -> assert_equal (empty <> a) a)

let%test_unit "empty right unit of <>" =
  Quickcheck.test doc_gen ~f:(fun a -> assert_equal (empty <> a) a)

let%test_unit "<> associative" =
  Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
    ~f:(fun (a, b, c) ->
        assert_equal
          (a <> (b <> c))
          ((a <> b) <> c))

let%test_unit "text <> homomorphism" =
  Quickcheck.test (Gen.tuple2 string_gen string_gen)
    ~f:(fun (s, t) ->
        assert_equal
          (text s <> text t)
          (text (s ^ t)))

let%test_unit "flush <>" =
  Quickcheck.test (Gen.tuple2 doc_gen doc_gen)
    ~f:(fun (a, b) ->
        assert_equal
          (flush a <> flush b)
          (flush (flush a <> b)))

let%test_unit "<|> associative" =
  Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
    ~f:(fun (a, b, c) ->
        assert_equal
          (a <|> (b <|> c))
          ((a <|> b) <|> c))

let%test_unit "<> left distribute <|> wrt height" =
  Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
    ~f:(fun (a, b, c) ->
        assert_height
          ((a <|> b) <> c)
          ((a <> c) <|> (b <> c)))

let%test_unit "<> right distribute <|> wrt height" =
  Quickcheck.test (Gen.tuple3 doc_gen doc_gen doc_gen)
    ~f:(fun (a, b, c) ->
        assert_height
          (c <> (a <|> b))
          ((c <> a) <|> (c <> b)))

let%test_unit "flush distribute <|> wrt height" =
  Quickcheck.test (Gen.tuple2 doc_gen doc_gen)
    ~f:(fun (a, b) ->
        assert_height
          (flush (a <|> b))
          (flush a <|> flush b))

let%test_unit "<|> commutative wrt height" =
  Quickcheck.test (Gen.tuple2 doc_gen doc_gen)
    ~f:(fun (a, b) ->
        assert_height
          (a <|> b)
          (b <|> a))
