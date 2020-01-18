open OUnit

module M = struct
  type t = string
  let empty = ""
  let (^^) = (^)
end

let tests =
  "ppx_monoid" >:::
  [ "empty" >:: (fun () ->
        let empty = "empty" in
        assert_equal empty (begin%monoid end))

  ; "one operation" >:: (fun () ->
        let (^^) = (^) in
        assert_equal "foobar" (begin%monoid "foo"; "bar" end))

  ; "single" >:: (fun () ->
        assert_equal "foo" (begin%monoid "foo" end))

  ; "specified module" >:: (fun () ->
        assert_equal "foobar" (begin%monoid.M "foo"; "bar" end))

  ; "let open" >:: (fun () ->
        let open! M in
        assert_equal "foobar" (begin%monoid "foo"; "bar" end))

  ; "nested let" >:: (fun () -> 
        let x =
          begin%monoid.M
            let x = "foo" in
            x; x
          end
        in
        assert_equal "foofoo" x)

  ; "nested let begin..end" >:: (fun () ->
        let open! M in
        let x =
          begin%monoid
            (* this wouldn't type check if the inner begin..end was
               treated as a monoid expression *)
            let x = begin (); "foo" end in
            x; x
          end
        in
        assert_equal "foofoo" x)

  (* Nesting begin..end expressions *)
  ; "nested begin..end, left" >:: (fun () ->
        let open! M in
        let x =
          begin%monoid
            begin
              "foo";
              "bar"
            end;
            "baz"
          end
        in
        assert_equal "foobarbaz" x)

  ; "nested begin..end, right" >:: (fun () ->
        let open! M in
        let x =
          begin%monoid
            "baz";
            begin
              "foo";
              "bar"
            end;
          end
        in
        assert_equal "bazfoobar" x)

  (* Without begin..end *)
  ; "without begin..end" >:: (fun () ->
        let open! M in
        let x =
          [%monoid
            "foo";
            "bar"
          ]
        in
        assert_equal "foobar" x)

  (* If-then-else expressions *)
  ; "if-then-else" >:: (fun () ->
        let open! M in
        let b = true in
        let x =
          begin%monoid
            if b then begin
              "foo";
              "bar"
            end else begin
              "bar";
              "foo"
            end
          end
        in
        assert_equal "foobar" x)

  ; "if-then" >:: (fun () ->
        let open! M in
        let b = true in
        let x =
          begin%monoid
            if b then begin
              "foo";
              "bar"
            end
          end
        in
        assert_equal "foobar" x)

  (* match expressions *)
  ; "match" >:: (fun () ->
        let open! M in
        let o = Some "foo" in
        let x =
          begin%monoid
            (match o with
              | None ->
                 ()
              | Some x ->
                 begin x; x end);
            "baz"
          end
        in
        assert_equal "foofoobaz" x)

  (* local opens *)
  ; "local open" >:: (fun () ->
        let open! M in
        let x =
          begin%concat
            let open String in
            "foo";
            string_of_int (length "bar")
          end
        in
        assert_equal x "foo3")

  (* local module definitions *)
  ; "local module" >:: (fun () ->
        let open! M in
        let x =
          begin%concat
            let module X = struct let x = "bar" end in
            "foo";
            X.x
          end
        in
        assert_equal ~printer:(fun s -> s) "foobar" x)

  ; "while loop" >:: (fun () ->
        let open! M in
        let m =
          begin%concat
            let x = ref 3 in
            while !x > 0 do
              let _ = decr x in
              "foo"
            done
          end
        in
        assert_equal ~printer:(fun s -> s) "foofoofoo" m)

  ; "for loop" >:: (fun () ->
        let open! M in
        let m =
          begin%concat
            for i = 0 to 3 do
              string_of_int i
            done;
            for i = 3 downto 0 do
              string_of_int i
            done
          end
        in
        assert_equal ~printer:(fun s -> s) "01233210" m)

  ; "for loop clash" >:: (fun () ->
        let open! M in
        let accum = "foo" in
        let m =
          begin%concat
            for i = 1 to 3 do
              accum
            done;
          end
        in
        assert_equal ~printer:(fun s -> s) "foofoofoo" m)

  ; "for loop downto clash" >:: (fun () ->
        let open! M in
        let accum = "foo" in
        let loop = "bar" in
        let m =
          begin%concat
            for i = 3 downto 1 do
              accum; loop
            done;
          end
        in
        assert_equal ~printer:(fun s -> s) "foobarfoobarfoobar" m)

  ]

let _ =
  run_test_tt_main tests

