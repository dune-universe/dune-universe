open Core
(*
   module RFC2822 = struct
   let unfold str =
   let lexbuf = Lexing.from_string str in
   let bigbuffer = Bigbuffer.create (String.length str) in
   Lexer.field_unstructured_unfold bigbuffer lexbuf;
   Bigbuffer.contents bigbuffer
   ;;

   let fold str =
   let lexbuf = Lexing.from_string str in
   let bigbuffer = Bigbuffer.create (String.length str) in
   Lexer.field_unstructured_fold bigbuffer lexbuf;
   Bigbuffer.contents bigbuffer
   ;;

   TEST_MODULE "Folding_and_unfolding" = struct
   TEST = (fold "a\n b\nc") = " a\n b\n c"
   TEST = (fold " a\n b\nc") = " a\n b\n c"
   TEST = (fold "\ta\n b\nc") = "\ta\n b\n c"
   TEST = (unfold " a\n b\nc d") = "a b c d"
   end
   end
*)

module RFC2045 = struct
  module Token = struct
    include (Mimestring.Case_insensitive : Mimestring.S)

    let is_valid str =
      not (String.is_empty str)
      && String.for_all str ~f:(function
        | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':'
        | '\\' | '"' | '/' | '[' | ']' | '?' | '=' -> false
        (* '\n', '\r', ' ' are excluded by the following: *)
        | '\033' .. '\126' -> true
        | _ -> false)

    let is_valid_or_quote str =
      if is_valid str then str
      else Mimestring.quote str

    let%test_module "RFC2045.Token" =
      (module struct
        let (=) = String.(=)

        let%test _ = is_valid_or_quote "abcdefghijkl" = "abcdefghijkl"
        let%test _ = is_valid_or_quote "abc=dka" = "\"abc=dka\""
        let%test _ = is_valid_or_quote "" = "\"\""
        let%test _ = is_valid_or_quote "\"" = "\"\\\"\""

        let valid =
          [ "abcdefghijkl"
          ; "LoremIpsum"
          ; "3.141"
          ; "---"
          ; "a"
          ]

        let%test_unit _ =
          List.iter valid ~f:(fun t ->
            OUnit.assert_equal
              ~msg:(sprintf !"is_valid %{sexp:string}" t)
              ~printer:Bool.to_string
              true (is_valid t))

        let invalid =
          [ "abc=dka"
          ; ""
          ; "\""
          ; "@"
          ; ","
          ; ":"
          ; ";"
          ; "\x00"
          ; "\x7F"
          ; "\x80"
          ]

        let%test_unit _ =
          List.iter invalid ~f:(fun t ->
            OUnit.assert_equal
              ~msg:(sprintf !"not (is_valid %{sexp:string})" t)
              ~printer:Bool.to_string
              false (is_valid t))

      end)
  end
end
