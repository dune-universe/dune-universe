open Core
open Angstrom

module Let_syntax = struct
  let bind t ~f = t >>= f
  let map t ~f = t >>| f
  let both a b = lift2 Tuple2.create a b
end

let ws = take_while1 Char.is_whitespace

let charset =
  choice
    (* The following might not be an exhaustive list. We can add to this as we encounter
       more cases. *)
    [ string_ci "US-ASCII" >>| const `Ascii
    ; string_ci "UTF-8" >>| const `Utf8
    ; string_ci "ISO-8859-1" >>| const `Latin1
    ; string_ci "ISO-8859-2" >>| const `Latin2
    ]
;;

let encoding : [ `Base64 | `Quoted_printable ] Angstrom.t =
  choice [ string_ci "B" >>| const `Base64; string_ci "Q" >>| const `Quoted_printable ]
;;

let parser_ : string Angstrom.t =
  let%bind () = string "=?" >>| ignore
  and charset = charset
  and () = string "?" >>| ignore
  and encoding = encoding
  and () = string "?" >>| ignore
  and data =
    take_while (function
      | '?' -> false
      | c -> (not (Char.is_whitespace c)) && Char.is_print c)
  and () = string "?=" >>| ignore in
  let%bind data =
    match encoding with
    | `Quoted_printable ->
      (* RFC2047 deviates slightly from common quoted printable.
         In particular
         4.2(2) - Underscore may be used to encode space, and
         4.2(3)- underscore must be encoded.
         This substituion handles that decoding step. *)
      let data = String.substr_replace_all data ~pattern:"_" ~with_:" " in
      let data_bstr, _ =
        Quoted_printable_lexer.decode_quoted_printable
          (String.length data)
          (Lexing.from_string data)
      in
      return (Bigbuffer.contents data_bstr)
    | `Base64 ->
      (match Base64.decode data with
       | Ok data -> return data
       | Error (`Msg msg) -> fail msg)
  in
  match charset with
  | `Ascii
  | `Utf8
  | `Latin1
  | `Latin2
    -> return data
;;

let parser_many : string Angstrom.t =
  many
    (choice
       [ (let%map hd = parser_
          and tl =
            (* RFC2047 6.2 When displaying a particular header field that contains
               multiple 'encoded-word's, any 'linear-white-space' that separates a
               pair of adjacent 'encoded-word's is ignored. *)
            many
              (let%bind (_ : string) = option "" ws in
               parser_)
          in
          hd :: tl)
       ; (let%map c =
            choice
              [ take_while1 (function
                  | '=' -> false
                  | c -> not (Char.is_whitespace c))
              ; string "="
              (* Collapse Line breaks as per
                 RFC822 - 3.1.1 Unfolding is accomplished by regarding CRLF immediately
                 followed by an LWSP-char as equivalent to the LWSP-char.
                 RFC822 - 3.1.3 Rules of (un)folding apply to these (unstructured) fields *)
              ; (let%bind (_ : string) = choice [ string "\r\n"; string "\n" ] in
                 ws)
              (* The RFC is ambiguous on what should happen if there is a lone CRLF, so we
                 ignore those, and treat these as regular white space. The RFC is also
                 ambiguous on how to treat multiple consecutive whitespaces, so we do the
                 conservative thing and leave them exactly as is. *)
              ; ws
              ]
          in
          [ c ])
       ])
  >>| List.concat
  >>| String.concat ~sep:""
;;

let decode str =
  Angstrom.parse_string parser_many str |> Result.map_error ~f:Error.of_string
;;

let%expect_test _ =
  let test str = printf "%S\n" (decode str |> Or_error.ok_exn) in
  test "hello = there?!?";
  [%expect {|
    "hello = there?!?"
    |}];
  test "hello\n there\n\tagain\n   my\nfriend";
  [%expect {|
    "hello there\tagain   my\nfriend"
    |}];
  (* Some test vectors from: https://tools.ietf.org/html/rfc2047 *)
  test "=?ISO-8859-1?Q?a?=";
  [%expect {|
    "a"
    |}];
  test "=?ISO-8859-1?Q?a?= b";
  [%expect {|
    "a b"
    |}];
  test "=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=";
  [%expect {|
    "ab"
    |}];
  test "=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=";
  [%expect {|
    "ab"
    |}];
  test "=?ISO-8859-1?Q?a?=\n       =?ISO-8859-1?Q?b?=";
  [%expect {|
    "ab"
    |}];
  test "=?ISO-8859-1?Q?a_b?=";
  [%expect {|
    "a b"
    |}];
  test "=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?_b?=";
  [%expect {|
    "a b"
    |}];
  test " =?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>";
  [%expect {| " Keith Moore <moore@cs.utk.edu>" |}];
  test " =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>";
  [%expect {| " Keld J\248rn Simonsen <keld@dkuug.dk>" |}];
  test " =?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>";
  [%expect {| " Andr\233 Pirard <PIRARD@vm1.ulg.ac.be>" |}];
  test
    "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=\n\
     =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=";
  [%expect {| "If you can read this you understand the example." |}];
  (* a UTF8 Example *)
  test "=?UTF-8?B?SGkgVGhlcmVcIQo=?=";
  [%expect {| "Hi There\\!\n" |}]
;;
