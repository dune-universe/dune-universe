open Core

module Encoding = struct
  (** Text or binary are the type of the plaintext. For Base64, if the mode is
      text, '\n' is turned into '\r\n' when encoding, and viceversa. *)
  type known =
    [ `Base64
    | `Bit7
    | `Bit8
    | `Binary
    | `Quoted_printable
    ]
  [@@deriving sexp, bin_io, compare, hash]

  type t =
    [ known
    | `Unknown of string
    ] [@@deriving sexp, bin_io, compare, hash]
  ;;

  let of_string encoding =
    match encoding |> String.strip |> String.lowercase with
    | "base64"           -> `Base64
    | "7bit"             -> `Bit7
    | "8bit"             -> `Bit8
    | "binary"           -> `Binary
    | "quoted-printable" -> `Quoted_printable
    | unknown            -> `Unknown unknown

  let to_string = function
    | `Base64 -> "base64"
    | `Bit7 -> "7bit"
    | `Bit8 -> "8bit"
    | `Binary -> "binary"
    | `Quoted_printable -> "quoted-printable"
    | `Unknown unknown -> unknown

  let default = `Bit7

  let of_headers headers =
    Headers.last headers "content-transfer-encoding"
    |> Option.map ~f:of_string

  let of_headers_or_default headers =
    match of_headers headers with
    | Some t -> t
    | None   -> default
end

type t =
  { encoding : Encoding.t
  ; content  : Bigstring_shared.t
  } [@@deriving sexp, bin_io, compare, hash]

let create ?(encoding = Encoding.default) content =
  { encoding; content }

let encoding t = t.encoding
let encoded_contents t = t.content

let empty = create Bigstring_shared.empty

let to_string_monoid t = Bigstring_shared.to_string_monoid (encoded_contents t)

let of_string str = create (Bigstring_shared.of_string str)
let to_string t = Bigstring_shared.to_string (encoded_contents t)

(*
   let length t  = Bigstring_shared.length t.content

   let to_lexbuf t = Bigstring_shared.to_lexbuf t.content;;
*)

(*
   (** Bigstring.sub creates copies of the Bigstring *)
   let of_bigstring bstr = create (Bigstring.subo bstr)
   let to_bigstring t = Bigstring.subo (contents t)

   let of_string_monoid mon = of_bigstring (String_monoid.to_bigstring mon)
*)

(********)

module Identity = struct
  let encode bstr = bstr
  let decode bstr = bstr
end

module Base64 = struct
  include Base64.Make(struct
      let char62 = '+'
      let char63 = '/'
      let pad_char = '='
      let pad_when_encoding = true
      (* Permissive - ignore anything that would be an invalid base64 character... *)
      let ignore_char = function
        | '0'..'9' | 'a'..'z' | 'A'..'Z' | '+' | '/' | '=' -> false
        | _ -> true
    end)

  let decode bstr =
    bstr
    |> Bigstring_shared.to_string
    |> decode
    (* Ignore unconsumed data *)
    |> fst
    |> Bigstring_shared.of_string

  let split ~len =
    let rec go acc bstr =
      if Bigstring_shared.length bstr <= len then
        List.rev (bstr :: acc)
      else
        go (Bigstring_shared.sub bstr ~len :: acc) (Bigstring_shared.sub bstr ~pos:len)
    in
    go []

  let encoded_line_length = 76
  let decoded_block_length = encoded_line_length / 4 * 3

  let encode bstr =
    split ~len:decoded_block_length bstr
    |> List.map ~f:(fun bstr ->
      Bigstring_shared.to_string bstr
      |> encode)
    |> String_monoid.concat_string ~sep:"\n"
    |> Bigstring_shared.of_string_monoid
  ;;

  let%test_module "Octet_stream.Base64" =
    (module struct
      open OUnit
      open Bigstring_shared

      let test_vectors = List.map ~f:(fun (x,y) ->
        (of_string x, of_string y))
        [
          ("YW55IGNhcm5hbCBwbGVhc3VyZS4=", "any carnal pleasure.");
          ("YW55IGNhcm5hbCBwbGVhc3VyZQ==", "any carnal pleasure" );
          ("YW55IGNhcm5hbCBwbGVhc3Vy"    , "any carnal pleasur"  );
          ("YW55IGNhcm5hbCBwbGVhc3U="    , "any carnal pleasu"   );
          ("YW55IGNhcm5hbCBwbGVhcw=="    , "any carnal pleas"    );
          ("\
TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwg\n\
c2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWdu\n\
YSBhbGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0\n\
aW9uIHVsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1\n\
YXQuIER1aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2\n\
ZWxpdCBlc3NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRl\n\
dXIgc2ludCBvY2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1\n\
aSBvZmZpY2lhIGRlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg==\
            ", "\
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis \
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. \
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore \
eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt \
in culpa qui officia deserunt mollit anim id est laborum.\
            ")
        ]
      ;;

      let%test_unit "decode" = List.iter test_vectors  ~f:(fun (coded, plaintext) ->
        let plaintext' = decode coded in
        assert_equal ~printer:to_string plaintext plaintext')
      ;;

      let%test_unit "encode" =
        List.iter test_vectors  ~f:(fun (coded, plaintext) ->
          let coded' = encode plaintext in
          assert_equal ~printer:to_string coded coded')
      ;;
    end)
end

module Quoted_printable = struct
  open OUnit

  let decode bstr =
    (* The RFC2045 says that newlines can be converted to the platforms native
       format, so that's what we'll do. It's the same for both binary data and
       text data. If a CRLF sequence appears in the decoded data, that's because
       it was encoded as =0D=0A, which means the characters shouldn't be
       interpreted as EOL.  *)
    let bigbuffer, _ =
      Quoted_printable_lexer.decode_quoted_printable
        (Bigstring_shared.length bstr) (Bigstring_shared.to_lexbuf bstr)
    in
    Bigstring_shared.of_bigbuffer_volatile bigbuffer
  ;;

  let encode bstr =
    let bigbuffer =
      Quoted_printable_lexer.encode_quoted_printable
        (Bigstring_shared.length bstr)
        (Bigstring_shared.to_lexbuf bstr)
    in
    Bigstring_shared.of_bigbuffer_volatile bigbuffer
  ;;

  let%test_module "quoted-printable" =
    (module struct
      open Bigstring_shared

      let mathematics = List.map ~f:(fun (x,y) -> (of_string x, of_string y))
                          [("If you believe that truth=3Dbeauty, then surely =\n\
                             mathematics is the most beautiful branch of philosophy.",
                            "If you believe that truth=beauty, then surely mathematics is the \
                             most beautiful branch of philosophy.")]
      ;;

      let encoding = List.map ~f:(fun (x,y) -> (of_string x, of_string y))
                       [("=00=01=02a=03   =20\n", "\000\001\002a\003    \n");
                        ("This text is fairly long and should be wrapped by a conforming =\n\
                          implementation.",
                         "This text is fairly long and should be wrapped by a conforming \
                          implementation.");
                        ("123456789A123456789B123456789C123456789D123456789E123456789F\
                          123456789G12345=\n6789H123456789I",
                         "123456789A123456789B123456789C123456789D123456789E123456789F\
                          123456789G123456789H123456789I");
                        ("123456789A123456789B123456789C123456789D123456789E123456789F\
                          123456789G=3D23=\n456789H123456789I",
                         "123456789A123456789B123456789C123456789D123456789E123456789F\
                          123456789G=23456789H123456789I")
                       ]
      ;;

      let test_decode pos l =
        let coded, plaintext = List.nth_exn l pos in
        let plaintext' = decode coded in
        assert_equal ~printer:to_string plaintext plaintext'
      ;;

      let test_encode pos l =
        let coded, plaintext = List.nth_exn l pos in
        let coded' = encode plaintext in
        assert_equal ~printer:to_string coded coded';
        let plaintext' = decode coded' in
        assert_equal ~printer:to_string plaintext plaintext';
      ;;

      let%test_unit _ = test_decode 0 mathematics
      let%test_unit _ = test_encode 0 encoding
      let%test_unit _ = test_encode 1 encoding
      let%test_unit _ = test_encode 2 encoding
      let%test_unit _ = test_encode 3 encoding
    end)
end

let decode t =
  match t.encoding with
  | `Base64           -> Some (Base64.decode t.content)
  | `Quoted_printable -> Some (Quoted_printable.decode t.content)
  | `Bit7             -> Some (Identity.decode t.content)
  | `Bit8             -> Some (Identity.decode t.content)
  | `Binary           -> Some (Identity.decode t.content)
  | `Unknown _        -> None

let encode ~encoding bstr =
  let bstr =
    match encoding with
    | `Base64           -> Base64.encode bstr
    | `Quoted_printable -> Quoted_printable.encode bstr
    | `Bit7             -> Identity.encode bstr
    | `Bit8             -> Identity.encode bstr
    | `Binary           -> Identity.encode bstr
  in
  let encoding = (encoding :> Encoding.t) in
  create ~encoding bstr
