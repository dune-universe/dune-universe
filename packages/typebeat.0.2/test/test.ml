let content =
  let module M =
  struct
    type t = TypeBeat.content

    let pp = TypeBeat.pp
    let equal : t -> t -> bool = TypeBeat.equal ~insensitive:`All
  end in (module M : Alcotest.TESTABLE with type t = M.t)

let make_test (str, value) =
  Format.sprintf "%S" str,
  `Slow,
  (fun () -> match TypeBeat.of_string str with
     | Ok value' -> Alcotest.(check content) "" value value'
     | Error _ -> assert false)

let tests_rfc2045 =
  [ "text/plain; charset=us-ascii (Plain Text)",
    TypeBeat.make `Text "plain" ~parameters:[("charset", `Token "us-ascii")]
  ; "text/plain; charset=\"us-ascii\"",
    TypeBeat.make `Text "plain" ~parameters:[("charset", `Token "us-ascii")]
  ; "text/plain; charset=ISO-8859-1",
    TypeBeat.make `Text "plain" ~parameters:[("charset", `Token "ISO-8859-1")] ]

let tests_rfc7231 =
  [ "text/html;charset=utf-8",
    TypeBeat.make `Text "html" ~parameters:[("charset", `Token "utf-8")]
  ; "text/html;charset=UTF-8",
    TypeBeat.make `Text "html" ~parameters:[("charset", `Token "utf-8")]
  ; "Text/HTML;Charset=\"utf-8\"",
    TypeBeat.make `Text "html" ~parameters:[("charset", `Token "utf-8")]
  ; "text/html; charset=\"utf-8\"",
    TypeBeat.make `Text "html" ~parameters:[("charset", `Token "utf-8")] ]

let () =
  Alcotest.run "Content-Type tests"
    [ "RFC2045", List.map make_test tests_rfc2045
    ; "RFC7231", List.map make_test tests_rfc7231 ]
