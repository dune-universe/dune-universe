let () = Printexc.record_backtrace true
let ( <.> ) f g = fun x -> f (g x)

let rest_of_input ~committed ~len buffer input =
  let head = Bigstringaf.substring buffer ~off:committed ~len:(len - committed) in
  String.concat "" (head :: input)

let valid_unstructured_flow input expect =
  Alcotest.test_case (Fmt.strf "[%a]" Fmt.(list ~sep:(always "; ") (fmt "%S")) input) `Quick @@ fun () ->
  let buffer = Bigstringaf.create 0x1000 in
  let rec go len input = function
    | Angstrom.Unbuffered.Done (committed, v) ->
      let rest = rest_of_input ~committed ~len buffer input in
      Ok (v, rest)
    | Fail (_, _, err) -> Error err
    | Partial { committed; continue; } ->
      let len = len - committed in
      Bigstringaf.blit buffer ~src_off:committed buffer ~dst_off:0 ~len ;

      match input with
      | [] -> go len [] (continue buffer ~off:0 ~len Complete)
      | x :: r ->
        Bigstringaf.blit_from_string x ~src_off:0 buffer ~dst_off:len ~len:(String.length x) ;
        go (len + String.length x) r (continue buffer ~off:0 ~len:(len + String.length x) Incomplete) in
  let buf = Bytes.create 0x1000 in
  let res = go 0 input (Angstrom.Unbuffered.parse (Unstrctrd_parser.unstrctrd buf)) in
  match res with
  | Ok (v, rest) ->
    let res = Unstrctrd.to_utf_8_string v in
    Alcotest.(check string) "result" res (fst expect) ;
    Alcotest.(check string) "rest" rest (snd expect)
  | Error err -> Alcotest.fail err

module Q = Ke.Rke.Weighted

let parser length =
  let is_ftext = function '\033' .. '\057' | '\059' .. '\126' -> true | _ -> false in
  let is_wsp = function ' ' | '\t' -> true | _ -> false in
  let open Angstrom in
  let crlf = string "\r\n" in
  let field =
    take_while is_ftext >>= fun field_name ->
    skip_while is_wsp *> char ':' *>
    Unstrctrd_parser.unstrctrd (Bytes.create length) >>= fun v -> return (field_name, v) in
      (field >>| fun v -> `Field v)
  <|> (crlf *> return `End)

let blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let valid_mail_flow (input, length) expect =
  Alcotest.test_case (Fmt.strf "header") `Quick @@ fun () ->
  let raw = Bigstringaf.create (2 * length) in
  let q = Q.from raw in

  let rec read input acc = function
    | Angstrom.Unbuffered.Partial { committed= 0; continue; } ->
      ( match input with
        | [] -> go [] acc (continue raw ~off:0 ~len:0 Angstrom.Unbuffered.Complete)
        | x :: r ->
          let _ = Q.N.push_exn q ~blit:blit_from_string ~length:String.length ~off:0 ~len:(String.length x) x in
          let off = 0 and len = Q.length q in
          let[@warning "-8"] [ x ] = Q.N.peek q in
          Fmt.epr ">>> @[<hov>%a@].\n%!" (Hxd_string.pp Hxd.O.default) (Bigstringaf.to_string x) ;
          go r acc (continue raw ~off ~len Angstrom.Unbuffered.Incomplete) )
    | s -> go input acc s

  and go input acc = function
    | Angstrom.Unbuffered.Partial { committed; continue; } ->
      Fmt.epr "[partial] committed: %d.\n%!" committed ;
      Q.N.shift_exn q committed ;
      Q.compress q ;
      let more = match input with
        | [] -> Angstrom.Unbuffered.Complete
        | _  -> Angstrom.Unbuffered.Incomplete in
      let off = 0 and len = Q.length q in
      read input acc (continue raw ~off ~len more)
    | Angstrom.Unbuffered.Fail (committed, _, err) ->
      Fmt.epr "[fail] committed: %d.\n%!" committed ;
      Q.N.shift_exn q committed ;
      Alcotest.fail err
    | Angstrom.Unbuffered.Done (committed, `End) ->
      Fmt.epr "[done] committed: %d.\n%!" committed ;
      Q.N.shift_exn q committed ;
      Q.compress q ;

      ( match Q.N.peek q with
        | [] -> List.rev acc
        | _  -> Alcotest.fail "We did not consumed all input" )
    | Angstrom.Unbuffered.Done (committed, `Field (field_name, v)) ->
      Fmt.epr "[done] committed: %d.\n%!" committed ;
      Fmt.epr ">>> %S\n%!" (Unstrctrd.to_utf_8_string v) ;
      let v = String.concat ":" [ field_name; Unstrctrd.to_utf_8_string (Unstrctrd.fold_fws v) ] in
      Q.N.shift_exn q committed ;
      go input (v :: acc) (Angstrom.Unbuffered.parse (parser length)) in

  let fields = go input [] (Angstrom.Unbuffered.parse (parser length)) in
  Fmt.epr "Header is good.\n%!" ;
  Alcotest.(check (list string)) "fields" fields expect 

let invalid_unstructured_flow input =
  Alcotest.test_case (Fmt.strf "[%a]" Fmt.(list ~sep:(always "; ") (fmt "%S")) input) `Quick @@ fun () ->
  let buffer = Bigstringaf.create 0x1000 in
  let rec go len input = function
    | Angstrom.Unbuffered.Done (committed, v) ->
      let rest = rest_of_input ~committed ~len buffer input in
      Ok (v, rest)
    | Fail (_, _, err) -> Error err
    | Partial { committed; continue; } ->
      let len = len - committed in
      Bigstringaf.blit buffer ~src_off:committed buffer ~dst_off:0 ~len ;

      match input with
      | [] -> go len [] (continue buffer ~off:0 ~len Complete)
      | x :: r ->
        Bigstringaf.blit_from_string x ~src_off:0 buffer ~dst_off:len ~len:(String.length x) ;
        go (len + String.length x) r (continue buffer ~off:0 ~len:(len + String.length x) Incomplete) in
  let buf = Bytes.create 0x1000 in
  let res = go 0 input (Angstrom.Unbuffered.parse (Unstrctrd_parser.unstrctrd buf)) in
  match res with
  | Ok (v, rest) -> Alcotest.failf "Unexpected result: %S and %S" (Unstrctrd.to_utf_8_string v) rest
  | Error _ -> ()

let parser =
  let is_ftext = function '\033' .. '\057' | '\059' .. '\126' -> true | _ -> false in
  let is_wsp = function ' ' | '\t' -> true | _ -> false in
  let open Angstrom in
  take_while is_ftext >>= fun field_name ->
  Fmt.epr ">>> Start to recognize value of %S\n%!" field_name ;
  let buf = Bytes.create 0x7f in
  skip_while is_wsp *> char ':' *> Unstrctrd_parser.unstrctrd buf >>= fun v ->
  let res =
    let open Rresult in
    Unstrctrd.without_comments v >>| Unstrctrd.fold_fws >>| Unstrctrd.to_utf_8_string in
  match res with
  | Ok v -> return (field_name, `String v)
  | Error _ -> return (field_name, `Unstructured v)

let value =
  let pp ppf = function
    | `String v -> Fmt.string ppf v
    | `Unstructured _ -> Fmt.string ppf "#unstrctrd" in
  Alcotest.testable pp ( = )

let valid_unstructured_string input (field_name', v') =
  Alcotest.test_case (Fmt.strf "%S" input) `Quick @@ fun () ->
  let res =
    let open Rresult in
    ( R.reword_error R.msg <.> Angstrom.parse_string parser) input in
  match res with
  | Ok (field_name, v) ->
    Alcotest.(check string) "field-name" field_name field_name' ;
    Alcotest.(check value) "result" v v'
  | Error (`Msg err) -> Alcotest.failf "%s" err

let valid_unstructured_strings input lst' =
  Alcotest.test_case (Fmt.strf "%S" input) `Quick @@ fun () ->
  let res =
    let open Rresult in
    ( R.reword_error R.msg <.> Angstrom.(parse_string (many parser)) ) input in
  match res with
  | Ok lst ->
    List.iter2
      (fun (field_name', v') (field_name, v) ->
        Alcotest.(check string) "field-name" field_name field_name' ;
        Alcotest.(check value) "result" v v')
      lst' lst
  | Error (`Msg err) -> Alcotest.failf "%s" err

let input0 =
{|From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>

|}

let input1 =
  [ String.concat ""
      [ "\x52\x65\x74\x75\x72\x6e\x2d\x50\x61\x74\x68\x3a\x20\x3c\x6e\x6f" (* Return-Path: <no *)
      ; "\x72\x65\x70\x6c\x79\x40\x67\x69\x74\x68\x75\x62\x2e\x63\x6f\x6d" (* reply@github.com *)
      ; "\x3e\x0d\x0a\x44\x65\x6c\x69\x76\x65\x72\x65\x64\x2d\x54\x6f\x3a" (* >..Delivered-To: *)
      ; "\x20\x72\x6f\x6d\x61\x69\x6e\x2e\x63\x61\x6c\x61\x73\x63\x69\x62" (*  romain.calascib *)
      ; "\x65\x74\x74\x61\x40\x67\x6d\x61\x69\x6c\x2e\x63\x6f\x6d\x0d\x0a" (* etta@gmail.com.. *)
      ; "\x52\x65\x63\x65\x69\x76\x65\x64\x3a\x20\x62\x79\x20\x32\x30\x30" (* Received: by 200 *)
      ; "\x32\x3a\x61\x30\x63\x3a\x38\x62\x36\x65\x3a\x30\x3a\x30\x3a\x30" (* 2:a0c:8b6e:0:0:0 *)
      ; "\x3a\x30\x3a\x30\x20\x77\x69\x74\x68\x20\x53\x4d\x54\x50\x20\x69" (* :0:0 with SMTP i *)
      ; "\x64\x20\x64\x34\x36\x63\x73\x70\x31\x30\x35\x33\x37\x35\x37\x34" (* d d46csp10537574 *)
      ; "\x71\x76\x63\x3b\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x54\x68" (* qvc;..        Th *)
      ; "\x75\x2c\x20\x36\x20\x44\x65\x63\x20\x32\x30\x31\x38\x20\x30\x34" (* u, 6 Dec 2018 04 *)
      ; "\x3a\x34\x35\x3a\x33\x36\x20\x2d\x30\x38\x30\x30\x20\x28\x50\x53" (* :45:36 -0800 (PS *)
      ; "\x54\x29\x0d\x0a\x58\x2d\x47\x6f\x6f\x67\x6c\x65\x2d\x53\x6d\x74" (* T)..X-Google-Smt *)
      ; "\x70\x2d\x53\x6f\x75\x72\x63\x65\x3a\x20\x41\x46\x53\x47\x44\x2f" (* p-Source: AFSGD/ *)
      ; "\x57\x55\x56\x77\x42\x33\x34\x72\x68\x62\x37\x57\x7a\x72\x64\x64" (* WUVwB34rhb7Wzrdd *)
      ; "\x79\x72\x7a\x6c\x70\x54\x49\x74\x53\x32\x42\x44\x52\x69\x46\x56" (* yrzlpTItS2BDRiFV *)
      ; "\x46\x35\x61\x42"                                                 (* F5aB *) ]
  ; String.concat ""
      [ "\x67\x64\x58\x46\x6e\x70\x54\x58\x71\x34\x46\x65\x74\x38\x42\x66" (* gdXFnpTXq4Fet8Bf *)
      ; "\x64\x35\x42\x37\x48\x32\x38\x75\x4a\x67\x47\x68\x46\x6b\x4d\x56" (* d5B7H28uJgGhFkMV *)
      ; "\x64\x6a\x0d\x0a\x58\x2d\x52\x65\x63\x65\x69\x76\x65\x64\x3a\x20" (* dj..X-Received:  *)
      ; "\x62\x79\x20\x32\x30\x30\x32\x3a\x61\x33\x37\x3a\x35\x32\x64\x36" (* by 2002:a37:52d6 *)
      ; "\x3a\x3a\x20\x77\x69\x74\x68\x20\x53\x4d\x54\x50\x20\x69\x64\x20" (* :: with SMTP id  *)
      ; "\x67\x32\x30\x35\x6d\x72\x32\x36\x33\x39\x35\x36\x31\x36\x71\x6b" (* g205mr26395616qk *)
      ; "\x62\x2e\x33\x33\x35\x2e\x31\x35\x34\x34\x31\x30\x30\x33\x33\x36" (* b.335.1544100336 *)
      ; "\x34\x33\x31\x3b\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x54\x68" (* 431;..        Th *)
      ; "\x75\x2c\x20\x30\x36\x20\x44\x65\x63\x20\x32\x30\x31\x38\x20\x30" (* u, 06 Dec 2018 0 *)
      ; "\x34\x3a\x34\x35\x3a\x33\x36\x20\x2d\x30\x38\x30\x30\x20\x28\x50" (* 4:45:36 -0800 (P *)
      ; "\x53\x54\x29\x0d\x0a\x0d\x0a"                                     (* ST)....*) ] ]

let () =
  Alcotest.run "unstrctrd + angstrom"
    [ "valid", [ valid_unstructured_flow [ "Hello World!\r\n" ] ("Hello World!", "")
               ; valid_unstructured_flow [ "\r\r\n" ] ("\r", "")
               ; valid_unstructured_flow [ "\r\r\r\n" ] ("\r\r", "")
               ; valid_unstructured_flow [ "\r\n \r\r\n" ] ("\r\n \r", "")
               ; valid_unstructured_flow [ "\n\r\n" ] ("\n", "")
               ; valid_unstructured_flow [ "\n\n\r\n \r\n" ] ("\n\n\r\n ", "")
               ; valid_unstructured_flow [ "Hello World!\r\nSubject" ] ("Hello World!", "Subject")
               ; valid_unstructured_flow [ "Hello"; " "; "World!\r\n" ] ("Hello World!", "")
               ; valid_unstructured_flow [ "Hello"; ""; ""; ""; ""; ""; "\r\n"; "Subject" ] ("Hello", "Subject")
               ; valid_unstructured_flow [ "Hello"; "\r\n "; ""; "World!"; "\r\n" ] ("Hello\r\n World!", "")
               ; valid_unstructured_flow [ "Hello"; "\r\n"; " "; "\r\n"; ""; " "; ""; "World!\r\n" ] ("Hello\r\n \r\n World!", "")
               ; valid_unstructured_flow [ "Hello"; "\r"; "\n"; "Subject" ] ("Hello", "Subject")
               ; valid_unstructured_flow [ "Hello"; "\r"; "\n"; "\r"; "\n" ] ("Hello", "\r\n")
               ; valid_unstructured_string input0 ("From", `String " John Doe <jdoe@machine.example>")
               ; valid_unstructured_strings input0 [ ("From", `String " John Doe <jdoe@machine.example>")
                                                   ; ("To", `String " Mary Smith <mary@example.net>") ]
               ; valid_mail_flow (input1, 0x100) [ "Return-Path: <noreply@github.com>"
                                                 ; "Delivered-To: romain.calascibetta@gmail.com"
                                                 ; "Received: by 2002:a0c:8b6e:0:0:0:0:0 with SMTP id d46csp10537574qvc\
                                                    ;        Thu, 6 Dec 2018 04:45:36 -0800 (PST)"
                                                 ; "X-Google-Smtp-Source: AFSGD/WUVwB34rhb7WzrddyrzlpTItS2BDRiFVF5aBgdXFnpTXq4Fet8Bfd5B7H28uJgGhFkMVdj"
                                                 ; "X-Received: by 2002:a37:52d6:: with SMTP id g205mr26395616qkb.335.1544100336431\
                                                    ;        Thu, 06 Dec 2018 04:45:36 -0800 (PST)" ] ]
    ; "invalid", [ invalid_unstructured_flow [ "Hello" ]
                 ; invalid_unstructured_flow [ "Hello\r" ]
                 ; invalid_unstructured_flow [ "Hello\r\n " ] ] ]
