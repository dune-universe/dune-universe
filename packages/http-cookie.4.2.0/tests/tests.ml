let () = Printexc.record_backtrace false

let print_date_time r =
  Fmt.result ~ok:Http_cookie.pp_date_time ~error:Fmt.string Fmt.stdout r

let%expect_test "date_time: year 0 " =
  Http_cookie.date_time ~year:0 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid year (>1600 && < 9999): 0 |}]

let%expect_test "date_time: year 10000" =
  Http_cookie.date_time ~year:10_000 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid year (>1600 && < 9999): 10000 |}]

let%expect_test "date_time: year 2021" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 23 Jan 2021 22:45:59 GMT |}]

let%expect_test "date_time: year 1601" =
  Http_cookie.date_time ~year:1601 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 23 Jan 1601 22:45:59 GMT |}]

let%expect_test "date_time: year 1600" =
  Http_cookie.date_time ~year:1600 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid year (>1600 && < 9999): 1600 |}]

let%expect_test "date_time: day 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:0 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid day of month ( > 0 && <= 31): 0 |}]

let%expect_test "date_time: day 31" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:31 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 31 Jan 2021 22:45:59 GMT |}]

let%expect_test "date_time: day 32" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:32 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid day of month ( > 0 && <= 31): 32 |}]

(* hour tests *)
let%expect_test "date_time: hour 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:0
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid hour (>0 && <24): 0 |}]

let%expect_test "date_time: hour 24" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:24
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid hour (>0 && <24): 24 |}]

let%expect_test "date_time: hour 22" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:45:59 GMT |}]

(* minute tests *)
let%expect_test "date_time: minute 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:0 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:00:59 GMT |}]

let%expect_test "date_time:minute 24" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:59 GMT |}]

let%expect_test "date_time: minute 60" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:60 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid minutes (>=0 && < 60): 60 |}]

(* seconds test *)
let%expect_test "date_time: seconds 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:0
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:00 GMT |}]

let%expect_test "date_time: seconds 59" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:59 GMT |}]

let%expect_test "date_time: seconds 60" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:60
  |> print_date_time ;
  [%expect {| Invalid seconds (>=0 && < 60): 60 |}]

let%expect_test "date_time: seconds -1" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:(-1)
  |> print_date_time ;
  [%expect {| Invalid seconds (>=0 && < 60): -1 |}]

(* create tests *)
let pp_t t =
  Fmt.result ~ok:Http_cookie.pp
    ~error:(fun fmt s -> Fmt.pf fmt "Error: %s" s)
    Fmt.stdout t

(* domain attribute parsing/validation tests *)
let%expect_test "create:  domain= eeee::192.168.0.1" =
  Http_cookie.create ~path:"/hello" ~domain:"eeee::192.168.0.1" ~name:"hello"
    "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: eeee::192.168.0.1
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=10.105.128.1" =
  Http_cookie.create ~path:"/hello" ~domain:"10.105.128.1" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 10.105.128.1
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=152.186.220.254" =
  Http_cookie.create ~path:"/hello" ~domain:"152.186.220.254" ~name:"hello"
    "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 152.186.220.254
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=::" =
  Http_cookie.create ~path:"/hello" ~domain:"::" ~name:"hello" "world" |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: ::
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=::1" =
  Http_cookie.create ~path:"/hello" ~domain:"::1" ~name:"hello" "world" |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: ::1
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=2002:2c26:f4e4:0:21c:42ff:fe20:4636" =
  Http_cookie.create ~path:"/hello"
    ~domain:"2002:2c26:f4e4:0:21c:42ff:fe20:4636" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 2002:2c26:f4e4:0:21c:42ff:fe20:4636
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=fec0::1" =
  Http_cookie.create ~path:"/hello" ~domain:"fec0::1" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: fec0::1
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=fe80::215:5dff:fe00:402" =
  Http_cookie.create ~path:"/hello" ~domain:"fe80::215:5dff:fe00:402"
    ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: fe80::215:5dff:fe00:402
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=2002:2c26:f4e4:0:21c:42ff:fe20:4636" =
  Http_cookie.create ~path:"/hello"
    ~domain:"2002:2c26:f4e4:0:21c:42ff:fe20:4636" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 2002:2c26:f4e4:0:21c:42ff:fe20:4636
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

let%expect_test "create:  domain=23ah" =
  Http_cookie.create ~path:"/hello" ~domain:"23ah" ~name:"hello" "world" |> pp_t ;
  [%expect {|
    Error: domain: 23ah |}]

let%expect_test "create:  domain=2333::ddd::1" =
  Http_cookie.create ~path:"/hello" ~domain:"2333::ddd::1" ~name:"hello" "world"
  |> pp_t ;
  [%expect {|
    Error: domain: 2333::ddd::1 |}]

(* name tests *)
let%expect_test "create: name=he@llo" =
  Http_cookie.create ~name:"he@llo" "world" |> pp_t ;
  [%expect {| Error: name: he@llo |}]

let%expect_test "create: name=he(llo" =
  Http_cookie.create ~name:"he(llo" "world" |> pp_t ;
  [%expect {| Error: name: he(llo |}]

let%expect_test "create: name=he>llo" =
  Http_cookie.create ~name:"he>llo" "world" |> pp_t ;
  [%expect {| Error: name: he>llo |}]

let%expect_test "create: name=he<llo" =
  Http_cookie.create ~name:"he<llo" "world" |> pp_t ;
  [%expect {| Error: name: he<llo |}]

(* value tests *)
let%expect_test "create: value=val dd (space ' ' is invalid)" =
  Http_cookie.create ~name:"hello" "val dd" |> pp_t ;
  [%expect {| Error: value: val dd |}]

let%expect_test "create: value=val,dd (',' is invalid)" =
  Http_cookie.create ~name:"hello" "val,dd" |> pp_t ;
  [%expect {| Error: value: val,dd |}]

let%expect_test "create: value=\00valdd ('\x00' is invalid)" =
  Http_cookie.create ~name:"hello" "\00valdd" |> pp_t ;
  [%expect {| Error: value: \00valdd |}]

(* path tests *)
let%expect_test "create: path=val;dd (';' is invalid)" =
  Http_cookie.create ~path:"val;dd" ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: path: val;dd |}]

let%expect_test "create: path=\x00valdd ('\x00' is invalid)" =
  Http_cookie.create ~path:"\x00valdd" ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: path: \000valdd |}]

let%expect_test "create: path=val dd" =
  Http_cookie.create ~path:"val dd" ~name:"hello" "value" |> pp_t ;
  [%expect
    {|
    name: hello
    value: value
    path: val dd
    domain:
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: |}]

(* extension tests*)
let%expect_test "create: extension=val;dd (';' is invalid)" =
  Http_cookie.create ~extension:"val;dd" ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: extension: val;dd |}]

let%expect_test "create: extension=\x00valdd ('\x00' is invalid)" =
  Http_cookie.create ~extension:"\x00valdd" ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: extension: \000valdd |}]

let%expect_test "create: extension=val dd" =
  Http_cookie.create ~extension:"val dd" ~name:"hello" "value" |> pp_t ;
  [%expect
    {|
    name: hello
    value: value
    path:
    domain:
    expires:
    max_age:
    secure: false
    http_only: true
    same_site:
    extension: val dd |}]

(* max_age tests*)
let%expect_test "create: max_age=0" =
  Http_cookie.create ~max_age:0L ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: Cookies 'Max-Age' attribute is less than or equal to 0 |}]

let%expect_test "create: max_age=-1" =
  Http_cookie.create ~max_age:(-1L) ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: Cookies 'Max-Age' attribute is less than or equal to 0 |}]

let%expect_test "create: max_age=23323" =
  Http_cookie.create ~max_age:23323L ~name:"hello" "value" |> pp_t ;
  [%expect
    {|
    name: hello
    value: value
    path:
    domain:
    expires:
    max_age: 23323
    secure: false
    http_only: true
    same_site:
    extension: |}]

(* of_cookie tests *)

let pp_t_list t =
  Fmt.vbox
    (Fmt.result
       ~ok:
         (Fmt.list
            ~sep:(fun fmt _ -> Fmt.pf fmt "@.@.")
            (Fmt.vbox Http_cookie.pp) )
       ~error:(fun fmt s -> Fmt.pf fmt "Error: %s" s) )
    Fmt.stdout t

let%expect_test "of_cookie: SID=31d4d96e407aad42; lang=en-US" =
  Http_cookie.of_cookie "SID=31d4d96e407aad42; lang=en-US" |> pp_t_list ;
  [%expect
    {|
    name: SID
    value: 31d4d96e407aad42
    path:
    domain:
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension:

    name: lang
    value: en-US
    path:
    domain:
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "of_cookie: SID=,31d4d96e407aad42; lang=en-US" =
  Http_cookie.of_cookie "SID=,31d4d96e407aad42; lang=en-US" |> pp_t_list ;
  [%expect {| Error: Invalid cookie : end_of_input |}]

let%expect_test "of_cookie: SID= 31d4d96e407aad42; lang=en-US" =
  Http_cookie.of_cookie "SID= 31d4d96e407aad42; lang=en-US" |> pp_t_list ;
  [%expect {| Error: Invalid cookie : end_of_input |}]

let%expect_test "of_cookie: SID>=31d4d96e407aad42; lang=en-US" =
  Http_cookie.of_cookie "SID>=31d4d96e407aad42; lang=en-US" |> pp_t_list ;
  [%expect {| Error: Invalid cookie : char '=' |}]

let%expect_test "of_cookie: SID>=31d4d96e407aad42; lang=en-US" =
  Http_cookie.of_cookie "SID=31d4d96e407aad42; SID=val2; lang=en-US"
  |> pp_t_list ;
  [%expect {| Error: Invalid cookie : duplicate cookies found |}]

(* to_cookie tests*)
let pp_to_cookie c =
  Fmt.result ~error:Fmt.string
    ~ok:(fun fmt c -> Fmt.pf fmt "%s" (Http_cookie.to_cookie c))
    Fmt.stdout c

let%expect_test "to_cookie: SID=31d4d96e407aad42" =
  Http_cookie.create ~name:"SID" "31asdfasddd" |> pp_to_cookie ;
  [%expect {| SID=31asdfasddd |}]

(* of_set_cookie tests *)
let%expect_test "of_set_cookie: " =
  Http_cookie.of_set_cookie
    "SID=31d4d96e407aad42; Path=/; Domain=example.com; Secure; HttpOnly; \
     Expires=Sun, 06 Nov 1994 08:49:37 GMT"
  |> pp_t ;
  [%expect
    {|
    name: SID
    value: 31d4d96e407aad42
    path: /
    domain: example.com
    expires: Sun, 06 Nov 1994 08:49:37 GMT
    max_age:
    secure: true
    http_only: true
    same_site:
    extension: |}]

let%expect_test "of_set_cookie: " =
  Http_cookie.of_set_cookie
    "SID=31d4d96e407aad42; Path=/; Domain=192.169.0.1; Secure; HttpOnly; \
     Expires=Sun, 06 Nov 1994 08:49:37 GMT"
  |> pp_t ;
  [%expect
    {|
    name: SID
    value: 31d4d96e407aad42
    path: /
    domain: 192.169.0.1
    expires: Sun, 06 Nov 1994 08:49:37 GMT
    max_age:
    secure: true
    http_only: true
    same_site:
    extension: |}]

let%expect_test "of_set_cookie: " =
  Http_cookie.of_set_cookie
    "SID=31d4d96e407aad42; Path=/; Domain=eee::eee:ffff:234; Secure; HttpOnly; \
     Expires=Sun, 06 Nov 1994 08:49:37 GMT"
  |> pp_t ;
  [%expect
    {|
    name: SID
    value: 31d4d96e407aad42
    path: /
    domain: eee::eee:ffff:234
    expires: Sun, 06 Nov 1994 08:49:37 GMT
    max_age:
    secure: true
    http_only: true
    same_site:
    extension: |}]

let%expect_test "of_set_cookie: " =
  Http_cookie.of_set_cookie
    "SID=31d4d96e407aad42; Path=/; Domain=eee::aaa:eee:ffff:234:192.168.0.1; \
     Secure; HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
  |> pp_t ;
  [%expect
    {|
    name: SID
    value: 31d4d96e407aad42
    path: /
    domain: eee::aaa:eee:ffff:234:192.168.0.1
    expires: Sun, 06 Nov 1994 08:49:37 GMT
    max_age:
    secure: true
    http_only: true
    same_site:
    extension: |}]

let%expect_test "of_set_cookie: " =
  Http_cookie.of_set_cookie
    "SID=31d4d96e407aad42; Path=/; Domain=eee::eee::ffff:234; Secure; \
     HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
  |> pp_t ;
  [%expect {|
    Error: Invalid 'Set-Cookie' data : end_of_input |}]

let%expect_test "of_set_cookie: " =
  Http_cookie.of_set_cookie
    "SID=31d4d96e407aad42; Path=i;; Domain=eee::eee:ffff:234; Secure; \
     HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
  |> pp_t ;
  [%expect {|
    Error: Invalid 'Set-Cookie' data : end_of_input |}]

(* to_set_cookie tests *)

let pp_to_set_cookie c = Printf.printf "%s" (Http_cookie.to_set_cookie c)

let%expect_test "to_set_cookie" =
  let expires =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Mon ~day:12 ~hour:23
      ~minutes:23 ~seconds:59
    |> Result.get_ok
  in
  Http_cookie.create ~path:"/home/about" ~domain:"example.com" ~expires
    ~max_age:2342342L ~secure:true ~http_only:true ~same_site:`Strict
    ~name:"hello" "value1"
  |> Result.get_ok |> pp_to_set_cookie ;
  [%expect
    {| hello=value1; Path=/home/about; Domain=example.com; Expires=Mon, 12 Jan 2021 23:23:59 GMT; Max-Age=2342342; Secure; HttpOnly; SameSite=Strict |}]

let%expect_test "to_set_cookie" =
  let expires =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Mon ~day:12 ~hour:23
      ~minutes:23 ~seconds:59
    |> Result.get_ok
  in
  Http_cookie.create ~path:"/home/about" ~domain:"example.com" ~expires
    ~max_age:2342342L ~http_only:true ~same_site:`Strict ~name:"hello" "value1"
  |> Result.get_ok |> pp_to_set_cookie ;
  [%expect
    {| hello=value1; Path=/home/about; Domain=example.com; Expires=Mon, 12 Jan 2021 23:23:59 GMT; Max-Age=2342342; HttpOnly; SameSite=Strict |}]

let%expect_test "to_set_cookie" =
  let expires =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Mon ~day:12 ~hour:23
      ~minutes:23 ~seconds:59
    |> Result.get_ok
  in
  Http_cookie.create ~path:"/home/about" ~domain:"example.com" ~expires
    ~max_age:2342342L ~same_site:`Strict ~name:"hello" "value1"
  |> Result.get_ok |> pp_to_set_cookie ;
  [%expect
    {| hello=value1; Path=/home/about; Domain=example.com; Expires=Mon, 12 Jan 2021 23:23:59 GMT; Max-Age=2342342; HttpOnly; SameSite=Strict |}]

let%expect_test "to_set_cookie" =
  let expires =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Mon ~day:12 ~hour:23
      ~minutes:23 ~seconds:59
    |> Result.get_ok
  in
  Http_cookie.create ~path:"/home/about" ~domain:"example.com" ~expires
    ~max_age:2342342L ~same_site:`Lax ~name:"hello" "value1"
  |> Result.get_ok |> pp_to_set_cookie ;
  [%expect
    {| hello=value1; Path=/home/about; Domain=example.com; Expires=Mon, 12 Jan 2021 23:23:59 GMT; Max-Age=2342342; HttpOnly; SameSite=Lax |}]

let%expect_test "to_set_cookie" =
  let expires =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Mon ~day:12 ~hour:23
      ~minutes:23 ~seconds:59
    |> Result.get_ok
  in
  Http_cookie.create ~path:"/home/about" ~domain:"198.168.0.1" ~expires
    ~max_age:2342342L ~same_site:`None ~name:"hello" "value1"
  |> Result.get_ok |> pp_to_set_cookie ;
  [%expect
    {| hello=value1; Path=/home/about; Domain=198.168.0.1; Expires=Mon, 12 Jan 2021 23:23:59 GMT; Max-Age=2342342; HttpOnly; SameSite=None |}]
