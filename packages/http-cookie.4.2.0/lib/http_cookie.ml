(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * http-cookie v4.2.0
 *-------------------------------------------------------------------------*)

type t =
  { name: string
  ; value: string
  ; path: string option
  ; domain: string option
  ; expires: date_time option
  ; max_age: int64 option
  ; secure: bool
  ; http_only: bool
  ; same_site: same_site option
  ; extension: string option }

and date_time =
  { year: int
  ; month:
      [ `Jan
      | `Feb
      | `Mar
      | `Apr
      | `May
      | `Jun
      | `Jul
      | `Aug
      | `Sep
      | `Oct
      | `Nov
      | `Dec ]
  ; weekday: [`Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat]
  ; day: int
  ; hour: int
  ; minutes: int
  ; seconds: int }

and same_site = [`None | `Lax | `Strict]

(* Pretty Printers *)
let rec pp fmt' t =
  let fields =
    [ Fmt.field "name" (fun p -> p.name) Fmt.string
    ; Fmt.field "value" (fun p -> p.value) Fmt.string
    ; Fmt.field "path" (fun p -> p.path) Fmt.(option string)
    ; Fmt.field "domain" (fun p -> p.domain) Fmt.(option string)
    ; Fmt.field "expires" (fun p -> p.expires) Fmt.(option pp_date_time)
    ; Fmt.field "max_age" (fun p -> p.max_age) Fmt.(option int64)
    ; Fmt.field "secure" (fun p -> p.secure) Fmt.bool
    ; Fmt.field "http_only" (fun p -> p.http_only) Fmt.bool
    ; Fmt.field "same_site" (fun p -> p.same_site) Fmt.(option pp_same_site)
    ; Fmt.field "extension" (fun p -> p.extension) Fmt.(option string) ]
  in
  Fmt.(vbox (record fields) fmt' t)

and pp_date_time fmt tm =
  let weekday =
    match tm.weekday with
    | `Sun -> "Sun"
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
  in
  let month =
    match tm.month with
    | `Jan -> "Jan"
    | `Feb -> "Feb"
    | `Mar -> "Mar"
    | `Apr -> "Apr"
    | `May -> "May"
    | `Jun -> "Jun"
    | `Jul -> "Jul"
    | `Aug -> "Aug"
    | `Sep -> "Sep"
    | `Oct -> "Oct"
    | `Nov -> "Nov"
    | `Dec -> "Dec"
  in
  Format.fprintf fmt "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday tm.day month
    tm.year tm.hour tm.minutes tm.seconds

and pp_same_site fmt = function
  | `None -> Format.fprintf fmt "None"
  | `Lax -> Format.fprintf fmt "Lax"
  | `Strict -> Format.fprintf fmt "Strict"

and to_string pp t =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a%!" pp t ;
  Buffer.contents buf

let pp_rfc1123 = pp_date_time
let date_to_string tm = to_string pp_date_time tm
let same_site_to_string ss = to_string pp_same_site ss

(* Conversion *)

let to_weekday = function
  | "Sun" | "Sunday" -> `Sun
  | "Mon" | "Monday" -> `Mon
  | "Tue" | "Tuesday" -> `Tue
  | "Wed" | "Wednesday" -> `Wed
  | "Thu" | "Thursday" -> `Thu
  | "Fri" | "Friday" -> `Fri
  | "Sat" | "Saturday" -> `Sat
  | _ -> assert false

let to_month = function
  | "Jan" -> `Jan
  | "Feb" -> `Feb
  | "Mar" -> `Mar
  | "Apr" -> `Apr
  | "May" -> `May
  | "Jun" -> `Jun
  | "Jul" -> `Jul
  | "Aug" -> `Aug
  | "Sep" -> `Sep
  | "Oct" -> `Oct
  | "Nov" -> `Nov
  | "Dec" -> `Dec
  | _ -> assert false

(* Compare *)
let compare (t1 : t) (t2 : t) = Stdlib.compare t1 t2

let compare_date_time (dt1 : date_time) (dt2 : date_time) =
  Stdlib.compare dt1 dt2

(* Cookie parsers.

   https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1

   set-cookie-header = "Set-Cookie:" SP set-cookie-string
   set-cookie-string = cookie-pair *( ";" SP cookie-av )
   cookie-pair       = cookie-name "=" cookie-value
   cookie-name       = token
   cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
   cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                         ; US-ASCII characters excluding CTLs,
                         ; whitespace DQUOTE, comma, semicolon,
                         ; and backslash
   token             = <token, defined in [RFC2616], Section 2.2>

   cookie-av         = expires-av / max-age-av / domain-av /
                       path-av / secure-av / httponly-av /
                       extension-av
   expires-av        = "Expires=" sane-cookie-date
   sane-cookie-date  = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
   max-age-av        = "Max-Age=" non-zero-digit *DIGIT
                         ; In practice, both expires-av and max-age-av
                         ; are limited to dates representable by the
                         ; user agent.
   non-zero-digit    = %x31-39
                         ; digits 1 through 9
   domain-av         = "Domain=" domain-value
   domain-value      = <subdomain>
                         ; defined in [RFC1034], Section 3.5, as
                         ; enhanced by [RFC1123], Section 2.1
   path-av           = "Path=" path-value
   path-value        = <any CHAR except CTLs or ";">
   secure-av         = "Secure"
   httponly-av       = "HttpOnly"
   extension-av      = <any CHAR except CTLs or ";">
*)
open Angstrom

let token =
  take_while1 (function
    | '\x00' .. '\x1F' | '\x7F' -> false (* CONTROL chars *)
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
     |']' | '?' | '=' | '{' | '}' | ' ' ->
        false (* SEPARATOR chars *)
    | _ -> true )

let cookie_name = token

(*
cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                      ; US-ASCII characters excluding CTLs,
                      ; whitespace DQUOTE, comma, semicolon,
                      ; and backslash
*)
let cookie_value =
  let cookie_octet = function
    | '\x21'
     |'\x23' .. '\x2B'
     |'\x2D' .. '\x3A'
     |'\x3C' .. '\x5B'
     |'\x5D' .. '\x7E' ->
        true
    | _ -> false
  in
  take_while cookie_octet <|> (char '"' *> take_while cookie_octet <* char '"')

let cookie_pair =
  let* name = cookie_name in
  let+ value = char '=' *> cookie_value in
  (name, value)

(* Domain attribute value:

    domain-value      = <subdomain>
                       ; defined in [RFC1034], Section 3.5, as
                       ; enhanced by [RFC1123], Section 2.1

   https://datatracker.ietf.org/doc/html/rfc1034#section-3.5

    <subdomain> ::= <label> | <subdomain> "." <label>
    <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
    <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
    <let-dig-hyp> ::= <let-dig> | "-"
    <let-dig> ::= <letter> | <digit>
    <letter> ::= any one of the 52 alphabetic characters A through Z or a to z
    <digit> ::= any one of the ten digits 0 through 9
*)
let string_of_list l = List.to_seq l |> String.of_seq
let[@inline always] is_digit = function '0' .. '9' -> true | _ -> false

let[@inline always] is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let digit = satisfy is_digit
let letter = satisfy is_letter
let space = char ' '

let domain_name =
  let let_dig = letter <|> digit in
  let let_dig_hyp = let_dig <|> char '-' in
  let ldh_str = many1 let_dig_hyp in
  let label =
    let* first_char = letter in
    let* middle_chars = option [] ldh_str in
    let+ middle_chars =
      let len = List.length middle_chars in
      if len > 0 && len <= 62 then
        (* 62 = 63 - first_char *)
        let last_char = List.nth middle_chars (len - 1) in
        if is_digit last_char || is_letter last_char then return middle_chars
        else
          fail
            (Format.sprintf
               "Invalid 'Domain' cookie attribute value: %s. middle characters \
                must end with either a letter or a digit"
               (string_of_list middle_chars) )
      else if len = 0 then return []
      else
        fail
          (Format.sprintf
             "Invalid 'Domain' cookie attribute value: %s. label must 63 \
              characters or less."
             (string_of_list middle_chars) )
    in
    string_of_list (first_char :: middle_chars)
  in
  let subdomain = sep_by1 (char '.') label in
  (* A cookie domain attribute value may start with a leading dot which
     can ignored.

     https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.2.3
  *)
  let* () = option () (char '.' *> return ()) in
  let* start_pos = pos in
  let* subdomain = subdomain in
  let* end_pos = pos in
  let len = end_pos - start_pos in
  if len > 255 then
    fail (Format.sprintf "Domain attribute length exceeds 255 characters")
  else return (String.concat "." subdomain)

(*
     IPv4address = d8 "." d8 "." d8 "." d8

        d8          = DIGIT               ; 0-9
                    / %x31-39 DIGIT       ; 10-99
                    / "1" 2DIGIT          ; 100-199
                    / "2" %x30-34 DIGIT   ; 200-249
                    / "25" %x30-35        ; 250-255
*)
let ipv4_address =
  let d8 =
    lift2
      (fun s1 c -> Format.sprintf "%s%c" s1 c)
      (string "25")
      (satisfy (function '\x30' .. '\x35' -> true | _ -> false))
    <|> lift3
          (fun c1 c2 c3 -> Format.sprintf "%c%c%c" c1 c2 c3)
          (char '2')
          (satisfy (function '\x30' .. '\x34' -> true | _ -> false))
          digit
    <|> lift3
          (fun c1 c2 c3 -> Format.sprintf "%c%c%c" c1 c2 c3)
          (char '1') digit digit
    <|> lift2
          (fun c1 c2 -> Format.sprintf "%c%c" c1 c2)
          (satisfy (function '\x31' .. '\x39' -> true | _ -> false))
          digit
    <|> (digit >>| Format.sprintf "%c")
  in
  let* oct1 = d8 <* char '.' in
  let* oct2 = d8 <* char '.' in
  let* oct3 = d8 <* char '.' in
  let+ oct4 = d8 in
  Format.sprintf "%s.%s.%s.%s" oct1 oct2 oct3 oct4

(*
   IPv6address =                            6(h16 ":") ls32
                 /                     "::" 5(h16 ":") ls32
                 / [             h16 ] "::" 4(h16 ":") ls32
                 / [ *1(h16 ":") h16 ] "::" 3(h16 ":") ls32
                 / [ *2(h16 ":") h16 ] "::" 2(h16 ":") ls32
                 / [ *3(h16 ":") h16 ] "::"   h16 ":"  ls32
                 / [ *4(h16 ":") h16 ] "::"            ls32
                 / [ *5(h16 ":") h16 ] "::"             h16
                 / [ *6(h16 ":") h16 ] "::"
           ls32 = h16 ":" h16 / IPv4address
           h16  = 1*4HEXDIG
*)
let ipv6_address =
  let h16 =
    let* hexdigits =
      take_while1 (function
        | c when is_digit c -> true
        | 'A' .. 'F' -> true
        | 'a' .. 'f' -> true
        | _ -> false )
    in
    if String.length hexdigits <= 4 then return (`H16 hexdigits)
    else
      fail
        (Format.sprintf "IPv6 component must be 1 to 4 hex digits long: %s"
           hexdigits )
  in
  let dbl_colon = string "::" *> return `Dbl_colon in
  let* ip_parts =
    let ipv4 = ipv4_address >>| fun ipv4 -> `IPv4 ipv4 in
    let[@inline always] not_empty s = String.length s > 0 in
    let p =
      option "" (peek_string 2)
      >>= fun s ->
      match s with
      | "::" -> dbl_colon
      | s when not_empty s && s.[0] = ':' -> char ':' *> (ipv4 <|> h16)
      | _ -> ipv4 <|> h16
    in
    many1 p
  in
  let dbl_colon_exists =
    List.exists
      (function `Dbl_colon -> true | `IPv4 _ | `H16 _ -> false)
      ip_parts
  in
  let is_ipv4 = function `IPv4 _ -> true | `Dbl_colon | `H16 _ -> false in
  let ipv4_exists = List.exists is_ipv4 ip_parts in
  let len' = List.length ip_parts in
  let h16_len =
    List.filter (function `H16 _ -> true | _ -> false) ip_parts |> List.length
  in
  let exception Invalid_IPv6 of string in
  let validate_ipv4 () =
    if ipv4_exists then
      let is_ipv4_last = is_ipv4 @@ List.nth ip_parts (len' - 1) in
      if is_ipv4_last then ()
      else
        raise
          (Invalid_IPv6
             (Format.sprintf
                "Invalid IPv6 address. If IP v4 is specified, it must be the \
                 last component" ) )
    else ()
  in
  let validate_dbl_colons () =
    let len =
      List.find_all (function `Dbl_colon -> true | _ -> false) ip_parts
      |> List.length
    in
    if len > 1 then
      raise
        (Invalid_IPv6
           (Format.sprintf "Invalid IPv6 address. '::' is only allowed once.")
        )
    else ()
  in
  let validate_parts_count () =
    if len' = 1 && dbl_colon_exists then ()
    else if dbl_colon_exists && (not ipv4_exists) && h16_len <= 7 then ()
    else if dbl_colon_exists && ipv4_exists && h16_len <= 5 then ()
    else if (not dbl_colon_exists) && (not ipv4_exists) && h16_len = 8 then ()
    else if (not dbl_colon_exists) && ipv4_exists && h16_len <= 6 then ()
    else raise (Invalid_IPv6 (Format.sprintf "Invalid IPv6 address components"))
  in
  try
    validate_dbl_colons () ;
    validate_ipv4 () ;
    validate_parts_count () ;
    let ip =
      if len' = 1 then "::"
      else
        ip_parts
        |> List.mapi (fun i -> function
             | `H16 h16 -> h16
             | `IPv4 ipv4 -> ipv4
             | `Dbl_colon -> if i = 0 || i = len' - 1 then ":" else "" )
        |> String.concat ":"
    in
    return ip
  with Invalid_IPv6 s -> fail s

let domain_value = ipv4_address <|> ipv6_address <|> domain_name

let cookie_attr_value =
  take_while1 (function
    | '\x00' .. '\x1F' | '\x7F' -> false (* CONTROL chars *)
    | ';' -> false
    | _ -> true )

let path_value = cookie_attr_value
let extension_value = cookie_attr_value

(* https://datatracker.ietf.org/doc/html/rfc7231#section-7.1.1.1

   HTTP cookie specifies RFC 1123 date. The rest is included here
   for completeness.

   HTTP-date    = rfc1123-date | rfc850-date | asctime-date
   rfc1123-date = wkday "," SP date1 SP time SP "GMT"
   rfc850-date  = weekday "," SP date2 SP time SP "GMT"
   asctime-date = wkday SP date3 SP time SP 4DIGIT
   date1        = 2DIGIT SP month SP 4DIGIT
                 ; day month year (e.g., 02 Jun 1982)
   date2        = 2DIGIT "-" month "-" 2DIGIT
                 ; day-month-year (e.g., 02-Jun-82)
   date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
                 ; month day (e.g., Jun  2)
   time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
                 ; 00:00:00 - 23:59:59
   wkday        = "Mon" | "Tue" | "Wed"
               | "Thu" | "Fri" | "Sat" | "Sun"
   weekday      = "Monday" | "Tuesday" | "Wednesday"
               | "Thursday" | "Friday" | "Saturday" | "Sunday"
   month        = "Jan" | "Feb" | "Mar" | "Apr"
               | "May" | "Jun" | "Jul" | "Aug"
               | "Sep" | "Oct" | "Nov" | "Dec"
*)
let http_date =
  let wkday =
    string "Mon" <|> string "Tue" <|> string "Wed" <|> string "Thu"
    <|> string "Fri" <|> string "Sat" <|> string "Sun"
  in
  let digits count =
    let* digits = list @@ List.init count (fun _ -> digit) in
    let digits = string_of_list digits in
    try return (int_of_string digits)
    with exn ->
      fail
        (Format.sprintf "Invalid integer value:%s. %s" digits
           (Printexc.to_string exn) )
  in
  let time =
    let* hh =
      digits 2 <* char ':'
      >>= fun hour ->
      if hour >= 0 && hour < 24 then return hour
      else
        fail
        @@ Format.sprintf
             "Invalid hour value: %d. Hour must be in between 0 and 23 \
              inclusive"
             hour
    in
    let* mm =
      digits 2 <* char ':'
      >>= fun minutes ->
      if minutes >= 0 && minutes <= 59 then return minutes
      else
        fail
        @@ Format.sprintf
             "Invalid minutes value: %d. Minutes must be in between 0 and 59 \
              inclusive"
             minutes
    in
    let+ ss =
      digits 2
      >>= fun seconds ->
      if seconds >= 0 && seconds <= 59 then return seconds
      else
        fail
        @@ Format.sprintf
             "Invalid seconds value: %d. Seconds must be in between 0 and 59 \
              inclusive"
             seconds
    in
    (hh, mm, ss)
  in
  let month =
    string "Jan" <|> string "Feb" <|> string "Mar" <|> string "Apr"
    <|> string "May" <|> string "Jun" <|> string "Jul" <|> string "Aug"
    <|> string "Sep" <|> string "Oct" <|> string "Nov" <|> string "Dec"
  in
  let validate_year year =
    if year < 1601 then
      fail (Format.sprintf "Invalid year: %d. Year is less than 1601" year)
    else return year
  in
  let day_of_month day =
    if day >= 1 && day <= 31 then return day
    else
      fail
        (Format.sprintf
           "Invalid day of month: %d. Day of month must be in between 1 and 31 \
            inclusive."
           day )
  in
  let date1 =
    let* day = digits 2 <* space >>= day_of_month in
    let* month = month <* space >>| to_month in
    let+ year = digits 4 >>= validate_year in
    (day, month, year)
  in
  let rfc1123_date =
    let* weekday = wkday <* char ',' *> space >>| to_weekday in
    let* day, month, year = date1 <* space in
    let+ hour, minutes, seconds = time <* space *> string "GMT" in
    {weekday; day; month; year; hour; minutes; seconds}
  in
  rfc1123_date

let max_age_value =
  let non_zero_digit = satisfy (function '1' .. '9' -> true | _ -> false) in
  let* first_char = non_zero_digit in
  let* digits = take_while is_digit in
  let max_age = Format.sprintf "%c%s" first_char digits in
  try return (Int64.of_string max_age)
  with exn ->
    fail
      (Format.sprintf "Invalid max_age value:%s. %s" max_age
         (Printexc.to_string exn) )

let cookie_av =
  let expires_av =
    string "Expires=" *> http_date >>| fun v -> `Expires (Some v)
  in
  let max_age_av =
    string "Max-Age=" *> max_age_value >>| fun v -> `Max_age (Some v)
  in
  let domain_av =
    string "Domain=" *> domain_value >>| fun v -> `Domain (Some v)
  in
  let path_av = string "Path=" *> path_value >>| fun v -> `Path (Some v) in
  let secure_av = string "Secure" *> return `Secure in
  let httponly_av = string "HttpOnly" *> return `Http_only in
  let extension_av = extension_value >>| fun v -> `Extension (Some v) in
  expires_av <|> max_age_av <|> domain_av <|> path_av <|> secure_av
  <|> httponly_av <|> extension_av

let set_cookie_string =
  let* name, value = cookie_pair in
  let+ attr_values = many (char ';' *> space *> cookie_av) in
  (name, value, attr_values)

(* https://datatracker.ietf.org/doc/html/rfc6265#section-4.2.1

   cookie-header = "Cookie:" OWS cookie-string OWS
   cookie-string = cookie-pair *( ";" SP cookie-pair )
*)
let cookie_string =
  let* cookies = sep_by1 (char ';' *> char '\x20') cookie_pair in
  let name_counts = Hashtbl.create 0 in
  List.iter
    (fun (name, _) ->
      match Hashtbl.find_opt name_counts name with
      | Some count -> Hashtbl.replace name_counts name (count + 1)
      | None -> Hashtbl.replace name_counts name 1 )
    cookies ;
  let name_counts = Hashtbl.to_seq_values name_counts |> List.of_seq in
  if List.exists (fun count -> count > 1) name_counts then
    fail "duplicate cookies found"
  else return cookies

let parse_name name =
  parse_string ~consume:Consume.All cookie_name name
  |> function
  | Ok _ as ok -> ok | Error _ -> Error (Format.sprintf "name: %s" name)

let parse_value value =
  parse_string ~consume:Consume.All cookie_value value
  |> function
  | Ok _ as ok -> ok | Error _ -> Error (Format.sprintf "value: %s" value)

let parse_max_age max_age =
  match max_age with
  | None -> Ok None
  | Some ma ->
      if ma <= 0L then
        Error
          (Format.sprintf
             "Cookies 'Max-Age' attribute is less than or equal to 0" )
      else Ok (Some ma)

let parse_opt ?error_label p input =
  match input with
  | Some input -> (
      parse_string ~consume:Consume.All (p >>| Option.some) input
      |> function
      | Ok _ as ok -> ok
      | Error err ->
          let error_label = Option.value ~default:err error_label in
          Error (Format.sprintf "%s: %s" error_label (String.escaped input)) )
  | None -> Ok None

let ( let* ) r f = Result.bind r f
let ( let+ ) r f = Result.map f r

let date_time ~year ~month ~weekday ~day ~hour ~minutes ~seconds =
  let* year =
    if year >= 1601 && year < 9999 then Ok year
    else Error (Format.sprintf "Invalid year (>1600 && < 9999): %d" year)
  in
  let* day =
    if day > 0 && day <= 31 then Ok day
    else Error (Format.sprintf "Invalid day of month ( > 0 && <= 31): %d" day)
  in
  let* hour =
    if hour > 0 && hour < 24 then Ok hour
    else Error (Format.sprintf "Invalid hour (>0 && <24): %d" hour)
  in
  let* minutes =
    if minutes >= 0 && minutes < 60 then Ok minutes
    else Error (Format.sprintf "Invalid minutes (>=0 && < 60): %d" minutes)
  in
  let* seconds =
    if seconds >= 0 && seconds < 60 then Ok seconds
    else Error (Format.sprintf "Invalid seconds (>=0 && < 60): %d" seconds)
  in
  Ok {year; month; weekday; day; hour; minutes; seconds}

let create ?path ?domain ?expires ?max_age ?(secure = false) ?(http_only = true)
    ?same_site ?extension ~name value =
  let* name = parse_name name in
  let* value = parse_value value in
  let* domain = parse_opt ~error_label:"domain" domain_value domain in
  let* path = parse_opt ~error_label:"path" path_value path in
  let* max_age = parse_max_age max_age in
  let+ extension =
    parse_opt ~error_label:"extension" extension_value extension
  in
  { name
  ; value
  ; path
  ; domain
  ; expires
  ; max_age
  ; secure
  ; http_only
  ; same_site
  ; extension }

let of_cookie header =
  parse_string ~consume:All cookie_string header
  |> Result.map (fun cookies' ->
         List.map
           (fun (name, value) ->
             { name
             ; value
             ; path= None
             ; domain= None
             ; expires= None
             ; max_age= None
             ; secure= false
             ; http_only= false
             ; same_site= None
             ; extension= None } )
           cookies' )
  |> Result.map_error (fun s -> Format.sprintf "Invalid cookie %s" s)

let to_cookie t = Format.sprintf "%s=%s" t.name t.value

let to_set_cookie t =
  let module O = Option in
  let buf = Buffer.create 50 in
  let add_str fmt = Format.ksprintf (Buffer.add_string buf) fmt in
  add_str "%s=%s" t.name t.value ;
  O.iter (fun path -> add_str "; Path=%s" path) t.path ;
  O.iter (fun d -> add_str "; Domain=%s" d) t.domain ;
  O.iter
    (fun expires -> add_str "; Expires=%s" @@ date_to_string expires)
    t.expires ;
  O.iter
    (fun max_age -> if max_age > 0L then add_str "; Max-Age=%Ld" max_age)
    t.max_age ;
  if t.secure then add_str "; Secure" ;
  if t.http_only then add_str "; HttpOnly" ;
  O.iter
    (fun same_site -> add_str "; SameSite=%s" (same_site_to_string same_site))
    t.same_site ;
  O.iter (fun extension -> add_str "; %s" extension) t.extension ;
  Buffer.contents buf

let of_set_cookie set_cookie =
  parse_string ~consume:Consume.All set_cookie_string set_cookie
  |> Result.map (fun (name, value, attr_values) ->
         List.fold_left
           (fun cookie -> function
             | `Expires expires -> {cookie with expires}
             | `Max_age max_age -> {cookie with max_age}
             | `Domain domain -> {cookie with domain}
             | `Path path -> {cookie with path}
             | `Secure -> {cookie with secure= true}
             | `Http_only -> {cookie with http_only= true}
             | `Extension extension -> {cookie with extension} )
           { name
           ; value
           ; path= None
           ; domain= None
           ; expires= None
           ; max_age= None
           ; secure= false
           ; http_only= false
           ; same_site= None
           ; extension= None }
           attr_values )
  |> Result.map_error (fun s -> Format.sprintf "Invalid 'Set-Cookie' data %s" s)

(* Attributes *)
let name c = c.name
let value c = c.value
let path c = c.path
let domain c = c.domain
let expires c = c.expires
let max_age c = c.max_age
let extension c = c.extension
let same_site c = c.same_site
let http_only c = c.http_only
let secure c = c.secure

(* Updates. *)
let update_value value cookie =
  let+ value = parse_value value in
  {cookie with value}

let update_name name cookie =
  let+ name = parse_name name in
  {cookie with name}

let update_path path cookie =
  let+ path = parse_opt path_value path in
  {cookie with path}

let update_domain domain cookie =
  let+ domain = parse_opt domain_value domain in
  {cookie with domain}

let update_expires expires cookie = {cookie with expires}

let update_max_age max_age cookie =
  let+ max_age = parse_max_age max_age in
  {cookie with max_age}

let update_secure secure cookie = {cookie with secure}
let update_http_only http_only cookie = {cookie with http_only}
let update_same_site same_site cookie = {cookie with same_site}

let update_extension extension cookie =
  let+ extension = parse_opt extension_value extension in
  {cookie with extension}
