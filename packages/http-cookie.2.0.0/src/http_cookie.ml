(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * http-cookie v2.0.0
 *-------------------------------------------------------------------------*)
module Same_site = struct
  type t =
    | Default
    | None
    | Lax
    | Strict

  let compare (t1 : t) (t2 : t) = compare t1 t2
  let equal (t1 : t) (t2 : t) = compare t1 t2 = 0

  let to_string = function
    | Default -> ""
    | None -> "None"
    | Lax -> "Lax"
    | Strict -> "Strict"
  ;;
end

exception Cookie of string

(** [to_rfc1123 t] converts [t] to a string in a format as defined by RFC 1123. *)
let date_to_string (tm : Unix.tm) =
  let weekday =
    match tm.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | 7 -> "Sun"
    | wd -> raise (Cookie (Format.sprintf "Invalid date time. weekday is %d" wd))
  in
  let month =
    match tm.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | m -> raise (Cookie (Format.sprintf "Invalid date time. month is %d" m))
  in
  Printf.sprintf
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekday
    tm.tm_mday
    month
    (1900 + tm.tm_year)
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
;;

type t =
  { name : string
  ; value : string
  ; path : string option
  ; domain : string option
  ; expires : Unix.tm option
  ; max_age : int option
  ; secure : bool option
  ; http_only : bool option
  ; same_site : Same_site.t option
  ; extension : string option
  }

let compare { name = name1; _ } { name = name2; _ } = String.compare name1 name2
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

let is_control_char c =
  let code = Char.code c in
  (code >= 0 && code <= 31) || code = 127
;;

let err fmt = Format.ksprintf (fun s -> raise (Cookie s)) fmt

(* Parses cookie attribute value. Cookie attribute values shouldn't contain any
   CTL(control characters) or ';' char. *)
let parse_cookie_av attr_value err =
  let rec validate i av =
    if i >= String.length av
    then av
    else (
      let c = av.[i] in
      if is_control_char c || Char.equal c ';' then err c else validate (i + 1) av)
  in
  match attr_value with
  | None -> None
  | Some value when String.length value = 0 -> None
  | Some value -> Some (validate 0 value)
;;

let parse_path path =
  err "Cookie 'Path' attribute value contains invalid character '%c'"
  |> parse_cookie_av path
;;

let parse_extension extension =
  err "Cookie extension value contains invalid character '%c'"
  |> parse_cookie_av extension
;;

(* Parses a given cookie into a cookie_name. Valid cookie name/token as defined
   in https://tools.ietf.org/html/rfc2616#section-2.2 *)
let parse_name name =
  let is_separator = function
    | '('
    | ')'
    | '<'
    | '>'
    | '@'
    | ','
    | ';'
    | ':'
    | '\\'
    | '"'
    | '/'
    | '['
    | ']'
    | '?'
    | '='
    | '{'
    | '}'
    | ' ' -> true
    | c when Char.code c = 9 -> true
    | _ -> false
  in
  let is_us_ascii_char c =
    let code = Char.code c in
    code >= 0 && code <= 127
  in
  let rec validate i name =
    if i >= String.length name
    then name
    else (
      let c = name.[i] in
      match c with
      | c when is_control_char c -> err "Control character '%c' found in name." c
      | c when is_separator c -> err "Separator character '%c' found in name." c
      | c when not (is_us_ascii_char c) ->
        err "Invalid US-ASCII character '%c' found in name." c
      | _ -> validate (i + 1) name)
  in
  let name = if String.length name > 0 then name else err "0 length cookie name." in
  validate 0 name
;;

(* Based on https://golang.org/src/net/http/cookie.go
 * https://tools.ietf.org/html/rfc6265#section-4.1.1
 * cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
 * cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
 *
 * US-ASCII characters excluding CTLs, whitespace DQUOTE, comma, semicolon,
 * and backslash
 *
 * We loosen this as spaces and commas are common in cookie values but we produce
 * a quoted cookie-value in when value starts or ends with a comma or space.
 * See https://golang.org/issue/7243 for the discussion.
 *)
let parse_value value =
  let dquote = Char.code '"' in
  let semi = Char.code ';' in
  let b_slash = Char.code '\\' in
  let rec validate i s =
    if i >= String.length s
    then s
    else (
      let c = s.[i] in
      let code = Char.code c in
      if 0x20 <= code && code < 0x7f && code <> dquote && code <> semi && code <> b_slash
      then validate (i + 1) s
      else err "Invalid char '%c' found in cookie value" c)
  in
  let strip_quotes s =
    let is_dquote = String.equal "\"" in
    let first_s = String.sub s 0 1 in
    let last_s = String.sub s (String.length s - 1) 1 in
    if is_dquote first_s && is_dquote last_s
    then String.sub s 1 (String.length s - 2)
    else s
  in
  let value =
    if String.length value > 0 then value else err "Cookie value length must be > 0."
  in
  strip_quotes value |> validate 0
;;

(** See https://tools.ietf.org/html/rfc1034#section-3.5 and
    https://tools.ietf.org/html/rfc1123#section-2 *)
let parse_domain_av domain_av =
  let rec validate last_char label_count (i, s) =
    if i >= String.length s
    then s, last_char, label_count
    else (
      let label_count = label_count + 1 in
      let c = s.[i] in
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> validate c label_count (i + 1, s)
      | '-' ->
        if Char.equal last_char '.'
        then err "Character before '-' cannot be '.'"
        else validate c label_count (i + 1, s)
      | '.' ->
        if Char.equal last_char '.' || Char.equal last_char '-'
        then err "Character before '.' cannot be '.' or '-'"
        else if label_count > 63 || label_count = 0
        then err "Domain name label can't exceed 63 characters or have 0 length"
        else validate c 0 (i + 1, s) (* reset label_count *)
      | _ -> err "Invalid character '%c'" c)
  in
  let validate_length av =
    if String.length av > 255
    then err "Domain attribute value length must not exceed 255 characters"
    else ()
  in
  let validate_last_char last_char =
    if Char.equal '-' last_char
    then err "Domain attribute value's last character is not allowed to be '-'"
    else ()
  in
  let validate_label_length label_count =
    if label_count > 63
    then err "Domain attribute value label length can't exceed 63 characters"
    else ()
  in
  match domain_av with
  | None -> None
  | Some domain_av when String.length domain_av = 0 -> None
  | Some domain_av ->
    let () = validate_length domain_av in
    let domain_av =
      if String.equal "." (String.sub domain_av 0 1)
      then
        (* A cookie domain attribute may start with a leading dot. *)
        String.sub domain_av 0 1
      else domain_av
    in
    let domain_av, last_char, label_count = validate '.' 0 (0, domain_av) in
    let domain_av = domain_av in
    let () = validate_last_char last_char in
    let () = validate_label_length label_count in
    Some domain_av
;;

let parse_max_age max_age =
  match max_age with
  | None -> None
  | Some ma ->
    if ma <= 0
    then err "Cookies 'Max-Age' attribute is less than or equal to 0"
    else Option.some ma
;;

let create
    ?path
    ?domain
    ?expires
    ?max_age
    ?secure
    ?http_only
    ?same_site
    ?extension
    name
    ~value
  =
  let name = parse_name name in
  let value = parse_value value in
  let domain = parse_domain_av domain in
  let path = parse_path path in
  let max_age = parse_max_age max_age in
  let extension = parse_extension extension in
  { name; value; path; domain; expires; max_age; secure; http_only; same_site; extension }
;;

let of_cookie_header header =
  String.split_on_char ';' header
  |> List.filter_map (fun s ->
         let s = String.trim s in
         if String.length s > 0 then Some s else None)
  |> List.filter_map (fun cookie ->
         try
           let cookie_items = String.split_on_char '=' cookie in
           let name = List.nth cookie_items 0
           and value = List.nth cookie_items 1 in
           Some (create name ~value)
         with
         | Failure _ | Invalid_argument _ -> None)
;;

let to_set_cookie_header_value t =
  let module O = Option in
  let buf = Buffer.create 50 in
  let add_str fmt = Format.ksprintf (Buffer.add_string buf) fmt in
  add_str "%s=%s" (name t) (value t);
  O.iter (fun path -> add_str "; Path=%s" path) (path t);
  O.iter (fun d -> add_str "; Domain=%s" d) (domain t);
  O.iter (fun expires -> add_str "; Expires=%s" @@ date_to_string expires) (expires t);
  O.iter (fun max_age -> if max_age > 0 then add_str "; Max-Age=%d" max_age) (max_age t);
  O.iter (fun secure -> if secure then add_str "; Secure") t.secure;
  O.iter (fun http_only -> if http_only then add_str "; HttpOnly") t.http_only;
  O.iter
    (fun same_site ->
      if Same_site.(equal Default same_site)
      then add_str "; SameSite"
      else add_str "; SameSite=%s" (Same_site.to_string same_site))
    t.same_site;
  O.iter (fun extension -> add_str "; %s" extension) (extension t);
  Buffer.contents buf
;;

let to_cookie_header_value t = Format.sprintf "%s=%s" (name t) (value t)
