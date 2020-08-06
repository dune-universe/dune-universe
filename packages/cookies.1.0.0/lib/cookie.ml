(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * cookies v1.0.0
 *-------------------------------------------------------------------------*)

open Sexplib0.Sexp_conv

module R = struct
  include Result

  module O = struct
    let ( >>= ) = bind

    let ( >>| ) r f = map f r

    let ( let* ) = ( >>= )

    let ( let+ ) = ( >>| )
  end
end

module Ptime = struct
  include Ptime

  (** [to_rfc1123 t] converts [t] to a string in a format as defined by RFC
      1123. *)
  let to_rfc1123 t =
    let weekday =
      match Ptime.weekday ~tz_offset_s:0 t with
      | `Mon -> "Mon"
      | `Tue -> "Tue"
      | `Wed -> "Wed"
      | `Thu -> "Thu"
      | `Fri -> "Fri"
      | `Sat -> "Sat"
      | `Sun -> "Sun"
    in
    let month = function
      | 1 -> "Jan"
      | 2 -> "Feb"
      | 3 -> "Mar"
      | 4 -> "Apr"
      | 5 -> "May"
      | 6 -> "Jun"
      | 7 -> "Jul"
      | 8 -> "Aug"
      | 9 -> "Sep"
      | 10 -> "Oct"
      | 11 -> "Nov"
      | 12 -> "Dec"
      | _ -> assert false
    in
    let (y, m, d), ((hh, min, sec), _tz) =
      Ptime.to_date_time ~tz_offset_s:0 t
    in
    Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday d (month m) y
      hh min sec

  let sexp_of_t t =
    Ptime.to_rfc3339 ~tz_offset_s:0 t |> Sexplib0.Sexp_conv.sexp_of_string
end

module String = struct
  include StringLabels

  let replace_all ~pattern ~with_ s =
    let b = Bytes.of_string s in
    for i = 0 to Bytes.length b - 1 do
      let c = Bytes.get b i in
      if Char.equal c pattern then Bytes.set b i with_ else ()
    done;
    Bytes.to_string b
end

module Option = struct
  include Option

  module O = struct
    let ( let* ) = bind
  end

  let some_if cond a = if cond then Some a else None
end

open R.O

let[@inline] ( >> ) f g x = g (f x)

let sprintf = Printf.sprintf

type cookie_name = Cookie_name of string [@unboxed]
[@@deriving sexp_of, variants]

type cookie_value = Cookie_value of string [@unboxed]
[@@deriving sexp_of, variants]

type domain_value = Domain_value of string [@unboxed]
[@@deriving sexp_of, variants]

type cookie_path = Cookie_path of string [@unboxed]
[@@deriving sexp_of, variants]

type cookie_max_age = Cookie_max_age of int [@unboxed]
[@@deriving sexp_of, variants]

type cookie_extension = Cookie_extension of string [@unboxed]
[@@deriving sexp_of, variants]

type cookie_expires = Cookie_expires of Ptime.t [@unboxed]
[@@deriving sexp_of, variants]

type error =
  [ `Cookie_name_error of string
  | `Cookie_value_error of string
  | `Cookie_domain_av_error of string
  | `Cookie_path_error of string
  | `Cookie_max_age_error of string
  | `Cookie_extension_error of string ]
[@@deriving sexp_of, variants]

type t = {
  name : cookie_name;
  value : cookie_value;
  path : cookie_path option; [@sexp.option]
  domain : domain_value option; [@sexp.option]
  expires : cookie_expires option; [@sexp.option]
  max_age : cookie_max_age option; [@sexp.option]
  secure : bool option; [@sexp.option]
  http_only : bool option; [@sexp.option]
  same_site : Same_site.t option; [@sexp.option]
  extension : cookie_extension option; [@sexp.option]
  raw : string option; [@sexp.option] (* Raw cookie string *)
}
[@@deriving sexp_of, fields]

(* -------------------------------------------------------------------------
 * Cookie query functions
 * -------------------------------------------------------------------------*)

let name { name = Cookie_name nm; _ } = nm

let value { value = Cookie_value v; _ } = v

let path { path; _ } = Option.map (fun (Cookie_path cp) -> cp) path

let domain { domain; _ } = Option.map (fun (Domain_value v) -> v) domain

let expires { expires; _ } =
  Option.map (fun (Cookie_expires expires) -> expires) expires

let max_age { max_age; _ } =
  Option.map (fun (Cookie_max_age max_age) -> max_age) max_age

let extension { extension; _ } =
  Option.map (fun (Cookie_extension ext) -> ext) extension

(*--------------------------------------------------------------------------
 * Cookie Parsing.
 * -------------------------------------------------------------------------*)

let is_control_char c =
  let code = Char.code c in
  (code >= 0 && code <= 31) || code = 127

let err f fmt = Format.ksprintf (f >> R.error) fmt

(* Parses cookie attribute value. Cookie attribute values shouldn't contain any
   CTL(control characters) or ';' char. *)
let parse_cookie_av attr_value make err =
  let rec validate i av =
    if i >= String.length av then R.ok av
    else
      let c = av.[i] in
      if is_control_char c || Char.equal c ';' then err c
      else validate (i + 1) av
  in
  match attr_value with
  | None -> R.ok None
  | Some value when String.length value = 0 -> R.ok None
  | Some value ->
      let+ value = validate 0 value in
      Some (make value)

let parse_path path =
  err cookie_path_error
    "Cookie 'Path' attribute value contains invalid character '%c'"
  |> parse_cookie_av path cookie_path

let parse_extension extension =
  err cookie_extension_error
    "Cookie extension value contains invalid character '%c'"
  |> parse_cookie_av extension cookie_extension

(* Parses a given cookie into a cookie_name. Valid cookie name/token as defined
   in https://tools.ietf.org/html/rfc2616#section-2.2 *)
let parse_name name =
  let is_separator = function
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
    | ']' | '?' | '=' | '{' | '}' | ' ' ->
        true
    | c when Char.code c = 9 -> true
    | _ -> false
  in
  let is_us_ascii_char c =
    let code = Char.code c in
    code >= 0 && code <= 127
  in
  let err fmt = err cookie_name_error fmt in
  let rec validate i name =
    if i >= String.length name then R.ok name
    else
      let c = name.[i] in
      match c with
      | c when is_control_char c ->
          err "Control character '%c' found in name." c
      | c when is_separator c -> err "Separator character '%c' found in name." c
      | c when not (is_us_ascii_char c) ->
          err "Invalid US-ASCII character '%c' found in name." c
      | _ -> validate (i + 1) name
  in
  let* name =
    if String.length name > 0 then R.ok name else err "0 length cookie name."
  in
  let+ name = validate 0 name in
  Cookie_name name

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
  let err fmt = err cookie_value_error fmt in
  let rec validate i s =
    if i >= String.length s then R.ok s
    else
      let c = s.[i] in
      let code = Char.code c in
      if
        0x20 <= code
        && code < 0x7f
        && code <> dquote
        && code <> semi
        && code <> b_slash
      then validate (i + 1) s
      else err "Invalid char '%c' found in cookie value" c
  in
  let strip_quotes s =
    let is_dquote = String.equal "\"" in
    let first_s = String.sub s ~pos:0 ~len:1 in
    let last_s = String.sub s ~pos:(String.length s - 1) ~len:1 in
    if is_dquote first_s && is_dquote last_s then
      String.sub s ~pos:1 ~len:(String.length s - 2)
    else s
  in
  let* value =
    if String.length value > 0 then R.ok value
    else err "Cookie value length must be > 0."
  in
  let+ value = strip_quotes value |> validate 0 in
  Cookie_value value

(** See https://tools.ietf.org/html/rfc1034#section-3.5 and
    https://tools.ietf.org/html/rfc1123#section-2 *)
let parse_domain_av domain_av =
  let return_err err_msg last_char label_count =
    (R.error (`Cookie_domain_av_error err_msg), last_char, label_count)
  in
  let rec validate last_char label_count (i, s) =
    if i >= String.length s then (R.ok s, last_char, label_count)
    else
      let label_count = label_count + 1 in
      let c = s.[i] in
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> validate c label_count (i + 1, s)
      | '-' ->
          if Char.equal last_char '.' then
            return_err "Character before '-' cannot be '.'" c label_count
          else validate c label_count (i + 1, s)
      | '.' ->
          if Char.equal last_char '.' || Char.equal last_char '-' then
            return_err "Character before '.' cannot be '.' or '-'" c label_count
          else if label_count > 63 || label_count = 0 then
            return_err
              "Domain name label can't exceed 63 characters or have 0 length"
              last_char label_count
          else validate c 0 (i + 1, s) (* reset label_count *)
      | _ ->
          return_err (sprintf "Invalid character '%c'" c) last_char label_count
  in
  let err = err cookie_domain_av_error in
  let validate_length av =
    if String.length av = 0 then
      err "Domain attribute value length must be greater than 0"
    else if String.length av > 255 then
      err "Domain attribute value length must not exceed 255 characters"
    else R.ok ()
  in
  let validate_last_char last_char =
    if Char.equal '-' last_char then
      err "Domain attribute value's last character is not allowed to be '-'"
    else R.ok ()
  in
  let validate_label_length label_count =
    if label_count > 63 then
      err "Domain attribute value label length can't exceed 63 characters"
    else R.ok ()
  in
  match domain_av with
  | None -> R.ok None
  | Some domain_av ->
      let* () = validate_length domain_av in
      let domain_av =
        if String.equal "." (String.sub domain_av ~pos:0 ~len:1) then
          (* A cookie domain attribute may start with a leading dot. *)
          String.sub domain_av ~pos:0 ~len:1
        else domain_av
      in
      let domain_av, last_char, label_count = validate '.' 0 (0, domain_av) in
      let* domain_av = domain_av in
      let* () = validate_last_char last_char in
      let+ () = validate_label_length label_count in
      Some (Domain_value domain_av)

let parse_max_age max_age =
  match max_age with
  | None -> R.ok None
  | Some ma ->
      if ma <= 0 then
        err cookie_max_age_error
          "Cookies 'Max-Age' attribute is less than or equal to 0"
      else (cookie_max_age >> Option.some >> R.ok) ma

(* -------------------------------------------------------------------------
 * Sanitize functions
 * -------------------------------------------------------------------------*)

(** Sanitizes cookie_value by double quoting it if it starts or ends in '
    '(space) or ','(comma) character. *)
let sanitize_cookie_value (Cookie_value v as cv) =
  let is_space_comma c = String.equal c " " || String.equal c "," in
  let start_char = String.sub v ~pos:0 ~len:1 in
  let end_char = String.sub v ~pos:(String.length v - 1) ~len:1 in
  if is_space_comma start_char || is_space_comma end_char then
    cookie_value @@ "\"" ^ v ^ "\""
  else cv

(** Sanitizes cookie name by replacing \n or \r with '-' character. *)
let sanitive_cookie_name (Cookie_name name) =
  String.replace_all ~pattern:'\n' ~with_:'-' name
  |> String.replace_all ~pattern:'\r' ~with_:'-'
  |> cookie_name

let create
    ~name
    ~value
    ?(sanitize_name = true)
    ?(sanitize_value = true)
    ?path
    ?domain
    ?expires
    ?max_age
    ?secure
    ?http_only
    ?same_site
    ?extension
    () =
  let* name = parse_name name in
  let name = if sanitize_name then sanitive_cookie_name name else name in
  let* value = parse_value value in
  let value = if sanitize_value then sanitize_cookie_value value else value in
  let* domain = parse_domain_av domain in
  let* path = parse_path path in
  let* max_age = parse_max_age max_age in
  let* extension = parse_extension extension in
  let expires = Option.map (fun expires -> Cookie_expires expires) expires in
  R.ok
    {
      name;
      value;
      path;
      domain;
      expires;
      max_age;
      secure;
      http_only;
      same_site;
      extension;
      raw = None;
    }

let of_cookie_header header =
  String.split_on_char ~sep:';' header
  |> List.filter_map (fun s ->
         String.trim s |> Option.some_if (String.length s > 0))
  |> List.filter_map (fun cookie ->
         let cookie_items = String.split_on_char ~sep:'=' cookie in
         let open Option.O in
         let* name = List.nth_opt cookie_items 0 in
         let* value = List.nth_opt cookie_items 1 in
         Some (cookie, name, value))
  |> List.map (fun (raw, name, value) ->
         let* cookie =
           create ~name ~value ~sanitize_name:false ~sanitize_value:false ()
         in
         R.ok { cookie with raw = Some raw })

(*---------------------------------------------------------------------------
 * Cookie.t string conversion functions
 *--------------------------------------------------------------------------*)

let to_set_cookie_header_value t =
  let module O = Option in
  let buf = Buffer.create 50 in
  let add_str fmt = Format.ksprintf (Buffer.add_string buf) fmt in
  add_str "%s=%s" (name t) (value t);
  O.iter (fun path -> add_str "; Path=%s" path) (path t);
  O.iter (fun d -> add_str "; Domain=%s" d) (domain t);
  O.iter
    (fun expires -> add_str "; Expires=%s" @@ Ptime.to_rfc1123 expires)
    (expires t);
  O.iter
    (fun max_age -> if max_age > 0 then add_str "; Max-Age=%d" max_age)
    (max_age t);
  O.iter (fun secure -> if secure then add_str "; Secure") t.secure;
  O.iter (fun http_only -> if http_only then add_str "; HttpOnly") t.http_only;
  O.iter
    (fun same_site ->
      if Same_site.(equal default same_site) then add_str "; SameSite"
      else add_str "; SameSite=%s" (Same_site.to_cookie_string same_site))
    t.same_site;
  O.iter (fun extension -> add_str "; %s" extension) (extension t);
  Buffer.contents buf

let to_cookie_header_value t = sprintf "%s=%s" (name t) (value t)
