(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)
open Reparse

exception Multipart of string

module Map = struct
  include Map.Make (String)

  let pp pp_value fmt t =
    let pp_kv = Fmt.pair ~sep:Fmt.comma Fmt.string pp_value in
    let s = to_seq t in
    Fmt.seq ~sep:Fmt.semi pp_kv fmt s
end

let rec list_equal f l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _
  | _, [] ->
    false
  | x1 :: l1', x2 :: l2' -> f x1 x2 && list_equal f l1' l2'

module Part = struct
  type t =
    { body : bytes
    ; name : string
    ; content_type : string
    ; filename : string option
    ; parameters : string Map.t
    }

  let pp fmt p =
    let fields =
      [ Fmt.field "name" (fun p -> p.name) Fmt.string
      ; Fmt.field "content_type" (fun p -> p.content_type) Fmt.string
      ; Fmt.field "filename" (fun p -> p.filename) Fmt.(option string)
      ; Fmt.field "parameters" (fun p -> p.parameters) (Map.pp Fmt.string)
      ; Fmt.field "body" (fun p -> Bytes.to_string p.body) Fmt.string
      ]
    in
    Fmt.record ~sep:Fmt.semi fields fmt p

  let equal p1 p2 =
    let body = Bytes.compare p1.body p2.body = 0 in
    let name = String.compare p1.name p2.name = 0 in
    let content_type = String.compare p1.content_type p2.content_type = 0 in
    let filename = Option.compare String.compare p1.filename p2.filename = 0 in
    let parameters =
      Map.compare String.compare p1.parameters p2.parameters = 0
    in
    body && name && content_type && filename && parameters
end

type t = Part.t list Map.t

let pp_parts = Fmt.list ~sep:Fmt.semi Part.pp

let equal_parts = list_equal Part.equal

let pp fmt (m : t) = Map.pp pp_parts fmt m

let equal (m1 : t) (m2 : t) = Map.equal equal_parts m1 m2

type part_header =
  | Content_type of
      { ty : string
      ; subtype : string
      ; parameters : string Map.t
      }
  | Content_disposition of string Map.t

let is_alpha_digit = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
    true
  | _ -> false

let is_space c = c == '\x20'

let is_control = function
  | '\x00' .. '\x1F'
  | '\x7F' ->
    true
  | _ -> false

let is_tspecial = function
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
  | '=' ->
    true
  | _ -> false

let is_ascii_char = function
  | '\x00' .. '\x7F' -> true
  | _ -> false

let is_ctext = function
  | '\x21' .. '\x27'
  | '\x2A' .. '\x5B'
  | '\x5D' .. '\x7E' ->
    true
  | _ -> false

let is_qtext = function
  | '\x21'
  | '\x23' .. '\x5B'
  | '\x5D' .. '\x7E' ->
    true
  | _ -> false

let is_token_char c =
  is_ascii_char c
  && (not (is_space c))
  && (not (is_control c))
  && not (is_tspecial c)

let implode l = List.to_seq l |> String.of_seq

let token =
  let+ chars = take ~at_least:1 (char_if is_token_char) <?> "[token]" in
  implode chars

(* https://tools.ietf.org/html/rfc5322#section-3.2.1 quoted-pair = ('\' (VCHAR /
   WSP)) / obs-qp *)
let quoted_pair = String.make 1 <$> char '\\' *> (whitespace <|> vchar)

(* Folding whitespace and comments -
   https://tools.ietf.org/html/rfc5322#section-3.2.2

   let r = parse " \r\n " fws r = " " *)
let fws =
  let* ws_count1 = skip whitespace in
  let+ lws_count = skip (string "\r\n" *> skip ~at_least:1 whitespace) in
  if ws_count1 + lws_count > 0 then
    " "
  else
    ""

(* let v = parse_string comment "( asdfasdfasdfasd(aaa) \\(cccc\\) (bbb(ddd)))"
   in v = " asdfasdfasdfasd;aaa (cccc) ;bbb;ddd";; *)
let comment =
  let ctext = char_if is_ctext >>| String.make 1 in
  recur (fun comments ->
      let ccontent =
        let+ s =
          take
            (map2
               ~f:(fun sp content -> sp ^ content)
               fws
               (any [ ctext; quoted_pair; comments >>| ( ^ ) ";" ]))
        in
        String.concat "" s
      in
      char '(' *> map2 ~f:(fun comment_txt sp -> comment_txt ^ sp) ccontent fws
      <* char ')')

(* let r = parse " ( asdfasdfasdfasd(aaa) \\(cccc\\) (bbb(ddd))) " cfws;; r = "
   " *)
let cfws =
  let one_or_more_comments =
    take ~at_least:1
      (map2 ~f:(fun sp comment_txt -> sp ^ comment_txt) fws comment)
    *> fws
  in
  one_or_more_comments <|> fws

(* Test cases : let r = parse "\" \r\n hello \r\n \"" quoted_string;; r = "
   hello ";;

   parse "(comment1)\"hello\"(comment2)" quoted_string;; r = "hello";; *)
let quoted_string =
  let qtext = String.make 1 <$> char_if is_qtext in
  let qcontent =
    (fun l -> String.concat "" l)
    <$> take
          (map2
             ~f:(fun sp qcontent' -> sp ^ qcontent')
             fws (qtext <|> quoted_pair))
  in
  cfws *> dquote *> map2 ~f:(fun qcontent' sp -> qcontent' ^ sp) qcontent fws
  <* dquote
  <* cfws

(* let r = parse "asdfasdf" p_param_value;; r = "asdfasdf";;

   parse "\"hello\"" p_param_value;; r = "hello" *)
let param_value = token <|> quoted_string

(* let r = parse "; field1=value1;" p_param;; r = ("field1", "value1");;

   let r = parse "; field1=\"value1\";" p_param;; r = ("field1", "value1");; *)
let param =
  let name = skip whitespace *> char ';' *> skip whitespace *> token in
  let value = char '=' *> param_value in
  map2 ~f:(fun name value -> (name, value)) name value

let p_restricted_name =
  let p_restricted_name_chars =
    char_if (function
      | '!'
      | '#'
      | '$'
      | '&'
      | '-'
      | '^'
      | '_'
      | '.'
      | '+' ->
        true
      | c when is_alpha_digit c -> true
      | _ -> false)
  in
  let* first_ch = char_if is_alpha_digit in
  let buf = Buffer.create 10 in
  Buffer.add_char buf first_ch;
  let+ restricted_name = take ~up_to:126 p_restricted_name_chars in
  Buffer.add_string buf (implode restricted_name);
  Buffer.contents buf

let content_disposition =
  let+ params =
    string "Content-Disposition:"
    *> skip whitespace
    *> string "form-data"
    *> take param
  in
  let params = List.to_seq params |> Map.of_seq in
  Content_disposition params

let content_type parse_header_name =
  let* ty =
    (if parse_header_name then
      string "Content-Type:" *> unit
    else
      unit)
    *> skip whitespace
    *> p_restricted_name
  in
  let* subtype = char '/' *> p_restricted_name in
  let+ params = take param in
  let parameters = params |> List.to_seq |> Map.of_seq in
  Content_type { ty; subtype; parameters }

let header_boundary =
  let is_bcharnospace = function
    | '\''
    | '('
    | ')'
    | '+'
    | '_'
    | ','
    | '-'
    | '.'
    | '/'
    | ':'
    | '='
    | '?' ->
      true
    | c when is_alpha_digit c -> true
    | _ -> false
  in
  let bchars =
    char_if (function
      | '\x20' -> true
      | c when is_bcharnospace c -> true
      | _ -> false)
  in
  let boundary =
    let* bchars = take ~up_to:70 bchars in
    let len = List.length bchars in
    if len > 0 then
      let last_char = List.nth bchars (len - 1) in
      if is_bcharnospace last_char then
        return (implode bchars)
      else
        fail "Invalid boundary value: invalid last char"
    else
      fail "Invalid boundary value: 0 length"
  in
  optional dquote *> boundary <* optional dquote <|> token

let multipart_formdata_header =
  let param =
    let* attribute = skip whitespace *> char ';' *> skip whitespace *> token in
    let+ value =
      char '='
      *>
      if attribute = "boundary" then
        header_boundary
      else
        param_value
    in
    (attribute, value)
  in
  let+ params =
    (optional crlf
     *> optional (string "Content-Type:")
     *> skip whitespace
     *> string "multipart/form-data"
    <?> "Not multipart formdata header")
    *> skip whitespace
    *> take param
  in
  params |> List.to_seq |> Map.of_seq

let body_part headers body =
  let name, content_type, filename, parameters =
    List.fold_left
      (fun (name, ct, filename, params) header ->
        match header with
        | Content_type ct ->
          let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
          ( name
          , content_type
          , filename
          , Map.union (fun _key a _b -> Some a) params ct.parameters )
        | Content_disposition params2 ->
          let name = Map.find_opt "name" params2 in
          let filename = Map.find_opt "filename" params2 in
          ( name
          , ct
          , filename
          , Map.union (fun _key a _b -> Some a) params params2 ))
      (None, None, None, Map.empty)
      headers
  in
  match name with
  | None -> fail "parameter 'name' not found"
  | Some name ->
    let content_type =
      try Option.get content_type with
      | _ -> "text/plain"
    in
    let parameters = Map.remove "name" parameters in
    let parameters =
      match filename with
      | Some _ -> Map.remove "filename" parameters
      | None -> parameters
    in
    return
      ( name
      , { Part.body = Bytes.unsafe_of_string body
        ; name
        ; content_type
        ; filename
        ; parameters
        } )

let add_part (name, bp) m =
  match Map.find_opt name m with
  | Some l -> Map.add name (bp :: l) m
  | None -> Map.add name [ bp ] m

let multipart_bodyparts boundary_value =
  let dash_boundary = "--" ^ boundary_value in
  let end_boundary = dash_boundary ^ "--" in
  let line = line `CRLF in
  let rec loop_body buf =
    let* ln = line in
    if ln = dash_boundary then
      return (Buffer.contents buf, true)
    else if ln = end_boundary then
      return (Buffer.contents buf, false)
    else (
      Buffer.add_string buf ln;
      Buffer.add_string buf "\r\n";
      loop_body buf
    )
  in
  let rec loop_parts parts =
    let* part_headers =
      take ~at_least:1 ~sep_by:crlf
        (any [ content_disposition; content_type true ])
    in
    let* body, continue = loop_body (Buffer.create 0) in
    let* bp = body_part part_headers body in
    if continue then
      loop_parts (bp :: parts)
    else
      return (bp :: parts)
  in
  let+ parts =
    all_unit
      [ ignore_m @@ crlf; ignore_m @@ string dash_boundary; ignore_m @@ crlf ]
    *> loop_parts []
  in
  List.fold_left (fun m (name, bp) -> add_part (name, bp) m) Map.empty parts

let parse ~content_type_header ~body =
  let header_params =
    parse_string multipart_formdata_header content_type_header
  in
  match Map.find "boundary" header_params with
  | boundary_value -> parse_string (multipart_bodyparts boundary_value) body
  | exception Not_found -> raise @@ Multipart "Boundary paramater not found"
