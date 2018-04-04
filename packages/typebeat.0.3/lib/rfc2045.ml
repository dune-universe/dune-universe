type discrete =
  [ `Text
  | `Image
  | `Audio
  | `Video
  | `Application ]
type composite =
  [ `Message
  | `Multipart ]
type extension =
  [ `Ietf_token of string
  | `X_token    of string ]

type ty = [ discrete | composite | extension ]

type subty =
  [ `Ietf_token of string
  | `Iana_token of string
  | `X_token    of string ]

type mechanism =
  [ `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Base64
  | `Ietf_token of string
  | `X_token    of string ]

type value = [ `String of string | `Token of string ]

type content =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list }

type version = (int * int)

open Angstrom

let sp = Format.sprintf

let is_tspecials = function
  | '(' | ')' | '<' | '>'  | '@'
  | ',' | ';' | ':' | '\\' | '"'
  | '/' | '[' | ']' | '?'  | '=' -> true
  | _ -> false

let is_ctl = function
  | '\000' .. '\031' -> true
  | _ -> false

let is_space = (=) ' '

let is_token c =
  not (is_tspecials c) && not (is_ctl c) && not (is_space c)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let token = take_while1 is_token

let attribute = token >>| String.lowercase_ascii

let ietf_token = token

let iana_token = token

let of_string s p =
  match parse_string p s with
  | Ok x -> Some x
  | Error _ -> None

let x_token =
  satisfy (function 'x' | 'X' -> true | _ -> false)
  *> char '-'
  *> token

let extension_token =
  peek_char_fail <?> "extension_token" >>= function
  | 'X' | 'x' -> x_token >>| fun v -> `X_token v
  | _ -> ietf_token >>| fun v -> `Ietf_token v

let composite_ty =
  token >>= fun s -> match String.lowercase_ascii s with
  | "message"   -> return `Message
  | "multipart" -> return `Multipart
  | _ -> match of_string s extension_token with
         | Some v -> return v
         | None -> fail (sp "Invalid token %s" s)

let ty =
  token >>= fun s -> match String.lowercase_ascii s with
  | "text"        -> return `Text
  | "image"       -> return `Image
  | "audio"       -> return `Audio
  | "video"       -> return `Video
  | "application" -> return `Application
  | "message"     -> return `Message
  | "multipart"   -> return `Multipart
  | _ -> match of_string s extension_token with
         | Some v -> return v
         | None -> fail (sp "Invalid token %s" s)

let ty_to_string = function
  | `Text -> "text"
  | `Image -> "image"
  | `Audio -> "audio"
  | `Video -> "video"
  | `Application -> "application"
  | `Message -> "message"
  | `Multipart -> "multipart"
  | `Ietf_token s | `X_token s -> s

let subty ty =
  (peek_char_fail <?> "subty" >>= function
   | 'X' | 'x' -> extension_token
   | _ -> token >>| fun v ->
     try `Iana_token (Iana.Set.find (String.lowercase_ascii v) (Iana.Map.find (ty_to_string ty) Iana.iana))
     with _ -> `X_token v)
  >>| fun subty -> (ty, subty)

let value =
  (Rfc822.quoted_string >>| fun v -> `String v)
  <|> (token >>| fun v -> `Token v)

let parameter =
  attribute
  >>= fun attribute -> char '='
  *> value
  >>| fun value -> (attribute, value)

let content =
  option () Rfc822.cfws
  *> ty
  <* option () Rfc822.cfws
  <* char '/'
  <* option () Rfc822.cfws
  >>= subty
  <* option () Rfc822.cfws
  >>= fun (ty, subty) ->
    many (char ';' *> option () Rfc822.cfws *> parameter)
  >>| fun parameters -> { ty; subty; parameters; }

let version =
  option () Rfc822.cfws
  *> take_while1 is_digit
  >>| int_of_string
  <* option () Rfc822.cfws
  <* char '.'
  <* option () Rfc822.cfws
  >>= fun a -> take_while1 is_digit
  >>| int_of_string
  <* option () Rfc822.cfws
  >>| fun b -> (a, b)

let mechanism =
  token >>= fun s -> match String.lowercase_ascii s with
  | "7bit" -> return `Bit7
  | "8bit" -> return `Bit8
  | "binary" -> return `Binary
  | "quoted-printable" -> return `QuotedPrintable
  | "base64" -> return `Base64
  | _ -> match of_string s extension_token with
         | Some v -> return v
         | None -> fail (sp "Invalid token %s" s)

let encoding =
  option () Rfc822.cfws
  *> mechanism
  <* option () Rfc822.cfws

let id =
  option () Rfc822.cfws
  *> Rfc822.msg_id
  <* option () Rfc822.cfws
