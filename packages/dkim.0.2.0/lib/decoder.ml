open Angstrom

let failf fmt = Fmt.kstrf fail fmt

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_plus = ( = ) '+'

let is_slash = ( = ) '/'

let is_dash = ( = ) '-'

let is_equal = ( = ) '='

let ( or ) f g x = f x || g x

let is_base64 = is_digit or is_alpha or is_plus or is_slash or is_equal

(* XXX(dinosaure): [is_equal] is necessary to take padding but a
    post-processing with [Base64] will check if we have a valid Base64 input. *)

(* XXX(dinosaure): [field-name] from [mrmime]. See RFC 6376:

    The following tokens are imported from [RFC5322]:
    o  "field-name" (name of a header field)
    o  "dot-atom-text" (in the local-part of an email address)
*)

let is_ftext = function
  | '\033' .. '\057' | '\060' .. '\126' -> true
  (* XXX(dinosaure): [is_ftext] should accept ';' but it not the case about
      DKIM-Signature (which use this character as seperator). *)
  | _ -> false

let field_name = take_while1 is_ftext >>| Mrmime.Field_name.v

(* XXX(dinosaure): [local-part] and [sub-domain] from [colombe]. See RFC 6376:

    The following tokens are imported from [RFC5321]:
    o  "local-part" (implementation warning: this permits quoted strings)
    o  "sub-domain"
*)

let let_dig = satisfy (is_alpha or is_digit)

let ldh_str =
  take_while1 (is_alpha or is_digit or is_dash) >>= fun res ->
  if res.[String.length res - 1] <> '-'
  then return res
  else fail "Invalid ldh-str token"

let sub_domain =
  let_dig >>= fun pre ->
  option "" ldh_str >>| fun lst -> String.concat "" [ String.make 1 pre; lst ]

let domain_name =
  sub_domain >>= fun x ->
  many (char '.' *> sub_domain) >>| fun r -> x :: r

let is_atext = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?' | '^'
  | '_' | '`' | '{' | '}' | '|' | '~' ->
      true
  | _ -> false

let is_qtextSMTP = function
  | '\032' | '\033' | '\035' .. '\091' | '\093' .. '\126' -> true
  | _ -> false

let atom = take_while1 is_atext

let dot_string =
  atom >>= fun x ->
  many (char '.' *> atom) >>| fun r -> `Dot_string (x :: r)

let quoted_pairSMTP =
  char '\\' *> satisfy (function '\032' .. '\126' -> true | _ -> false)
  >>| String.make 1

let qcontentSMTP = quoted_pairSMTP <|> take_while1 is_qtextSMTP

let quoted_string =
  char '"' *> many qcontentSMTP <* char '"' >>| String.concat "" >>| fun x ->
  `String x

let local_part = dot_string <|> quoted_string

(* See RFC 6376:

    hyphenated-word =  ALPHA [ *(ALPHA / DIGIT / "-") (ALPHA / DIGIT) ]
*)
let hyphenated_word =
  peek_char >>= function
  | None -> failf "Unexpected end of input"
  | Some chr ->
  match chr with
  | ('a' .. 'z' | 'A' .. 'Z') as chr ->
      take_while (is_alpha or is_digit or is_dash) >>= fun rest ->
      if String.length rest > 0
      then
        if rest.[String.length rest - 1] <> '-'
        then return (String.make 1 chr ^ rest)
        else
          failf "Unexpected character %02x"
            (Char.code rest.[String.length rest - 1])
      else return (String.make 1 chr)
  | chr -> failf "Unexpected character %02x" (Char.code chr)

(* See RFC 6376:

    hdr-name        =  field-name
*)
let hdr_name = field_name

let rsa = string "rsa" *> return Value.RSA

let sha1 = string "sha1" *> return Value.SHA1

let sha256 = string "sha256" *> return Value.SHA256

let simple = string "simple" *> return Value.Simple

let relaxed = string "relaxed" *> return Value.Relaxed

let algorithm_extension : Value.algorithm t =
  take_while1 (is_digit or is_alpha) >>= fun k ->
  if not (is_digit k.[0])
  then return (Value.Algorithm_ext k)
  else failf "Invalid algorithm key: %s" k

let hash_extension : Value.hash t =
  take_while1 (is_digit or is_alpha) >>= fun h ->
  if not (is_digit h.[0])
  then return (Value.Hash_ext h)
  else failf "Invalid hash: %s" h

(* See RFC 6376

    dkim-quoted-printable =  *(FWS / hex-octet / dkim-safe-char)
                              ; hex-octet is from RFC2045
    dkim-safe-char        =  %x21-3A / %x3C / %x3E-7E
                              ; '!' - ':', '<', '>' - '~'
*)
let dkim_quoted_printable =
  let is_hex = function '0' .. '9' | 'A' .. 'F' -> true | _ -> false in
  take_while (function
    | '\x21' .. '\x3a' | '\x3c' | '\x3e' .. '\x7e' -> true
    | chr -> is_hex chr)

let qp_hdr_value = dkim_quoted_printable

let qp_section = dkim_quoted_printable

(* XXX(dinosaure): RFC 6376 said: a single line of quoted-printable-encoded
    text. But you know, I'm lazy with this kind of stuff. And it used to have
    notes. Common! TODO(dinosaure): use [pecu]? *)

let selector =
  sub_domain >>= fun x ->
  many (char '.' *> sub_domain) >>| fun r -> x :: r

(* See RFC 6376

    tag-list  =  tag-spec *( ";" tag-spec ) [ ";" ]
    tag-spec  =  [FWS] tag-name [FWS] "=" [FWS] tag-value [FWS]
    tag-name  =  ALPHA *ALNUMPUNC
    tag-value =  [ tval *( 1*(WSP / FWS) tval ) ]
                    ; Prohibits WSP and FWS at beginning and end
    tval      =  1*VALCHAR
    VALCHAR   =  %x21-3A / %x3C-7E
                    ; EXCLAMATION to TILDE except SEMICOLON
    ALNUMPUNC =  ALPHA / DIGIT / "_"
*)
let is_valchar = function
  | '\x21' .. '\x3a' | '\x3c' .. '\x7e' -> true
  | _ -> false

let is_alnumpunc = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tag_name =
  peek_char >>= function
  | None -> failf "Unexpected end of input"
  | Some chr ->
  match chr with
  | ('a' .. 'z' | 'A' .. 'Z') as chr ->
      take_while is_alnumpunc >>= fun rest -> return (String.make 1 chr ^ rest)
  | chr -> failf "Unexpected character: %02x" (Char.code chr)

let tag_value = take_while1 is_valchar

let tag_spec :
    type v. tag_name:v Map.key t -> tag_value:v t -> (v Map.key * v option) t =
 fun ~tag_name ~tag_value ->
  tag_name <* char '=' >>= fun name ->
  option None (tag_value >>| Option.some) >>= fun value -> return (name, value)

let binding = function k, Some v -> Some (Map.B (k, v)) | _ -> None

(* sig-v-tag       = %x76 [FWS] "=" [FWS] 1*DIGIT *)
let v =
  let tag_name = string "v" >>| fun _ -> Map.K.v in
  let tag_value = take_while1 is_digit >>| int_of_string in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-a-tag       = %x61 [FWS] "=" [FWS] sig-a-tag-alg
    sig-a-tag-alg   = sig-a-tag-k "-" sig-a-tag-h
    sig-a-tag-k     = "rsa" / x-sig-a-tag-k
    sig-a-tag-h     = "sha1" / "sha256" / x-sig-a-tag-h
    x-sig-a-tag-k   = ALPHA *(ALPHA / DIGIT)
                    ; for later extension
    x-sig-a-tag-h   = ALPHA *(ALPHA / DIGIT)
                    ; for later extension *)
let a =
  let tag_name = string "a" >>| fun _ -> Map.K.a in
  let tag_value =
    rsa <|> algorithm_extension <* char '-' >>= fun k ->
    sha1 <|> sha256 <|> hash_extension >>| fun h -> (k, h) in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-b-tag       = %x62 [FWS] "=" [FWS] sig-b-tag-data
    sig-b-tag-data  = base64string *)
let b =
  (* XXX(dinosaure): base64string = ALPHADIGITPS *([FWS] ALPHADIGITPS) [ [FWS]
      "=" [ [FWS] "=" ] ]. Definition of the hell, a pre-processing is needed in
      this case to concat fragments separated by [FWS]. *)
  let tag_name = string "b" >>| fun _ -> Map.K.b in
  let tag_value = take_while1 is_base64 >>= fun str -> return str in
  tag_spec ~tag_name ~tag_value >>| fun v -> binding v

(* sig-bh-tag      = %x62 %x68 [FWS] "=" [FWS] sig-bh-tag-data
    sig-bh-tag-data = base64string *)
let bh =
  let tag_name = string "bh" >>| fun _ -> Map.K.bh in
  let tag_value = take_while1 is_base64 in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-c-tag       = %x63 [FWS] "=" [FWS] sig-c-tag-alg
                ["/" sig-c-tag-alg]
    sig-c-tag-alg   = "simple" / "relaxed" / x-sig-c-tag-alg
    x-sig-c-tag-alg = hyphenated-word    ; for later extension *)
let c =
  let tag_name = string "c" >>| fun _ -> Map.K.c in
  let tag_value =
    let sig_c_tag_alg =
      simple
      <|> relaxed
      <|> (hyphenated_word >>| fun x -> Value.Canonicalization_ext x) in
    sig_c_tag_alg >>= fun h ->
    option Value.Simple (char '/' *> sig_c_tag_alg) >>= fun b -> return (h, b)
  in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-d-tag       = %x64 [FWS] "=" [FWS] domain-name
    domain-name     = sub-domain 1*("." sub-domain)
                ; from [RFC5321] Domain,
                ; excluding address-literal *)
let d =
  let tag_name = string "d" >>| fun _ -> Map.K.d in
  let tag_value = domain_name in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-h-tag       = %x68 [FWS] "=" [FWS] hdr-name
                  *( [FWS] ":" [FWS] hdr-name ) *)
let h =
  let tag_name = string "h" >>| fun _ -> Map.K.h in
  let tag_value =
    hdr_name >>= fun x ->
    many (char ':' *> hdr_name) >>= fun r -> return (x :: r) in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-i-tag       = %x69 [FWS] "=" [FWS] [ Local-part ]
                          "@" domain-name *)
let i =
  let tag_name = string "i" >>| fun _ -> Map.K.i in
  let tag_value =
    option None (local_part >>| Option.some) >>= fun local ->
    char '*' *> domain_name >>= fun domain -> return { Value.local; domain }
  in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-l-tag    = %x6c [FWS] "=" [FWS]
              1*76DIGIT *)
let l =
  let tag_name = string "l" >>| fun _ -> Map.K.l in
  let tag_value = take_while1 is_digit >>| int_of_string in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-q-tag        = %x71 [FWS] "=" [FWS] sig-q-tag-method
                    *([FWS] ":" [FWS] sig-q-tag-method)
    sig-q-tag-method = "dns/txt" / x-sig-q-tag-type
                        ["/" x-sig-q-tag-args]
    x-sig-q-tag-type = hyphenated-word  ; for future extension
    x-sig-q-tag-args = qp-hdr-value *)
let q =
  let tag_name = string "q" >>| fun _ -> Map.K.q in
  let tag_value =
    let sig_q_tag_method =
      string "dns/txt"
      >>| (fun _ -> `DNS `TXT)
      <|> (hyphenated_word >>| fun x -> `Query_ext x)
      >>= fun meth ->
      option None (char '/' *> qp_hdr_value >>| Option.some) >>= fun args ->
      return (meth, args) in
    sig_q_tag_method >>= fun x ->
    many (char ':' *> sig_q_tag_method) >>= fun r -> return (x :: r) in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-s-tag    = %x73 [FWS] "=" [FWS] selector *)
let s =
  let tag_name = string "s" >>| fun _ -> Map.K.s in
  let tag_value = selector in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-t-tag    = %x74 [FWS] "=" [FWS] 1*12DIGIT *)
let t =
  let tag_name = string "t" >>| fun _ -> Map.K.t in
  let tag_value = take_while1 is_digit >>| Int64.of_string in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-x-tag    = %x78 [FWS] "=" [FWS]
                            1*12DIGIT *)
let x =
  let tag_name = string "x" >>| fun _ -> Map.K.x in
  let tag_value = take_while1 is_digit >>| Int64.of_string in
  tag_spec ~tag_name ~tag_value >>| binding

(* sig-z-tag      = %x7A [FWS] "=" [FWS] sig-z-tag-copy
                *( "|" [FWS] sig-z-tag-copy )
    sig-z-tag-copy = hdr-name [FWS] ":" qp-hdr-value *)
let z =
  let tag_name = string "z" >>| fun _ -> Map.K.z in
  let tag_value =
    let sig_z_tag_copy =
      hdr_name >>= fun field ->
      char ':' *> qp_hdr_value >>= fun v -> return (field, v) in
    sig_z_tag_copy >>= fun x ->
    many (char '|' *> sig_z_tag_copy) >>= fun r -> return (x :: r) in
  tag_spec ~tag_name ~tag_value >>| binding

let mail_tag_list =
  let tag_spec =
    bh
    <|> v
    <|> a
    <|> b
    <|> c
    <|> d
    <|> h
    <|> i
    <|> l
    <|> q
    <|> s
    <|> t
    <|> x
    <|> z in
  tag_spec >>= function
  | Some (Map.B (k, v)) ->
      many (char ';' *> tag_spec)
      <* option () (char ';' *> return ())
      >>| List.fold_left
            (fun hmap -> function
              | Some (Map.B (k, v)) -> Map.add k v hmap
              | None -> hmap)
            (Map.singleton k v)
  | None -> failf "Expect at least one tag"

(* Server part *)

let v =
  let tag_name = string "v" >>| fun _ -> Map.K.sv in
  let tag_value = string "DKIM1" in
  tag_spec ~tag_name ~tag_value >>| binding

let h =
  let tag_name = string "h" >>| fun _ -> Map.K.sh in
  let tag_value =
    let key_h_tag_alg = sha1 <|> sha256 <|> hash_extension in
    key_h_tag_alg >>= fun x ->
    many (char ':' *> key_h_tag_alg) >>= fun r -> return (x :: r) in
  tag_spec ~tag_name ~tag_value >>| binding

let k =
  let tag_name = string "k" >>| fun _ -> Map.K.k in
  let tag_value = rsa <|> algorithm_extension in
  tag_spec ~tag_name ~tag_value >>| binding

let n =
  let tag_name = string "n" >>| fun _ -> Map.K.n in
  let tag_value = qp_section in
  tag_spec ~tag_name ~tag_value >>| binding

let p =
  let tag_name = string "p" >>| fun _ -> Map.K.p in
  let tag_value = take_while1 is_base64 in
  tag_spec ~tag_name ~tag_value >>| binding

let key_s_tag_type =
  let all = char '*' *> return Value.All in
  let email = string "email" *> return Value.Email in
  let x_key_s_tag_type = hyphenated_word >>| fun x -> Value.Service_ext x in
  all <|> email <|> x_key_s_tag_type

let s =
  let tag_name = string "s" >>| fun _ -> Map.K.ss in
  let tag_value =
    key_s_tag_type >>= fun x ->
    many (char ':' *> key_s_tag_type) >>= fun r -> return (x :: r) in
  tag_spec ~tag_name ~tag_value >>| binding

let key_t_tag_flag =
  let y = char 'y' *> return Value.Y in
  let s = char 's' *> return Value.S in
  let x_key_t_tag_flag = hyphenated_word >>| fun x -> Value.Name_ext x in
  y <|> s <|> x_key_t_tag_flag

let t =
  let tag_name = string "t" >>| fun _ -> Map.K.st in
  let tag_value =
    key_t_tag_flag >>= fun x ->
    many (char ':' *> key_t_tag_flag) >>= fun r -> return (x :: r) in
  tag_spec ~tag_name ~tag_value >>| binding

let server_tag_list =
  let tag_spec = v <|> h <|> k <|> n <|> p <|> s <|> t in
  tag_spec >>= function
  | Some (Map.B (k, v)) ->
      many (char ';' *> tag_spec)
      <* option () (char ';' *> return ())
      >>| List.fold_left
            (fun hmap -> function
              | Some (Map.B (k, v)) -> Map.add k v hmap
              | None -> hmap)
            (Map.singleton k v)
  | None -> failf "Expect at least one tag"
