type algorithm = RSA | ED25519 | Algorithm_ext of string

type hash = SHA1 | SHA256 | Hash_ext of string

type canonicalization = Simple | Relaxed | Canonicalization_ext of string

type base64 = string

type version = int

type domain_name = string list

type auid = {
  local : [ `String of string | `Dot_string of string list ] option;
  domain : domain_name;
}

type quoted_printable = string

type dns_record = [ `TXT ]

type query =
  [ `DNS of dns_record | `Query_ext of string ] * quoted_printable option

type selector = string list

type flag = Y | S | Flag_ext of string

type copies = (Mrmime.Field_name.t * quoted_printable) list

type service = Email | All | Service_ext of string

type name = Y | S | Name_ext of string

type server_version = string

let pp_algorithm ppf = function
  | RSA -> Fmt.string ppf "rsa"
  | ED25519 -> Fmt.string ppf "ed25519"
  | Algorithm_ext x -> Fmt.string ppf x

let pp_hash ppf = function
  | SHA1 -> Fmt.string ppf "sha1"
  | SHA256 -> Fmt.string ppf "sha256"
  | Hash_ext x -> Fmt.string ppf x

let pp_canonicalization ppf = function
  | Simple -> Fmt.string ppf "simple"
  | Relaxed -> Fmt.string ppf "relaxed"
  | Canonicalization_ext x -> Fmt.string ppf x

let pp_domain_name = Fmt.(list ~sep:(const string ".") string)

let pp_selector = Fmt.(list ~sep:(const string ".") string)

let pp_auid ppf t =
  let pp_local ppf = function
    | `String x -> Fmt.(quote string) ppf x
    | `Dot_string l -> Fmt.(list ~sep:(const string ".") string) ppf l in
  Fmt.pf ppf "{ @[<hov>local = %a;@ domain= %a;@] }"
    Fmt.(option pp_local)
    t.local
    Fmt.(list ~sep:(const string ".") string)
    t.domain

let pp_query ppf (query, arg) =
  match query with
  | `DNS `TXT ->
      Fmt.pf ppf "dns/txt%a" Fmt.(option (prefix (const string ":") string)) arg
  | `Query_ext x ->
      Fmt.pf ppf "%s%a" x Fmt.(option (prefix (const string ":") string)) arg

let pp_copy = Fmt.Dump.pair Mrmime.Field_name.pp Fmt.string

let pp_service ppf = function
  | All -> Fmt.string ppf "*"
  | Email -> Fmt.string ppf "email"
  | Service_ext x -> Fmt.string ppf x

let pp_name ppf = function
  | Y -> Fmt.string ppf "y"
  | S -> Fmt.string ppf "s"
  | Name_ext x -> Fmt.string ppf x
