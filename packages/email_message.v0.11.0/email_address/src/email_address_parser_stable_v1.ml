open! Core_kernel

open! Core_kernel.Int.Replace_polymorphic_compare

open Angstrom

type t =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }

let string_contains = Core_kernel.String.contains

let whitespace_chars = " \r\n\t"
let not_unquoted_prefix_chars = "<>@,\""

let quoted_prefix =
  (* Includes the quotes. *)
  char '"' *> take_while (fun chr -> not (Char.equal '"' chr)) <* char '"'
  >>| fun s -> "\"" ^ s ^ "\""

let unquoted_prefix =
  (fun first_char rest -> Core_kernel.String.(of_char first_char ^ rest))
  <$> satisfy (fun chr ->
    not (string_contains not_unquoted_prefix_chars chr
         || string_contains whitespace_chars chr))
  <*> take_while (fun chr ->
    not (string_contains not_unquoted_prefix_chars chr))

let prefix =
  (* Includes the whitespace. *)
  (fun prefix_main whitespace -> prefix_main ^ whitespace)
  <$> ((quoted_prefix <|> unquoted_prefix) <?> "prefix")
  <*> take_while (fun chr -> string_contains whitespace_chars chr)

let not_address_chars = "<>@," ^ whitespace_chars
let not_domain_chars = not_address_chars ^ "'\""

let address_part =
  (fun local_part domain prefix -> { prefix; domain; local_part })
  <$> (take_while1 (fun chr -> not (string_contains not_address_chars chr)) <?> "local_part")
  <*> (option None ((fun x -> Some x) <$> (
    char '@' *> take_while1 (fun chr -> not (string_contains not_domain_chars chr)))) <?> "domain")

let email_without_prefix =
  (fun f -> f None)
  <$> address_part

let email_with_prefix =
  (fun x f -> f (Some x))
  <$> option "" prefix
  <*> (char '<' *> address_part <* char '>')

let email = (email_with_prefix <|> email_without_prefix) <?> "email"

let skip_whitespace = skip_while (string_contains whitespace_chars)

let email_list =
  let delim = skip_whitespace *> char ',' <* skip_whitespace in
  sep_by delim email <?> "email_list"

let parse_only x =
  skip_whitespace *> x <* skip_whitespace <* end_of_input

let email_only = parse_only email
let email_list_only = parse_only email_list
