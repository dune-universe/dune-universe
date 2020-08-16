open Core
open Httpaf
open Import

let cookies_parser =
  let open Angstrom in
  let cookie_name =
    take_while1 (function
      | '\000' .. '\031' | '\127' -> false
      | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
      | ']' | '?' | '=' | '{' | '}' | ' ' ->
          false
      | _ -> true)
  in
  let cookie_value =
    let cookie_value_chars =
      take_while (function
        | '!' | '#' .. '+' | '-' .. ':' | '<' .. '[' | ']' .. '~' -> true
        | _ -> false)
    in
    cookie_value_chars <|> (char '"' *> cookie_value_chars <* char '"')
  in
  let sep = string "; " in
  sep_by sep (both (cookie_name <* char '=') cookie_value)

let extract headers =
  Headers.get headers "Cookie"
  |> Option.bind ~f:(fun value ->
         Angstrom.parse_string ~consume:All cookies_parser value |> Result.ok)
  |> Option.value ~default:[]
