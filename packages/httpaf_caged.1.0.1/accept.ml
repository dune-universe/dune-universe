open Core
open Httpaf
open Import

type media_type = Any | Any_sub_type of string | Type of string * string

let media_type_parser =
  let open Angstrom in
  let ows = take_while (function ' ' | '\t' -> true | _ -> false) in
  let quoted_string =
    let lone_chars =
      take_while (function
        | '\t' | ' ' | '\x21'
        | '\x23' .. '\x5B'
        | '\x5D' .. '\x7E'
        | '\x80' .. '\xFF' ->
            true
        | _ -> false)
    in
    let quoted_char =
      char '\\'
      *> Angstrom.satisfy (function
           | '\t' | ' ' | '\x21' .. '\x7E' | '\x80' .. '\xFF' -> true
           | _ -> false)
      >>| Char.to_string
    in
    char '"' *> (many (lone_chars <|> quoted_char) >>| String.concat)
    <* char '"'
  in
  let token =
    take_while1 (function
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
      | '`' | '|' | '~'
      | '0' .. '9'
      | 'a' .. 'z'
      | 'A' .. 'Z' ->
          true
      | _ -> false)
  in
  let media_type =
    choice
      [
        string "*/*" >>| const Any;
        (token <* string "/*" >>| fun type_ -> Any_sub_type type_);
        ( both (token <* char '/') token >>| fun (type_, sub_type) ->
          Type (type_, sub_type) );
      ]
  in
  let parameter = both token (char '=' *> (token <|> quoted_string)) in
  let media_type_with_params =
    both media_type (many (ows *> char ';' *> ows *> parameter))
  in
  sep_by (ows *> char ',' <* ows) media_type_with_params
  >>| List.map ~f:(fun (media_type, params) ->
          let q =
            List.Assoc.find ~equal:String.equal params "q"
            |> Option.value_map ~f:Float.of_string ~default:1.
          in
          (media_type, q))

let extract headers =
  Headers.get headers "accept"
  |> Option.bind ~f:(fun value ->
         Angstrom.parse_string ~consume:All media_type_parser value |> Result.ok)
  |> Option.value ~default:[ (Any, 1.0) ]
