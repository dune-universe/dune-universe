open Core
open Angstrom
open ANSITerminal
   
module Parser = struct
  type example =
    | Command of string
    | Argument of string
  
  type expression =
    | Title of string
    | Description of string
    | Example of string * (example list)

  let _title t       = Title t
  let _description d = Description d
  let _example h b   = Example (h, b)
  let _command c     = Command c
  let _argument a    = Argument a

  let is_eol = function | '\n' | '\r' -> true | _ -> false
  let is_command = function | '{' | '`' -> false | _ -> true
  let not_brace = function | '{' | '}' -> false | _ -> true

  let eol    = string "\n" <|> string "\r\n"
  let tstart = string "# "
  let dstart = string "> "
  let estart = string "- "
  let tick   = char '`'
  let b_arg  = string "{{"
  let e_arg  = string "}}"

  let newlines = take_while is_eol
  let take_line = take_till is_eol

  let title       = _title       <$> (tstart *> take_line)
  let description = _description <$> (dstart *> take_line)
  let example =
    let command     = _command  <$> (take_while1 is_command) in
    let argument    = _argument <$> (b_arg *> take_while1 not_brace <* e_arg) in
    let head = (^) <$> estart <*> take_line <* newlines in
    let body = tick *> many (command <|> argument) <* tick in
    _example <$> head <*> body
                  
  let form = newlines *> (title <|> description <|> example) <* newlines

  let parse page =
    match parse_string (many form) page with
    | Ok v -> v
    | Error msg -> failwith msg
end


let color_example = function
  | Parser.Command com  -> sprintf [red] "%s" com
  | Parser.Argument arg -> sprintf [blue] "%s" arg

let color_display = function
  | Parser.Title title        -> sprintf [white; Bold] "\n%s\n\n" title
  | Parser.Description descr  -> sprintf [white] "%s\n" descr
  | Parser.Example (ex, body) -> sprintf [green] "\n%s\n    %s\n" ex
                                   (body
                                    |> List.map ~f:color_example
                                    |> String.concat)
                               
let display page =
  Parser.parse page
  |> List.map ~f:color_display
  |> String.concat
  |> Printf.printf "%s"
