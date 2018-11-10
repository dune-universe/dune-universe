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

  let is_eol   = function '\n' | '\r' -> true  | _ -> false
  let is_command = function '{' | '`' -> false | _ -> true
  let not_brace  = function '{' | '}' -> false | _ -> true

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


module Colors = struct
  let string_of_style = function
    | "black"      -> black
    | "red"        -> red
    | "green"      -> green
    | "yellow"     -> yellow
    | "blue"       -> blue
    | "magenta"    -> magenta
    | "cyan"       -> cyan
    | "white"      -> white
    | "on_black"   -> on_black
    | "on_red"     -> on_red
    | "on_green"   -> on_green
    | "on_yellow"  -> on_yellow
    | "on_blue"    -> on_blue
    | "on_magenta" -> on_magenta
    | "on_cyan"    -> on_cyan
    | "on_white"   -> on_white
    | "bold"       -> Bold
    | "underlined" -> Underlined
    | "blink"      -> Blink
    | _            -> default

  let color_from_environment env default =
    let f string = String.split ~on:';' string
                   |> List.map ~f:string_of_style
    in
    Sys.getenv env
    |> Option.value_map ~default:default ~f:f

  let command_style = color_from_environment "TLDR_COLOR_COMMAND" [red]
  let argument_style = color_from_environment "TLDR_COLOR_ARGUMENT" [blue]
  let title_style = color_from_environment "TLDR_COLOR_TITLE" [white; Bold]
  let description_style = color_from_environment "TLDR_COLOR_DESCRIPTION" [white]
  let example_style = color_from_environment "TLDR_COLOR_EXAMPLE" [green]

  let color_example =
    (* This is all to deal with underlined spaces looking weird*)
    let color x style =
      x
      |> String.split ~on:' '
      |> List.map ~f:(function "" -> "" | x -> sprintf style "%s" x)
      |> String.concat ~sep:" "
    in
    function
    | Parser.Command command  -> color command command_style
    | Parser.Argument argument -> color argument argument_style


  let color_expression = function
    | Parser.Title title        -> sprintf title_style "%s\n\n" title
    | Parser.Description descr  -> sprintf description_style "%s\n" descr
    | Parser.Example (ex, body) -> sprintf example_style "\n%s\n" ex
                                   ^ "  "
                                   ^ (List.map ~f:color_example body
                                      |> String.concat)
                                   ^ "\n"
end
                               
let display page =
  Parser.parse page
  |> List.map ~f:Colors.color_expression
  |> String.concat 
  |> Printf.printf "%s"
