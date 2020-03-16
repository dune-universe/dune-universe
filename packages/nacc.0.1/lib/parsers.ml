(**************************************************************************)
(*                           _   _    _    ____ ____                      *)
(*                          | \ | |  / \  / ___/ ___|                     *)
(*                          |  \| | / _ \| |  | |                         *)
(*                          | |\  |/ ___ \ |__| |___                      *)
(*                          |_| \_/_/   \_\____\____|                     *)
(*                                                                        *)
(*                                                                        *)
(*                        Copyright (c) 2020 - CodeAnon                   *)
(**************************************************************************)

open Parsing

(** Parse a single char *)
let char c = check (( == ) c)

(** Parser for any char of a given list *)
let one_of cl = check (fun c -> List.mem c cl)

(** Parse one any char of a given string *)
let one_in s = check (String.contains s)

(** Parse on of the char of a given list *)
let spaced p =
  let spaces = many (check (( == ) ' ')) in
  spaces *> p <* spaces

(** Parser for integer litterals *)
let integer =
  let convert l =
    List.(fold_left ( ^ ) "" (map (String.make 1) l)) |> int_of_string
  in
  convert <$> some (one_in "0123456789")

let floatingpoint =
  let convert l =
    List.(fold_left ( ^ ) "" (map (String.make 1) l)) |> float_of_string
  in
  let concat a b = List.concat [ a; b ] in
  let ( &> ) p1 p2 = concat <$> p1 <*> p2 in
  let maybe p inp =
    match inp --> p with
    | Some (x, r) -> Some ([ x ], r)
    | None -> Some ([], inp)
  in
  convert
  <$> ( many (one_in "0123456789")
      &> ~~(maybe (char '.'))
      &> some (one_in "0123456789")
      <|> many (one_in "0123456789") )

(** Parser for binary operations patterns
    @param  cons    a 2-parameters constructor
    @param  c       an operator character
    @param  v       any parser *)
let binop cons c v = cons <$> v <*> spaced (char c) *> v

(** Parser for binary operations patterns
    @param  cons    a 2-parameters constructor
    @param  cc      a parser (for the binary operator)
    @param  v       any parser *)
let cbinop cons cc v = cons <$> v <*> spaced cc *> v

(** Parser for optionnal white spaces *)
let blanks = many (char ' ' <|> char '\t' <|> char '\n')

(** Parser for at least one white space *)
let force_blanks = some (char ' ' <|> char '\t' <|> char '\n')

(** Parser parenthesized data *)
let parenthesized opar v cpar = spaced (char opar) *> v <* spaced (char cpar)

(** Custom parenthesized data *)
let cparenthesized copar v ccpar = spaced copar *> v <* spaced ccpar
