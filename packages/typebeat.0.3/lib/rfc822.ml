type word   = [ `Atom of string | `String of string ]
type domain = [ `Domain of string list | `Literal of string ]
type local  = word list
type msg_id = (local * domain)

open Angstrom

let is_vchar = function
  | '\x21' .. '\x7e' -> true
  | _ -> false

let is_obs_no_ws_ctl = function
  | '\001' .. '\008'
  | '\011'
  | '\012'
  | '\014' .. '\031'
  | '\127' -> true
  | _ -> false

let is_ctext = function
  | '\033' .. '\039'
  | '\042' .. '\091'
  | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let is_qtext = function
  | '\033'
  | '\035' .. '\091'
  | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let is_atext = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&'
  | '\''
  | '*' | '+' | '-' | '/' | '='
  | '?' | '^' | '_' | '`' | '{'
  | '}' | '|' | '~' -> true
  | _ -> false

let is_cr = (=) '\r'
let is_lf = (=) '\n'
let is_d0 = (=) '\000'

let is_wsp = function
  | '\x09' | '\x20' -> true
  | _ -> false

let is_quoted_pair chr =
  is_vchar chr
  || is_wsp chr
  || is_d0 chr
  || is_obs_no_ws_ctl chr
  || is_lf chr
  || is_cr chr

let is_dtext = function
  | '\033' .. '\090'
  | '\094' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let of_escaped_character = function
  | '\x61' -> '\x07'
  | '\x62' -> '\x08'
  | '\x74' -> '\x09'
  | '\x6E' -> '\x0A'
  | '\x76' -> '\x0B'
  | '\x66' -> '\x0C'
  | '\x72' -> '\x0D'
  | c      -> c

let e = char '\\'

let quoted_pair_ignore, quoted_pair =
  let quoted_char = char '\\' *> satisfy is_quoted_pair in
  quoted_char *> return (), quoted_char >>| of_escaped_character

let wsp = satisfy is_wsp

let crlf = char '\r' *> char '\n' *> return ()

let obs_fws =
  let many' p = fix
    @@ fun m -> (lift2 (fun _ _ -> (true, true)) p m)
                <|> return (false, false)
  in
  many1 wsp
  *> (many' (crlf *> (many1 wsp)))
  >>| fun (has_crlf, has_wsp) -> (true, has_crlf, has_wsp)

let fws =
  let many' p = fix
    @@ fun m -> (lift2 (fun _ _ -> (true, true)) p m)
                <|> return (false, true)
  in
  obs_fws
  <|> ((option (false, false) (many' wsp <* crlf))
       >>= fun (has_wsp, has_crlf) -> many1 wsp
       *> return (has_wsp, has_crlf, true))

let comment =
  (fix @@ fun comment ->
    let ccontent =
      peek_char_fail <?> "comment"
      >>= function
        | '('               -> comment
        | '\\'              -> quoted_pair_ignore
        | c when is_ctext c -> skip_while is_ctext  (* TODO: replace skip_while and handle unicode. *)
        | _                 -> fail "comment"
   in
   char '('
   *> (many ((option (false, false, false) fws) *> ccontent))
   *> (option (false, false, false) fws)
   *> char ')'
   *> return ())

let cfws =
  ((many1 ((option (false, false, false) fws)
         *> comment)
    *> (option (false, false, false) fws))
   <|> fws)
  *> return ()

let cfws = cfws <?> "cfws"

let qcontent =
  (take_while1 is_qtext) (* TODO: replace take_while and handle unicode. *)
  <|> (quoted_pair >>| String.make 1)

(* TODO: optimize. *)
let quoted_string =
  (option () cfws)
  *> char '"'
  *> (many (option (false, false, false) fws
            >>= fun (has_wsp, _, has_wsp') -> qcontent
            >>= fun s -> return (if has_wsp || has_wsp'
                                 then (String.concat "" [" "; s])
                                 else s))
      >>= fun pre -> option (false, false, false) fws
      >>= fun (has_wsp, _, has_wsp') -> return (if has_wsp || has_wsp'
                                                then " " :: pre
                                                else pre))
  <* char '"'
  >>| String.concat ""
  <* (option () cfws)

let atom =
  (option () cfws)
  *> (take_while1 is_atext) (* TODO: replace take_while and handle unicode. *)
  <* (option () cfws)

let word =
  (atom >>| fun s -> `Atom s)
  <|> (quoted_string >>| fun s -> `String s)

let obs_local_part =
  sep_by1 (char '.') word

let dot_atom_text =
  (* TODO: replace satisfy and handle unicode. *)
  sep_by1 (char '.') (take_while1 is_atext)

let dot_atom =
  (option () cfws) *> dot_atom_text <* (option () cfws)

let local_part =
  obs_local_part
  <|> (dot_atom >>| List.map (fun x -> `Atom x))
  <|> (quoted_string >>| fun s -> [`String s])

let obs_domain =
  lift2 (fun x r -> x :: r)
    atom
    (many1 (char '.' *> atom))

let domain_literal =
  (option () cfws)
  *> char '['
  *> (many ((option (false, false, false) fws)
            *> ((take_while1 is_dtext) <|> (quoted_pair >>| String.make 1)))
                 (* TODO: replace take_while and handle unicode. *)
      >>| String.concat "")
  <* (option (false, false, false) fws)
  <* char ']'
  <* (option () cfws)

let domain =
  (obs_domain >>| fun domain -> `Domain domain)
  <|> (domain_literal >>| fun literal -> `Literal literal)
  <|> (dot_atom >>| fun domain -> `Domain domain)

let id_left = local_part <|> (dot_atom_text >>| List.map (fun x -> `Atom x))

let no_fold_literal =
  char '['
  *> take_while is_dtext (* TODO: replace take_while and handle unicode. *)
  <* char ']'

let id_right =
  (no_fold_literal >>| fun literal -> `Literal literal)
  <|> domain
  <|> (dot_atom_text >>| fun domain -> `Domain domain)

let msg_id =
  option () cfws *>
  lift2 (fun x y -> (x, y))
    (char '<' *> id_left)
    (char '@' *> id_right <* char '>')
  <* option () cfws
