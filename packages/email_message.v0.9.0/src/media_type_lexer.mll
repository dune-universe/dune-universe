{
open Media_type_grammar

module String = Bytes

let unescape_staged = Core.String.Escaping.unescape ~escape_char:'\\';;
let unescape = Core.unstage unescape_staged;;

}
(* Rules from RFCs. Some may need to be copied to specific places to parse
 * their individual parts *)

(** RFC2234 - Core *)
let cr = "\013"
let lf = "\010"

(* XXX: Should be just cr lf *)
(* Deliberately chose to include bare CR and LF here, although the RFC suggests
 * them to be included as part of the text characters.
 * The rationale being that, when parsing e-mail from a text file, they will
 * probably mean CRLF.
 * The issue should not arise in conforming e-mails.
 *)
let crlf_conforming = cr lf
let crlf_non_conforming = crlf_conforming | lf
let crlf = crlf_non_conforming

let wsp = [' ' '\t']

(** RFC2822 3.2.1 -- Primitive tokens *)

let no_ws_ctl = [ '\001'-'\008' '\011' '\012' '\014'-'\031' '\127']

(* See crlf *)
let text_non_conforming = [ '\001'-'\009' '\011' '\012' '\014'-'\127' ]

(** RFC2822 3.2.2 -- Quoted characters  *)
let obs_qp = "\\" [ '\000' - '\127' ]
(* Allows for escaping of newlines *)
let crlf_qp_non_conforming = "\\" crlf

(** XXX: crlf-qp-non-conforming shouldn't be here. *)
let quoted_pair = "\\" text_non_conforming | crlf_qp_non_conforming | obs_qp

(** RFC2822 3.2.3 -- Folding whitespace and comments *)
(* Obs: If this matches a CRLF, there's forcibly wsp afterwards *)
let obs_fws = wsp + (crlf wsp +) *
let fws = ((wsp * crlf) ? wsp + ) | obs_fws

(** RFC2822 3.2.5 -- Quoted strings *)
let qtext = no_ws_ctl | [ '\033' '\035'-'\091' '\093'-'\126' ]
let qcontent = qtext | quoted_pair

let quoted_string_contents = (fws ? qcontent) * fws ?
(*
let quoted_string = cfws ? '"' quoted_string_contents '"' cfws ?
(** RFC2822 3.2.6 -- Miscellaneous tokens *)
let word = atom | quoted_string

let obs_phrase = word (word | "." | cfws) *
let phrase = word + | obs_phrase
*)

(**********************************************************)

(* RFC 2045 5.1 -- Syntax of the Content-type header field *)
(* Removed space characters, control characters, those outsize US-ASCII and
 * tspecials *)
let token_char =
  ([ ^ '\n' '\r'  ' '
       '(' ')' '<' '>' '@' ',' ';' ':' '\\' '"' '/' '[' ']' '?' '='
       '\000' - '\031' '\127' ])

let token = token_char +

rule

(******************)
(* HEADER PARSING *)
(******************)
(* These lexers are independent from the former, and work the way traditional
lexers do, without a dispatcher or state handlers *)


(** Allows other lexers to skip comments.

   Example usage:
rule my_lexer = parse
  | '('      -> { my_lexer (comment 1 lexbuf) }
  | .. other patterns ..

*)
comment level = parse
  | '('            { comment (level + 1) lexbuf }
  | ')' fws *      {
    if level <= 1 then
    begin
      assert (level = 1); lexbuf
    end
    else
      comment (level - 1) lexbuf }
  | '\\' ? _ { comment level lexbuf }
and

(* Parses a Content-type field *)
content_type = parse
  | '('          { content_type (comment 1 lexbuf) }
  | fws          { content_type lexbuf }
  | '"' (quoted_string_contents as str) '"' { STRING (unescape str) }
  | '/'          { SLASH }
  | token as str { ATOM (str) }
  | '='          { EQUALS }
  | ';'          { SEMICOLON }
  | _ as c       { ERROR (Core.sprintf "Unexpected char: %c" c) }
  | eof          { EOF }


{

}

