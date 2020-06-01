{
open Email_grammar
module LS = Email_lexer_state

module String = Bytes

let force_token token _lexbuf = token

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

(** RFC2822 3.2.3 -- Folding whitespace and comments *)
(* Obs: If this matches a CRLF, there's forcibly wsp afterwards *)
let obs_fws = wsp + (crlf wsp +) *
let fws = ((wsp * crlf) ? wsp + ) | obs_fws

let obs_char = [^ '\n' '\r' ]

(* Match lone CRs *)
(* This matching is not perfect, as in this case:
  - \r\r\r\r..\r\n
    It might not consider the last \r as part of a CRLF line ending.
    However, it fulfils the following properties:
    * On standards-compliant text, it will never match a CR that is part
      of a CRLF line-ending; standards compliant meaning that it doesn't
      have any lone CR pairs.
    * It works as expected on texts with lone CRs.
*)
let obs_cr_text = cr + [ ^ '\r' '\n']

let no_ws_printable = [ '\033' - '\126' ]

let utext =
  no_ws_ctl |
  no_ws_printable |
  obs_char |
  obs_cr_text

(* Beware of "a\n\t\nb". RFC2822, 3.2.3 says such headers must not be produced,
   but if they are, we must deal with them. *)
let unstructured = (fws? utext?)*

(**********************************************************)

(* Header fields *)
let ftext = [ '\033'-'\057' '\059'-'\126' ]
let field_name = ftext *

(* MESSAGE PARSING RULES *)

(* This lexer incorporates explicit state to allow it to process t
   the different parts of the email. This is because the tokens
   we want to generate depend on the context we are in.

   Another posibility is to handle everything in the grammar. However,
   each of the terminal symbols (characters) being a different
   state incurs significant overhead (both runtime and developing
   time).

   Statefulness is distasteful, so we keep it to a mininum:
     - Small number of states and transitions.
     - State is only read and set in one place, in the
       dispatcher.
*)

rule
(* Dispatcher *)
message t = parse
  | ""
  {
    match Core.Queue.dequeue t.LS.buf with
    | Some tok -> force_token tok lexbuf
    | None ->
      let result =
        match t.LS.state with
        (* Tokenization is vastly different depending on the context *)
        | `Header -> field lexbuf
        | `Content -> body_octet_stream lexbuf
        | `Expected_eof -> expected_eof lexbuf
      in
      begin
      LS.combine t result;
      message t lexbuf
      end
    }
and
(** Looks for a header name *)
field = parse
  | (field_name as name)
    (wsp * as _wsp)
     ":"
    (unstructured as body)
    crlf ?
    {
      (* This code is repeated to avoid cyclical dependencies. Effort has
       * been made to make it minimal.
       *)
      LS.return [FIELD(name, body)]
    }
  | crlf { LS.return ~new_state:`Content [HEADER_END] }
  | eof  { LS.return_eof }
  | "" { LS.return ~new_state:`Content [NO_HEADER_END] }

and

(** This rule throws an error if there are any more characters in the file *)
body_octet_stream =
  parse
  | "" | eof
  {
    assert (Lexing.lexeme_start lexbuf = Lexing.lexeme_end lexbuf);
    let pos = Lexing.lexeme_start lexbuf in
    LS.return ~new_state:`Expected_eof [ OCTET_STREAM_OFFSET (pos); EOF ]
  }
and


(* Supporting functions *)
error =
  parse
    | _ as c { LS.return_error (Core.sprintf "Unexpected character %c" c) }
    | eof    { LS.return_error "Unexpected EOF" }
and
expected_eof =
  parse
  | eof    { LS.return_eof }
  | ""     { error lexbuf }

{

}
