{
module type MONAD = sig
  type 'a t
  type buffer

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : string -> 'a t
  val read : (int -> 'a t) -> buffer -> 'a t
end

module type BUFFER = sig
  type t

  val blit_to_bytes : t -> int -> bytes -> int -> int -> unit
  val buf : t
end

let unlex buf n =
  let open Lexing in
  if n < 0 || n > buf.lex_curr_pos - buf.lex_start_pos
  then invalid_arg "unlex" ;
  buf.lex_curr_pos <- buf.lex_curr_pos - n ;
  if buf.lex_curr_p != dummy_pos
  then buf.lex_curr_p <- { buf.lex_start_p with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos }
;;

let zero_pos =
  let open Lexing in
  { pos_fname = ""
  ; pos_lnum = 1
  ; pos_bol = 0
  ; pos_cnum = 0 }

let make () =
  let open Lexing in
  { refill_buff = (fun _lexbuf -> ())
  ; lex_buffer = Bytes.create 1024
  ; lex_buffer_len = 0
  ; lex_abs_pos = 0
  ; lex_start_pos = 0
  ; lex_curr_pos = 0
  ; lex_last_pos = 0
  ; lex_last_action = 0
  ; lex_mem = [||]
  ; lex_eof_reached = false
  ; lex_start_p = zero_pos
  ; lex_curr_p = zero_pos }

module Make (Buffer : BUFFER) (Monad : MONAD with type buffer = Buffer.t) = struct
  let invalid_arg fmt = Format.kasprintf invalid_arg fmt

  let fill_lexbuf buf len lexbuf =
    let open Lexing in
    let n = if len > 0 then len else ( lexbuf.lex_eof_reached <- true ; 0 ) in
    if lexbuf.lex_buffer_len + n > Bytes.length lexbuf.lex_buffer
    then ( if lexbuf.lex_buffer_len - lexbuf.lex_start_pos + n <= Bytes.length lexbuf.lex_buffer
           then Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos lexbuf.lex_buffer 0 (lexbuf.lex_buffer_len - lexbuf.lex_start_pos)
           else ( let new_len = min (max (2 * Bytes.length lexbuf.lex_buffer) n) Sys.max_string_length in
                  if lexbuf.lex_buffer_len - lexbuf.lex_start_pos + n > new_len
                  then ( failwith "Lexing.fill_lexbuf: cannot grow buffer" )
                ; let new_buf = Bytes.create new_len in
                  Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos new_buf 0 (lexbuf.lex_buffer_len - lexbuf.lex_start_pos)
                ; lexbuf.lex_buffer <- new_buf )
          ; let s = lexbuf.lex_start_pos in
            lexbuf.lex_abs_pos <- lexbuf.lex_abs_pos + s
          ; lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - s
          ; lexbuf.lex_start_pos <- 0
          ; lexbuf.lex_last_pos <- lexbuf.lex_last_pos - s
          ; lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len - s
          ; let t = lexbuf.lex_mem in
            for i = 0 to Array.length t - 1 do
              let v = t.(i) in
              if v >= 0 then t.(i) <- v - s
            done ) ;
    Buffer.blit_to_bytes buf 0 lexbuf.lex_buffer lexbuf.lex_buffer_len n ;
    lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len + n
  ;;

  let on_refill lexbuf =
    let k len = fill_lexbuf Buffer.buf len lexbuf ; Monad.return () in
    Monad.read k Buffer.buf

  let refill_handler k lexbuf =
    Monad.bind (on_refill lexbuf) (fun () -> k lexbuf)

  let make = make
}

let wsp = [ ' ' '\t' ]
let not_wsp = [ ^ ' ' '\t' ]
let crlf = '\r' '\n'
let cr = '\r'
let lf = '\n'
let not_lf = [ ^ '\n' ]
let obs_no_ws_ctl = [ '\001'-'\008' '\011' '\012' '\014'-'\031' '\127' ]
let vchar_ascii = [ '\x21'-'\x7e' ]
let utf8_tail = [ '\x80'-'\xbf' ]

let vchar =
  vchar_ascii
  | ([ '\xc2'-'\xdf' ] utf8_tail)
  | ('\xe0' [ '\xa0'-'\xbf' ] utf8_tail)
  | ([ '\xe1'-'\xec' ] utf8_tail utf8_tail)
  | ('\xed' [ '\x80'-'\x9f' ] utf8_tail)
  | ([ '\xee'-'\xef' ] utf8_tail utf8_tail)
  | ('\xf0' [ '\x90'-'\xbf' ] utf8_tail utf8_tail)
  | ([ '\xf1'-'\xf3' ] utf8_tail utf8_tail utf8_tail)
  | ('\xf4' [ '\x80'-'\x8f' ] utf8_tail utf8_tail)

let obs_utext = '\000' | obs_no_ws_ctl | vchar

let fws = wsp* crlf wsp+ | wsp+

refill {refill_handler}

rule obs_unstruct acc = parse
  | (lf* as lf0) (cr* as cr0) crlf eof
    { let lf0 = String.length lf0 in
      let cr0 = String.length cr0 in
      Monad.return (List.rev (`OBS_UTEXT (lf0, cr0, "") :: acc)) }
  | (lf* as lf0) (cr* as cr0) crlf not_wsp
    { unlex lexbuf 1 ;
      let lf0 = String.length lf0 in
      let cr0 = String.length cr0 in
      Monad.return (List.rev (`OBS_UTEXT (lf0, cr0, "") :: acc)) }

(* ( *LF *CR *(obs-utext *LF *CR) ) *)

  | (lf* as lf0) (cr* as cr0) (obs_utext+ as x) crlf eof
    { let lf0 = String.length lf0 in
      let cr0 = String.length cr0 in
      Monad.return (List.rev (`OBS_UTEXT (lf0, cr0, x) :: acc)) }
  | (lf* as lf0) (cr* as cr0) ( obs_utext+ as x) crlf not_wsp
    { unlex lexbuf 1 ;
      let lf0 = String.length lf0 in
      let cr0 = String.length cr0 in
      Monad.return (List.rev (`OBS_UTEXT (lf0, cr0, x) :: acc)) }
  | (lf* as lf0) (cr* as cr0) ((obs_utext lf* cr*)+ as x) not_lf
    { unlex lexbuf 1 ;
      let lf0 = String.length lf0 in
      let cr0 = String.length cr0 in
      obs_unstruct (`OBS_UTEXT (lf0, cr0, x) :: acc) lexbuf }

  | lf+ as lf { obs_unstruct (`OBS_UTEXT (String.length lf, 0, "") :: acc) lexbuf }
  | cr+ as cr not_lf { unlex lexbuf 1 ; obs_unstruct (`OBS_UTEXT (0, String.length cr, "") :: acc) lexbuf }

(* / FWS *)

  | fws as fws { obs_unstruct (`FWS fws :: acc) lexbuf }
  | _ as chr { obs_unstruct (`Invalid_char chr :: acc) lexbuf }
  | eof { Monad.fail "Non-terminating unstructured form" }

and unstructured acc = parse
  | crlf eof { Monad.return (List.rev acc) }
  | crlf not_wsp { unlex lexbuf 1 ; Monad.return (List.rev acc) }

(* *([FWS] VCHAR) *WSP *)

  | fws as fws { unstructured (`FWS fws :: acc) lexbuf }
  | vchar+ as x { unstructured (`VCHAR x :: acc) lexbuf }
  | wsp+ as wsp { unstructured (`WSP wsp :: acc) lexbuf } (* XXX(dinosaure): not sure! *)

(* / obs-unstruct *)

  | cr { obs_unstruct (`OBS_UTEXT (0, 1, "") :: acc) lexbuf }
  | lf { obs_unstruct (`OBS_UTEXT (1, 0, "") :: acc) lexbuf }
  | obs_utext+ as obs_utext { obs_unstruct (`OBS_UTEXT (0, 0, obs_utext) :: acc) lexbuf }
  | _ as chr { unstructured (`Invalid_char chr :: acc) lexbuf }
  | eof { Monad.fail "Non-terminating unstrctured form" }

{
end
}
