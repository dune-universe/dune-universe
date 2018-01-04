{

(* Can't open Core, as the OCamlLex generated code requires Array.create
 * to be of type (int -> 'a -> 'a t), not (len:int -> 'a -> 'a t) *)
module C = Core

module Quoted_printable = struct

  (* Quoted printable functions *)
  let decode_hex a b =
    let decode_digit c = match c with
      | '0'..'9' -> (Char.code c) - (Char.code '0') + 0
      | 'A'..'F' -> (Char.code c) - (Char.code 'A') + 10
      | 'a'..'f' -> (Char.code c) - (Char.code 'a') + 10
      | _ -> invalid_arg "c"
    in
    Char.chr (((decode_digit a) * 16) + (decode_digit b))
  ;;

  let hex_to = "0123456789ABCDEF"

  module Buffer = struct
    type t =
      {
        max_len : int;
        text : C.Bigbuffer.t;

        (** Invariant: The length of `word' is always less than `max_len' *)
        word : C.Bigbuffer.t;

        (** Invariant: `pos' is always less than `max_len' *)
        mutable pos  : int;
      }
    ;;

    let create len =
      {
        text = C.Bigbuffer.create len;
        word = C.Bigbuffer.create 16;
        max_len = 76;
        pos = 0;
      }
    ;;

    let add_break t =
      if t.pos > 0 then
      begin
        C.Bigbuffer.add_string t.text "=\n";
        t.pos <- 0
      end
    ;;

    (** Adds the current word to the buffer, wrapping it to the
      next line if necessary *)
    let commit_word t =
      if t.pos + C.Bigbuffer.length t.word >= t.max_len then add_break t;

      C.Bigbuffer.add_buffer t.text t.word;
      t.pos <- t.pos + C.Bigbuffer.length t.word;
      C.Bigbuffer.clear t.word
      ;
    ;;

    let wrap_bigword t len_next =
      if C.Bigbuffer.length t.word >= t.max_len - len_next then
      begin
        add_break t;
        commit_word t;
        add_break t;
      end
    ;;

    let add_char t c =
      wrap_bigword t 1;
      C.Bigbuffer.add_char t.word c;
    ;;


    let add_quoted_char t c =
      wrap_bigword t 3;
      let code = Char.code c in
      let high = (code lsr 4) land (16 - 1) in
      let low = code land (16 - 1) in
      C.Bigbuffer.add_char t.word '=';
      C.Bigbuffer.add_char t.word hex_to.[high];
      C.Bigbuffer.add_char t.word hex_to.[low]
    ;;

    (* When calling this function, one must not immediately
      call add_new_line.
    *)
    let add_wsp t c =
      add_char t c;
      commit_word t
    ;;

    let add_new_line t c =
      C.Option.iter c ~f:(fun c -> add_quoted_char t c);
      commit_word t;
      C.Bigbuffer.add_char t.text '\n';
      t.pos <- 0
    ;;

    let to_bigbuffer t =
      commit_word t;
      t.text
    ;;
  end

  (* TEST_MODULE = struct
   *   TEST = C.String.length hex_to = 16;;
   * end *)
end



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

(* Quoted-printable *)
let hexdigit_non_strict = ['0'-'9' 'a'-'f' 'A'-'F']
let hexdigit_strict = hexdigit_non_strict # ['a'-'f']
let qp_wsp = [' ' '\t']

let qp_allowed = [ '\033'-'\060' '\062'-'\126' ] as c

rule
(*  Quoted-printable text *)
decode_quoted_printable buffer dirty = parse
  | wsp * crlf
      {
        C.Bigbuffer.add_char buffer '\n';
        decode_quoted_printable buffer dirty lexbuf
      }
  | '=' crlf { decode_quoted_printable buffer dirty lexbuf }
  | '=' (hexdigit_strict as a) (hexdigit_strict as b)
      {
        C.Bigbuffer.add_char buffer (Quoted_printable.decode_hex a b);
        decode_quoted_printable buffer dirty lexbuf
      }
  | '=' (hexdigit_non_strict as a) (hexdigit_non_strict as b)
      {
        C.Bigbuffer.add_char buffer (Quoted_printable.decode_hex a b);
        decode_quoted_printable buffer `Unexpected_characters lexbuf
      }
  | (qp_allowed | wsp) as c
      {
        C.Bigbuffer.add_char buffer c;
        decode_quoted_printable buffer dirty lexbuf;
      }
  | _ as c
      {
        (* This characters shoudn't appear in a quoted-printable body,
        buy the most robust way to handle them is probably copying
        them verbatim *)
        C.Bigbuffer.add_char buffer c;
        decode_quoted_printable buffer `Unexpected_characters lexbuf
      }
  | eof { dirty }

and

(* Quoted-printable encoding with wrapping *)
encode_quoted_printable buffer = parse
  | (wsp as c) ? '\n'
    {
      Quoted_printable.Buffer.add_new_line buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | wsp as c
    {
      Quoted_printable.Buffer.add_wsp buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | qp_allowed as c
    {
      Quoted_printable.Buffer.add_char buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | _ as c
    {
      Quoted_printable.Buffer.add_quoted_char buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | eof
    {
      ()
    }

{

let decode_quoted_printable len lexbuf =
  let length_estimate = len in
  let bigbuffer = C.Bigbuffer.create length_estimate in
  let warnings = decode_quoted_printable bigbuffer `Ok lexbuf in
  (bigbuffer, warnings)
;;

let encode_quoted_printable len lexbuf =
  let length_estimate = len in
  let buffer = Quoted_printable.Buffer.create length_estimate in
  encode_quoted_printable buffer lexbuf
  ;

  Quoted_printable.Buffer.to_bigbuffer buffer
;;

}

