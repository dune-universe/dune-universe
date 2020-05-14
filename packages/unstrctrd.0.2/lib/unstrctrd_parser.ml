let read
  : Lexing.lexbuf -> (int -> 'a Angstrom.t) -> bytes -> 'a Angstrom.t
  = fun lexbuf continue buffer ->
    let open Angstrom in
    fix @@ fun m0 ->
    pos >>= fun curr_pos0 ->
    let curr_pos0 = curr_pos0 - lexbuf.Lexing.lex_abs_pos in
    fix @@ fun m1 ->
    pos >>= fun curr_pos1 ->
    let curr_pos1 = curr_pos1 - lexbuf.Lexing.lex_abs_pos in

    if curr_pos1 - curr_pos0 > 0
    then ( peek_char >>= function
           | Some _ -> commit *> m0
           | None -> ( try continue 0 with _ -> fail "Invalid unstructured form" ) )
    else ( available >>= fun len ->
           let saved = lexbuf.Lexing.lex_curr_pos - curr_pos0 in

           match len - saved with
           | 0 ->
             ( peek_char >>= function
                 | Some _ -> ((advance len *> m1) <|> m0)
                 | None -> ( try continue 0 with _ -> fail "Invalid unstructured form" ) )
           | rest ->
             peek_string len >>= fun src ->
             let len = min rest (Bytes.length buffer) in
             Bytes.blit_string src saved buffer 0 len ;
             ((continue len) <|> m1) )

let unstrctrd buf =
  let lexbuf = Unstrctrd.lexbuf_make () in
  let read continue buffer = read lexbuf continue buffer in
  let module Buffer = struct
    type t = bytes
    let blit_to_bytes = Bytes.blit
    let buf = buf
  end in
  let module Monad = struct
    type 'a t = 'a Angstrom.t
    type buffer = bytes

    let return = Angstrom.return
    let bind x f =
      Angstrom.(x >>= fun x -> try f x with Failure _ -> Angstrom.fail "Invalid unstructured form")
    let fail = Angstrom.fail
    let read = read
  end in
  let module State = Unstrctrd.Make(Buffer)(Monad) in
  let open Angstrom in
  let trailer v =
    pos >>= fun curr_pos ->
    available >>= fun available ->
    let curr_pos = curr_pos - lexbuf.Lexing.lex_abs_pos in
    let saved = lexbuf.Lexing.lex_curr_pos - curr_pos in
    ( if saved <= available then advance saved else return () ) *> return v in

  pos >>= fun lex_abs_pos ->
  lexbuf.Lexing.lex_abs_pos <- lex_abs_pos ;
  State.unstructured [] lexbuf >>= trailer >>= Unstrctrd.post_process return
