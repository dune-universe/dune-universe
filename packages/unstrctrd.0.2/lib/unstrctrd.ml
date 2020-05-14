type elt =
  [ `Uchar of Uchar.t
  | `WSP of wsp
  | `LF
  | `CR
  | `FWS of wsp
  | `d0
  | `OBS_NO_WS_CTL of obs
  | `Invalid_char of invalid_char ]
and wsp = string
and obs = char
and invalid_char = char

type t = elt list

type error = [ `Msg of string ]

let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let empty = []
let length = List.length

let of_string str =
  let module B = struct
    type t = bytes

    let blit_to_bytes = Bytes.blit
    let buf = Bytes.create 4096
  end in
  let module M = struct
    type 'a t =
      | Read of { buffer : Bytes.t; continue : int -> 'a t }
      | Fail of string
      | Done of 'a

    type buffer = bytes

    let return x = Done x

    let rec bind : 'a t -> ('a -> 'b t) -> 'b t = fun x f -> match x with
      | Read { buffer; continue; } ->
        let continue len = bind (continue len) f in
        Read { buffer; continue; }
      | Fail err -> Fail err
      | Done x -> f x

    let fail err = Fail err
    let read k buf = Read { buffer= buf; continue= k; }
  end in
  let lexbuf = Lexer.make () in
  let module Lexer = Lexer.Make(B)(M) in
  let pos = ref 0 in
  let rec go = function
    | M.Done lst ->
      let k res = Ok (lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos, res) in
      Pp.pp k lst
    | M.Fail err -> Error (`Msg err)
    | M.Read { buffer; continue; } ->
      let len = min (String.length str - !pos) (Bytes.length buffer) in
      Bytes.blit_string str !pos buffer 0 len ; pos := !pos + len ;
      go (continue len) in
  try go (Lexer.unstructured [] lexbuf)
  with _exn -> Error (`Msg "Unterminated input")

let safely_decode str = match of_string (str ^ "\r\n") with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg "%s" err (* XXX(dinosaure): should never occur! *)

let to_utf_8_string lst =
  let buf = Buffer.create (List.length lst) in
  let iter = function
    | `Invalid_char chr -> invalid_arg "Invalid byte: %02x" (Char.code chr)
    | `d0 -> Buffer.add_char buf '\000'
    | `WSP wsp -> Buffer.add_string buf wsp
    | `OBS_NO_WS_CTL chr -> Buffer.add_char buf chr
    | `Uchar uchar -> Uutf.Buffer.add_utf_8 buf uchar
    | `CRLF -> Buffer.add_string buf "\r\n"
    | `FWS wsp -> Buffer.add_string buf "\r\n" ; Buffer.add_string buf wsp
    | `LF -> Buffer.add_char buf '\n'
    | `CR -> Buffer.add_char buf '\r' in
  List.iter iter lst ; Buffer.contents buf

let without_comments lst =
  let rec go stack escaped acc = function
    | [] -> if stack = 0 then Ok (List.rev acc) else error_msgf "Non-terminating comment"
    | `Uchar uchar as value :: r ->
      ( match Uchar.to_int uchar with
        | 0x28 (* '(' *) ->
          if not escaped
          then go (succ stack) false acc r
          else
            ( if stack > 0
              then go stack false acc r
              else go stack false (value :: acc) r )
        | 0x29 (* ')' *) ->
          if not escaped
          then go (pred stack) false acc r
          else
            ( if stack > 0
              then go stack false acc r
              else go stack false (value :: acc) r )
        | 0x5c (* '\' *) ->
          if not escaped
          then go stack true acc r
          else
            ( if stack > 0
              then go stack false acc r
              else go stack false (value :: acc) r )
        | _ ->
          if stack > 0
          then go stack false acc r
          else go stack false (value :: acc) r )
    | value :: r ->
      if stack > 0 then go stack false acc r else go stack false (value :: acc) r in
  go 0 false [] lst

let replace_invalid_bytes ~f t =
  List.fold_left (fun a -> function
      | `Invalid_char chr ->
        ( match f chr with
          | Some v -> v :: a
          | None -> a )
      | v -> v :: a) [] t
  |> List.rev

let iter ~f l = List.iter f l
let fold ~f a l = List.fold_left f a l
let map ~f l = List.map f l

let wsp ~len = `WSP (String.make len ' ')
let tab ~len = `WSP (String.make len '\t')

let fws ?(tab = false) indent =
  if indent <= 0 then invalid_arg "fws: invalid indent argument" ;
  if tab then `FWS (String.make indent '\t') else `FWS (String.make indent ' ')

let split_at ~index l =
  if index < 0 || index > List.length l then invalid_arg "split_at: index (%d) is invalid" index ;

  let rec go n l r = match n with
    | 0 -> List.rev l, r
    | n -> match r with
      | [] -> assert false | x :: r -> go (pred n) (x :: l) r in
  go index [] l

let split_on ~on l =
  let rec go l r = match r, on with
    | [], _ -> None
    | `CR    :: r, `CR
    | `LF    :: r, `LF
    | `WSP _ :: r, `WSP
    | `FWS _ :: r, `FWS
    | `d0    :: r, `Char '\000' ->
      Some (List.rev l, r)
    | `Uchar a :: r, `Uchar b
      when Uchar.equal a b ->
      Some (List.rev l, r)
    | `Uchar a :: r, `Char b
      when Uchar.equal a (Uchar.of_char b) ->
      Some (List.rev l, r)
    | `OBS_NO_WS_CTL a :: r, `Char b
      when Char.equal a b ->
      Some (List.rev l, r)
    | x :: r, _ -> go (x :: l) r in
  go [] l

let of_list l =
  let has_cr = ref false in
  let exception Break in
  let f = function
    | `LF -> if !has_cr then raise_notrace Break ; has_cr := false
    | `CR -> has_cr := true
    | _   -> has_cr := false in
  try List.iter f l ; Ok l
  with Break -> error_msgf "of_list: An unexpected CRLF token exists"

let fold_fws t =
  let folder (fws, acc) = function
    | `FWS wsp -> if fws then (fws, acc) else (true, `WSP wsp :: acc)
    | x -> (false, x :: acc) in
  List.fold_left folder (false, []) t |> fun (_, t) -> List.rev t

module type BUFFER = Lexer.BUFFER
module type MONAD = Lexer.MONAD
module Make = Lexer.Make

let lexbuf_make = Lexer.make
let post_process = Pp.pp
