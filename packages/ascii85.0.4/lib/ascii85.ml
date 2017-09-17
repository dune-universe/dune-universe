(** This module implements the Ascii85 Encoding as specified by 
    Adobe's "PostScript LANGUAGE REFERENCE third edition". It reads
    a named file and emits it in the new encoding on standard output.
    For the details of the format, see "ASCII Base-85 Strings" and
    "ASCII85Encode Filter".    

    @author Christian Lindig <lindig\@gmail.com>

    See LICSENSE.md for the license.
*)

exception Error of string
let error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt

(** number of character after which we emit a newline into
    the output stream *)

let line_length = 60    (** line length for output *)

(* We need 32 unsigned bits. I could not get it to work with (signed)
   Int32.t and moved to signed Int64 *)

(** Operations on 32-bit integers *)
let (<<)      = Int64.shift_left 
let (>>)      = Int64.shift_right
let (|||)     = Int64.logor
let zero      = Int64.zero


let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

(* State monad. We encapsulate two values as state: a counter n and 
   the output channel io. The counter counts the bytes emitted
   and is used to insert newlines to avoid long lines. The output channel
   is where output goes. This should make it easier to implement emitting
   to a string buffer, or similar changes, in the future.  *)

type ('s, 't) m = 's -> 't * 's
type 'a monad   = (int * out_channel, 'a) m

let return x        = fun s -> (x, s)
let (>>=) m f       = fun r -> let (x, s) = m r in (f x) s
let update f        = fun s -> ((), f s)

let get_outchan     = fun (_,io as s) -> (io, s)
let get_counter     = fun (n,_  as s) -> ( n, s)
let advance m       = update @@ fun (n, io) -> (n+m, io)

(** A file is represented as a sequence of 4-byte words and a last word
    that may contain 0 .. 3 bytes.  The last word is padded with zeros in
    the least significant bytes of the word and indicates the number of
    significant bytes (0 .. 3) *)
type word = 
  | Word of Int64.t       (** 4 bytes *)
  | Last of int * Int64.t (** n bytes, n = 0, 1, 2, 3 *)

(** read a character from [io] and return [None] for end of file *)
let input_char io = try Some (input_char io) with End_of_file -> None

(** [read_word io] reads the next 4-byte word from [io] *)
let read_word io = 
  let rec loop word = function
    | 4 ->	Word(word) (* done with reading 4 bytes *)
    | n ->	( match input_char io with
        | Some c -> 
          let word' = (word << 8) ||| Int64.of_int (int_of_char c)
          in
          loop word' (n+1)
        | None -> Last(n, word << 8*(4-n))
      )
  in
  loop Int64.zero 0 |> return


(** emit newline after [line_length] bytes of output *)
let newline =
  get_counter >>= fun n ->
  if n > 0 && n mod line_length = 0 then 
    get_outchan >>= fun oc -> return @@ output_string oc "\n"
  else
    return ()

(** emit string *)
let emit_str str =
  get_outchan >>= fun oc ->
  newline >>= fun () ->
  advance (String.length str) >>= fun () ->
  output_string oc str |> return

(** emit a base-85 digit (a single character) *)
let offset = Char.code('!') 
let emit_digit d = 
  let chr = d |> Int64.to_int |> (+) offset |> Char.chr in 
  get_outchan     >>= fun oc ->
  newline         >>= fun () ->
  advance 1       >>= fun () ->
  return (output_char oc chr)

(** emit a 'z' as a special case representing a 32-bit zero *)
let emit_zero =
  get_outchan     >>= fun oc ->
  newline         >>= fun () ->
  advance 1       >>= fun () ->
  return (output_char oc 'z')

let zero       = Int64.zero
let base85     = Int64.of_int 85
let div85 word = Int64.div word base85
let rem85 word = Int64.rem word base85

(** [emit_word word len skip] encodes a 4-byte word into len=5 Ascii
    characters and emits them to stdout. The parameter [skip] denotes the
    number of least significant Ascii characters that should be omitted.
    Except when emitting the last word, [skip] is zero. *)
let rec emit_word word len skip = match len, skip with
  | 0, _  ->  return ()
  | n, 0  ->  let word'   = div85 word in
    let digit   = rem85 word in
    emit_word word' (n-1) 0 >>= fun () -> emit_digit digit
  | n, s  ->  emit_word (div85 word) (n-1) (s-1) (* skip *)

(** [emit85 word] emits a word in Ascii85 encoding *)
let emit85 = function
  | Word(z) when z = zero -> emit_zero 
  | Word(word)		    -> emit_word word 5 0       (* regular case *)
  | Last(1,word)		    -> emit_word word 5 3 
  | Last(2,word)		    -> emit_word word 5 2
  | Last(3,word)		    -> emit_word word 5 1
  | Last(_,word)		    -> assert false

(** [encode85 head io] reads the file [io] in 4-byte words and emits it. 
    The string [head] is emitted ahead of the encoded contents of [io].
    This function doesn't open or close the file. *)

let encode85 head io =
  let rec loop = function
    | Last(0,word)		-> return ()
    | Last(n,word) as x	-> emit85 x
    | Word(word)   as x	-> emit85 x     >>= fun () 
      -> read_word io >>= fun w
      -> loop w
  in                     
  emit_str head >>= fun () ->
  read_word io  >>= fun w  -> loop w >>= fun () ->
  emit_str "~>\n"

(** [encode head ic oc] encodes input file [ic] to output file [oc]. 
    String [head] is emitted first.  *)
let encode head ic oc = encode85 head ic (0, oc) |> ignore

(** [encode_file] reads bytes from a named file and emits them in the
    Ascii85 encoding to stdout. The output is prepended with [head].
    [encode_file] opens and closes the named file.  *)

let encode_file head path =
  let ic = open_in path in
  try_finalize (encode head ic) stdout close_in ic


