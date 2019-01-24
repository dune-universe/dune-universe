(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. %%NAME%% %%VERSLwtN%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

type t =
  [ `Nil
  | `Integer of int64
  | `String of string
  | `Error of string
  | `Bulk of string
  | `Array of t array ]

type lexeme =
  [ `Nil
  | `Integer of int64
  | `String of string
  | `Error of string
  | `Bs of int
  | `As of int ]

type error =
  [ `Msg of string
  | `Unexpected of char
  | `Invalid_value
  | `Invalid_encoder ]

let pp_error fmt = function
  | `Msg s ->
    Format.fprintf fmt "%s" s
  | `Invalid_value ->
    Format.fprintf fmt "invalid value"
  | `Unexpected c ->
    Format.fprintf fmt "unexpected input: (%d)" (int_of_char c)
  | `Invalid_encoder ->
    Format.fprintf fmt "invalid encoder"

let string_of_error x =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_error fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

exception Exc of error

let unwrap = function
  | Ok x ->
    x
  | Error e ->
    raise (Exc e)

module type INPUT = sig
  type ic

  val read : ic -> int -> string Lwt.t
  val read_line : ic -> string Lwt.t
  val read_char : ic -> char Lwt.t
end

module type OUTPUT = sig
  type oc

  val write : oc -> string -> unit Lwt.t
end

module type READER = sig
  include INPUT

  val discard_sep : ic -> unit Lwt.t
  val read_lexeme : ic -> (lexeme, error) result Lwt.t
  val decode : ic -> lexeme -> t Lwt.t
end

module type WRITER = sig
  include OUTPUT

  val write_sep : oc -> unit Lwt.t
  val write_lexeme : oc -> lexeme -> unit Lwt.t
  val encode : oc -> t -> unit Lwt.t
end

module type S = sig
  module Reader : READER
  module Writer : WRITER

  val write : Writer.oc -> t -> unit Lwt.t
  val read : Reader.ic -> t Lwt.t
end

module Reader (I : INPUT) = struct
  include I

  let discard_sep ic =
    I.read_char ic >>= fun _ -> I.read_char ic >>= fun _ -> Lwt.return ()

  let read_lexeme ic : (lexeme, error) result Lwt.t =
    I.read_char ic
    >>= function
    | ':' ->
      I.read_line ic
      >>= fun i ->
      let i = Int64.of_string i in
      Lwt.return @@ Ok (`Integer i)
    | '-' ->
      I.read_line ic >>= fun line -> Lwt.return @@ Ok (`Error line)
    | '+' ->
      I.read_line ic >>= fun line -> Lwt.return @@ Ok (`String line)
    | '*' ->
      I.read_line ic
      >>= fun i ->
      let i = int_of_string i in
      if i < 0 then Lwt.return @@ Ok `Nil else Lwt.return @@ Ok (`As i)
    | '$' ->
      I.read_line ic
      >>= fun i ->
      let i = int_of_string i in
      if i < 0 then Lwt.return @@ Ok `Nil else Lwt.return @@ Ok (`Bs i)
    | c ->
      Printf.printf "Bad char: (%d) %c\n" (int_of_char c) c;
      Lwt.return @@ Error (`Unexpected c)

  let rec decode ic : lexeme -> t Lwt.t = function
    | `Nil ->
      Lwt.return `Nil
    | `Integer i ->
      Lwt.return @@ `Integer i
    | `Error e ->
      Lwt.return @@ `Error e
    | `String s ->
      Lwt.return @@ `String s
    | `Bs len ->
      read ic len
      >>= fun b -> discard_sep ic >>= fun () -> Lwt.return @@ `Bulk b
    | `As len ->
      let arr = Array.make len `Nil in
      let rec aux = function
        | 0 ->
          Lwt.return ()
        | n -> (
          read_lexeme ic
          >>= function
          | Ok v ->
            decode ic v
            >>= fun x ->
            arr.(len - n) <- x;
            aux (n - 1)
          | Error err ->
            raise (Exc err) )
      in
      aux len >>= fun () -> Lwt.return @@ `Array arr
end

module Writer (O : OUTPUT) = struct
  include O

  let ( >>= ) = Lwt.( >>= )
  let write_sep oc = O.write oc "\r\n"

  let write_lexeme oc = function
    | `Nil ->
      O.write oc "*-1\r\n"
    | `Error e ->
      O.write oc "-" >>= fun () -> O.write oc e >>= fun () -> write_sep oc
    | `Integer i ->
      O.write oc ":" >>= fun () -> O.write oc (Printf.sprintf "%Ld\r\n" i)
    | `Bs len ->
      O.write oc (Printf.sprintf "$%d\r\n" len)
    | `As len ->
      O.write oc (Printf.sprintf "*%d\r\n" len)
    | `String s ->
      O.write oc "+" >>= fun () -> O.write oc s >>= fun () -> write_sep oc

  let rec encode oc = function
    | `Nil ->
      write_lexeme oc `Nil
    | `Error e ->
      write_lexeme oc (`Error e)
    | `String s ->
      write_lexeme oc (`String s)
    | `Integer i ->
      write_lexeme oc (`Integer i)
    | `Bulk s ->
      let len = String.length s in
      write_lexeme oc (`Bs len)
      >>= fun () -> write oc s >>= fun () -> write_sep oc
    | `Array a ->
      let len = Array.length a in
      let rec write i =
        match i with
        | 0 ->
          Lwt.return ()
        | n ->
          encode oc a.(len - i) >>= fun () -> write (n - 1)
      in
      write_lexeme oc (`As len) >>= fun () -> write len
end

module Make (Reader : READER) (Writer : WRITER) = struct
  module Reader = Reader
  module Writer = Writer

  let ( >>= ) = Lwt.( >>= )
  let decode = Reader.decode

  let read ic =
    Reader.read_lexeme ic
    >>= function
    | Ok l ->
      decode ic l
    | Error e ->
      raise (Exc e)

  let encode = Writer.encode
  let write oc = encode oc
end

module String_writer = Writer (struct
  type oc = string ref

  let write oc s =
    oc := !oc ^ s;
    Lwt.return_unit
end)

module String_reader = Reader (struct
  type ic = string ref

  let read input i =
    Lwt.wrap (fun () ->
        let s = String.sub !input 0 i in
        input := String.sub !input i (String.length !input - i);
        s )

  let read_char input = read input 1 >|= fun c -> c.[0]

  let read_line t =
    let rec aux output =
      read t 1
      >>= function
      | "\n" ->
        Lwt.return output
      | "\r" ->
        aux output
      | c ->
        aux (output ^ c)
    in
    aux ""
end)

module String = Make (String_reader) (String_writer)

let is_nil = function
  | `Nil ->
    true
  | _ ->
    false

let to_string = function
  | `String s ->
    Ok s
  | `Bulk s ->
    Ok s
  | `Error e ->
    Ok e
  | `Nil ->
    Ok "nil"
  | `Integer i ->
    Ok (Int64.to_string i)
  | _ ->
    Error `Invalid_value

let to_string_exn x = to_string x |> unwrap

let to_integer = function
  | `Integer i ->
    Ok i
  | `String s
  | `Bulk s -> (
    try Ok (Int64.of_string s) with _ -> Error `Invalid_value )
  | _ ->
    Error `Invalid_value

let to_integer_exn x = to_integer x |> unwrap

let to_float = function
  | `Integer i ->
    Ok (Int64.to_float i)
  | `String s
  | `Bulk s -> (
    try Ok (float_of_string s) with _ -> Error `Invalid_value )
  | _ ->
    Error `Invalid_value

let to_float_exn x = to_float x |> unwrap

let to_array f = function
  | `Array a ->
    Ok (Array.map f a)
  | _ ->
    Error `Invalid_value

let to_array_exn f x = to_array f x |> unwrap

let of_alist l =
  `Array
    ( Array.of_list
    @@ List.fold_right (fun (k, v) acc -> `String k :: v :: acc) l [] )

let to_alist k v = function
  | `Array a ->
    let len = Array.length a in
    if len mod 2 <> 0 then Error `Invalid_value
    else
      let dest = ref [] in
      Array.iteri
        (fun i x -> if i < len - 1 then dest := (k x, v a.(i + 1)) :: !dest)
        a;
      Ok !dest
  | _ ->
    Error `Invalid_value

let to_alist_exn k v x = to_alist k v x |> unwrap

(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTLwtN OF CONTRACT, NEGLIGENCE
  OR OTHER TORTLwtUS ACTLwtN, ARISING OUT OF OR IN CONNECTLwtN WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
