(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. resp %%VERSLwtN%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
include Resp_intf

let pp_error fmt = function
  | `Msg s -> Format.fprintf fmt "%s" s
  | `Invalid_value -> Format.fprintf fmt "invalid value"
  | `Unexpected c -> Format.fprintf fmt "unexpected input: (%d)" (int_of_char c)
  | `Invalid_encoder -> Format.fprintf fmt "invalid encoder"

let string_of_error x =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_error fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

exception Exc of error

let unwrap = function Ok x -> x | Error e -> raise (Exc e)

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

  let rec read_lexeme ic : (lexeme, error) result Lwt.t =
    I.read_char ic >>= function
    | ':' ->
        I.read_line ic >>= fun i ->
        let i = Int64.of_string i in
        Lwt.return @@ Ok (`Integer i)
    | '-' -> I.read_line ic >>= fun line -> Lwt.return @@ Ok (`Error line)
    | '+' ->
        I.read_line ic >>= fun line -> Lwt.return @@ Ok (`Simple_string line)
    | '*' ->
        I.read_line ic >>= fun i ->
        let i = int_of_string i in
        if i < 0 then Lwt.return @@ Ok `Nil else Lwt.return @@ Ok (`As i)
    | '$' ->
        I.read_line ic >>= fun i ->
        let i = int_of_string i in
        if i < 0 then Lwt.return @@ Ok `Nil else Lwt.return @@ Ok (`Bs i)
    | '\r' -> I.read_char ic >>= fun _ -> read_lexeme ic
    | c -> Lwt.return_error (`Unexpected c)

  let rec decode ic : lexeme -> t Lwt.t = function
    | `Nil -> Lwt.return Nil
    | `Integer i -> Lwt.return @@ Integer i
    | `Error e -> Lwt.return @@ Error e
    | `Simple_string s -> Lwt.return (Simple_string s)
    | `Bs len ->
        if len = 0 then Lwt.return (Bulk (`String ""))
        else read ic len >>= fun b -> Lwt.return @@ Bulk (`String b)
    | `As len ->
        if len = 0 then Lwt.return (Array Seq.empty)
        else
          let rec aux l = function
            | 0 -> Lwt.return l
            | n -> (
                read_lexeme ic >>= function
                | Ok v -> decode ic v >>= fun x -> aux (x :: l) (n - 1)
                | Error err -> raise (Exc err))
          in
          aux [] len >|= fun l -> Array (List.to_seq (List.rev l))
end

module Writer (O : OUTPUT) = struct
  include O

  let ( >>= ) = Lwt.( >>= )

  let write_sep oc = O.write oc "\r\n"

  let write_lexeme oc = function
    | `Nil -> O.write oc "*-1\r\n"
    | `Error e ->
        O.write oc "-" >>= fun () ->
        O.write oc e >>= fun () -> write_sep oc
    | `Integer i ->
        O.write oc ":" >>= fun () -> O.write oc (Printf.sprintf "%Ld\r\n" i)
    | `Bs len -> O.write oc (Printf.sprintf "$%d\r\n" len)
    | `As len -> O.write oc (Printf.sprintf "*%d\r\n" len)
    | `Simple_string s ->
        O.write oc "+" >>= fun () ->
        O.write oc s >>= fun () -> write_sep oc

  let rec encode oc = function
    | Nil -> write_lexeme oc `Nil
    | Error e -> write_lexeme oc (`Error e)
    | Simple_string s -> write_lexeme oc (`Simple_string s)
    | Integer i -> write_lexeme oc (`Integer i)
    | Bulk (`Bytes s) ->
        let len = Bytes.length s in
        write_lexeme oc (`Bs len) >>= fun () ->
        write oc (Bytes.unsafe_to_string s) >>= fun () -> write_sep oc
    | Bulk (`String s) ->
        let len = String.length s in
        write_lexeme oc (`Bs len) >>= fun () ->
        write oc s >>= fun () -> write_sep oc
    | Array seq ->
        let l = List.of_seq seq in
        let len = List.length l in
        let rec write l =
          match l with
          | [] -> Lwt.return ()
          | hd :: tl -> encode oc hd >>= fun () -> write tl
        in
        write_lexeme oc (`As len) >>= fun () -> write l
end

module Make (Reader : READER) (Writer : WRITER) = struct
  module Reader = Reader
  module Writer = Writer

  let ( >>= ) = Lwt.( >>= )

  let decode = Reader.decode

  let read ic =
    Reader.read_lexeme ic >>= function
    | Ok l -> decode ic l
    | Error e -> raise (Exc e)

  let encode = Writer.encode

  let write oc = encode oc
end

let is_nil = function Nil -> true | _ -> false

let to_string = function
  | Simple_string s -> Ok s
  | Bulk (`String s) -> Ok s
  | Bulk (`Bytes b) -> Ok (Bytes.to_string b)
  | Integer i -> Ok (Int64.to_string i)
  | _ -> Error `Invalid_value

let to_string_exn x = to_string x |> unwrap

let to_bytes = function
  | Simple_string s -> Ok (Bytes.of_string s)
  | Bulk (`String s) -> Ok (Bytes.of_string s)
  | Bulk (`Bytes b) -> Ok b
  | _ -> Error `Invalid_value

let to_bytes_exn x = to_bytes x |> unwrap

let to_integer = function
  | Integer i -> Ok i
  | Simple_string s | Bulk (`String s) -> (
      try Ok (Int64.of_string s) with _ -> Error `Invalid_value)
  | _ -> Error `Invalid_value

let to_integer_exn x = to_integer x |> unwrap

let to_float = function
  | Integer i -> Ok (Int64.to_float i)
  | Simple_string s | Bulk (`String s) -> (
      try Ok (float_of_string s) with _ -> Error `Invalid_value)
  | Bulk (`Bytes b) -> (
      try Ok (float_of_string (Bytes.unsafe_to_string b))
      with _ -> Error `Invalid_value)
  | _ -> Error `Invalid_value

let to_float_exn x = to_float x |> unwrap

let to_seq f = function
  | Array a -> Ok (Seq.map f a)
  | _ -> Error `Invalid_value

let to_seq_exn f x = to_seq f x |> unwrap

let to_array f = function
  | Array a -> Ok (Array.of_seq (Seq.map f a))
  | _ -> Error `Invalid_value

let to_array_exn f x = to_array f x |> unwrap

let to_list f = function
  | Array a -> Ok (Seq.map f a |> List.of_seq)
  | _ -> Error `Invalid_value

let to_list_exn f x = to_list f x |> unwrap

let to_alist (a : t -> 'a) (b : t -> 'b) : t -> (('a * 'b) list, error) result =
  function
  | Array seq -> (
      let rec aux seq acc =
        match seq () with
        | Seq.Nil -> failwith "invalid value"
        | Seq.Cons (k, next) -> (
            match next () with
            | Seq.Nil -> failwith "invalid value"
            | Seq.Cons (v, next) -> aux next ((a k, b v) :: acc))
      in
      try Ok (aux seq [])
      with Failure s when s = "invalid value" -> Result.Error `Invalid_value)
  | _ -> Error `Invalid_value

let to_alist_exn k v x = to_alist k v x |> unwrap

let to_hashtbl k v a =
  match to_alist k v a with
  | Ok a ->
      let t = Hashtbl.create (List.length a) in
      List.iter (fun (k, v) -> Hashtbl.replace t k v) a;
      Ok t
  | Error e -> Error e

let to_hashtbl_exn k v x = to_hashtbl k v x |> unwrap

let nil = Nil

let int i = Integer (Int64.of_int i)

let int64 i = Integer i

let float f = Simple_string (Float.to_string f)

let bytes b = Bulk (`Bytes b)

let string s = Bulk (`String s)

let simple_string s = Simple_string s

let list t l = Array (List.to_seq l |> Seq.map t)

let array t a = Array (Array.to_seq a |> Seq.map t)

let seq_cons i s () = Seq.Cons (i, s)

let alist a b l =
  Array
    (List.fold_left
       (fun acc (k, v) -> seq_cons (a k) acc |> seq_cons (b v))
       Seq.empty (List.rev l))

let hashtbl a b ht =
  Array
    (Hashtbl.fold
       (fun k v acc -> seq_cons (a k) acc |> seq_cons (b v))
       ht Seq.empty)

let id x = x

let rec equal a b =
  match (a, b) with
  | Nil, Nil -> true
  | Integer a, Integer b -> Int64.equal a b
  | Simple_string a, Simple_string b -> String.equal a b
  | Bulk (`String a), Bulk (`String b) -> String.equal a b
  | Bulk (`Bytes a), Bulk (`Bytes b) -> Bytes.equal a b
  | Error a, Error b -> String.equal a b
  | Array a, Array b ->
      let rec inner a b =
        match (a (), b ()) with
        | Seq.Nil, Seq.Nil -> true
        | Seq.Nil, _ | _, Seq.Nil -> false
        | Seq.Cons (x, next), Seq.Cons (y, next') ->
            if equal x y then inner next next' else false
      in
      inner a b
  | _, _ -> false

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
        s)

  let read_char input = read input 1 >|= fun c -> c.[0]

  let read_line t =
    let rec aux output =
      read t 1 >>= function
      | "\n" -> Lwt.return output
      | "\r" -> aux output
      | c -> aux (output ^ c)
    in
    aux ""
end)

module String = Make (String_reader) (String_writer)

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
