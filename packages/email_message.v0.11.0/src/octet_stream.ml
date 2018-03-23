module Stable = struct
  open Core.Core_stable
  module Encoding = struct
    module V1 = struct
      type t =
        [ `Base64
        | `Bit7
        | `Bit8
        | `Binary
        | `Quoted_printable
        | `Unknown of string
        ] [@@deriving sexp, bin_io]
    end
  end
  module V1 = struct
    type t =
      { encoding : Encoding.V1.t
      ; content  : Bigstring_shared.Stable.V1.t
      } [@@deriving sexp, bin_io]
  end
end

open Core

module Encoding = struct
  (** Text or binary are the type of the plaintext. For Base64, if the mode is
      text, '\n' is turned into '\r\n' when encoding, and viceversa. *)
  type known =
    [ `Base64
    | `Bit7
    | `Bit8
    | `Binary
    | `Quoted_printable
    ] [@@deriving sexp_of, compare, hash]
  type t = (* Stable.Encoding.V1.t = *)
    [ known
    | `Unknown of string
    ] [@@deriving sexp_of, compare, hash]

  let of_string encoding =
    match encoding |> String.strip |> String.lowercase with
    | "base64"           -> `Base64
    | "7bit"             -> `Bit7
    | "8bit"             -> `Bit8
    | "binary"           -> `Binary
    | "quoted-printable" -> `Quoted_printable
    | unknown            -> `Unknown unknown

  let to_string = function
    | `Base64 -> "base64"
    | `Bit7 -> "7bit"
    | `Bit8 -> "8bit"
    | `Binary -> "binary"
    | `Quoted_printable -> "quoted-printable"
    | `Unknown unknown -> unknown

  let default  = `Bit7
  let default' = `Bit7

  let of_headers ?(ignore_base64_for_multipart = true) headers =
    Headers.last headers "content-transfer-encoding"
    |> Option.map ~f:of_string
    |> function
    | Some `Base64 when ignore_base64_for_multipart ->
      let is_multipart =
        match Media_type.last headers with
        | Some media_type -> Media_type.is_multipart media_type
        | None -> false
      in
      if is_multipart
      then Some default
      else Some `Base64
    | _ as encoding -> encoding

  let of_headers_or_default ?ignore_base64_for_multipart headers =
    match of_headers ?ignore_base64_for_multipart headers with
    | Some t -> t
    | None   -> default
end

type t = Stable.V1.t =
  { encoding : Encoding.t
  ; content  : Bigstring_shared.t
  } [@@deriving sexp_of, compare, hash]

let encoding t = t.encoding
let encoded_contents t = t.content
let encoded_contents_string t = Bigstring_shared.to_string (encoded_contents t)

let of_bigstring_shared ~encoding content = { encoding; content }

let of_string ~encoding str =
  of_bigstring_shared ~encoding (Bigstring_shared.of_string str)

let empty = of_bigstring_shared ~encoding:Encoding.default Bigstring_shared.empty

module Identity = struct
  let encode bstr = bstr
  let decode bstr = bstr
end

module Base64 = struct
  include Base64.Make(struct
      let char62 = '+'
      let char63 = '/'
      let pad_char = '='
      let pad_when_encoding = true
      (* Permissive - ignore anything that would be an invalid base64 character... *)
      let ignore_char = function
        | '0'..'9' | 'a'..'z' | 'A'..'Z' | '+' | '/' | '=' -> false
        | _ -> true
    end)

  let decode bstr =
    bstr
    |> Bigstring_shared.to_string
    |> decode
    (* Ignore unconsumed data *)
    |> fst
    |> Bigstring_shared.of_string

  let split ~len =
    let rec go acc bstr =
      if Bigstring_shared.length bstr <= len then
        List.rev (bstr :: acc)
      else
        go (Bigstring_shared.sub bstr ~len :: acc) (Bigstring_shared.sub bstr ~pos:len)
    in
    go []

  let encoded_line_length = 76
  let decoded_block_length = encoded_line_length / 4 * 3

  let encode bstr =
    split ~len:decoded_block_length bstr
    |> List.map ~f:(fun bstr ->
      Bigstring_shared.to_string bstr
      |> encode)
    |> String_monoid.concat_string ~sep:"\n"
    |> Bigstring_shared.of_string_monoid
  ;;
end

module Quoted_printable = struct
  let decode bstr =
    (* The RFC2045 says that newlines can be converted to the platforms native
       format, so that's what we'll do. It's the same for both binary data and
       text data. If a CRLF sequence appears in the decoded data, that's because
       it was encoded as =0D=0A, which means the characters shouldn't be
       interpreted as EOL.  *)
    let bigbuffer, _ =
      Quoted_printable_lexer.decode_quoted_printable
        (Bigstring_shared.length bstr) (Bigstring_shared.to_lexbuf bstr)
    in
    Bigstring_shared.of_bigbuffer_volatile bigbuffer
  ;;

  let encode bstr =
    let bigbuffer =
      Quoted_printable_lexer.encode_quoted_printable
        (Bigstring_shared.length bstr)
        (Bigstring_shared.to_lexbuf bstr)
    in
    Bigstring_shared.of_bigbuffer_volatile bigbuffer
  ;;
end

let decode t =
  match t.encoding with
  | `Base64           -> Some (Base64.decode t.content)
  | `Quoted_printable -> Some (Quoted_printable.decode t.content)
  | `Bit7             -> Some (Identity.decode t.content)
  | `Bit8             -> Some (Identity.decode t.content)
  | `Binary           -> Some (Identity.decode t.content)
  | `Unknown _        -> None

let encode ~encoding bstr =
  let bstr =
    match encoding with
    | `Base64           -> Base64.encode bstr
    | `Quoted_printable -> Quoted_printable.encode bstr
    | `Bit7             -> Identity.encode bstr
    | `Bit8             -> Identity.encode bstr
    | `Binary           -> Identity.encode bstr
  in
  let encoding = (encoding :> Encoding.t) in
  of_bigstring_shared ~encoding bstr
