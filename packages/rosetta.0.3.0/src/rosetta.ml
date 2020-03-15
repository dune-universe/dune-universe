type yuscii_encoding = [`UTF_7]

type encoding = [yuscii_encoding | Uuuu.encoding | Coin.encoding]

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let try_or f g x = try f x with _ -> g x

let ( <.> ) f g x = f (g x)

let cast x = (x :> encoding)

let yuscii_encoding_of_string = function
  | "UTF-7" | "csUTF7" -> `UTF_7
  | s -> invalid_arg "Invalid_character-sets: %s" s

let encoding_of_string =
  try_or (cast <.> yuscii_encoding_of_string)
  @@ try_or (cast <.> Uuuu.encoding_of_string)
  @@ try_or (cast <.> Coin.encoding_of_string)
  @@ invalid_arg "Invalid_character-sets: %s"

let encoding_to_string = function
  | #Uuuu.encoding as encoding -> Uuuu.encoding_to_string encoding
  | #Coin.encoding as encoding -> Coin.encoding_to_string encoding
  | `UTF_7 -> "UTF-7"

type ('kind, 'decoder) tag =
  | UTF_7 : ([> yuscii_encoding], Yuscii.decoder) tag
  | ISO8859 : ([> Uuuu.encoding], 'kind Uuuu.decoder) tag
  | KOI8 : ([> Coin.encoding], 'kind Coin.decoder) tag

type 'kind pack =
  | V : ('kind, 'decoder) tag * 'decoder -> 'kind pack [@unboxed]

type src = [`Channel of in_channel | `String of string | `Manual]

type decode = [`Await | `End | `Uchar of Uchar.t | `Malformed of string]

type 'kind decoder =
  {src: src; pack: 'kind pack}
  constraint 'kind = [< encoding]

let src {pack= V (kind, decoder); _} source off len =
  match kind with
  | UTF_7 -> Yuscii.src decoder source off len
  | ISO8859 -> Uuuu.src decoder source off len
  | KOI8 -> Coin.src decoder source off len

let decode {pack= V (kind, decoder); _} =
  match kind with
  | UTF_7 -> Yuscii.decode decoder
  | ISO8859 -> Uuuu.decode decoder
  | KOI8 -> Coin.decode decoder

let decoder : ([< encoding] as 'kind) -> src -> 'kind decoder =
 fun kind src ->
  match kind with
  | #Uuuu.encoding as k -> {src; pack= V (ISO8859, Uuuu.decoder k src)}
  | #Coin.encoding as k -> {src; pack= V (KOI8, Coin.decoder k src)}
  | #yuscii_encoding -> {src; pack= V (UTF_7, Yuscii.decoder src)}

let decoder_byte_count {pack= V (kind, decoder); _} =
  match kind with
  | UTF_7 -> Yuscii.decoder_byte_count decoder
  | ISO8859 -> Uuuu.decoder_byte_count decoder
  | KOI8 -> Coin.decoder_byte_count decoder

let decoder_src {src; _} = src

let decoder_kind {pack= V (kind, decoder); _} =
  match kind with
  | UTF_7 -> `UTF_7
  | ISO8859 -> (Uuuu.decoder_kind decoder :> encoding)
  | KOI8 -> (Coin.decoder_kind decoder :> encoding)

module String = struct
  type 'a folder =
    'a -> int -> [`Malformed of string | `Uchar of Uchar.t] -> 'a

  let fold kind ?off ?len folder acc str =
    let off, len =
      match (off, len) with
      | Some off, Some len -> (off, len)
      | None, Some len -> (0, len)
      | Some off, None -> (off, String.length str - off)
      | None, None -> (0, String.length str)
    in
    let acc = ref acc in
    let decoder = decoder kind (`String (String.sub str off len)) in
    let rec go decoder =
      match decode decoder with
      | (`Uchar _ | `Malformed _) as res ->
          acc := folder !acc (decoder_byte_count decoder) res ;
          go decoder
      | `End -> !acc
      | `Await -> assert false
    in
    go decoder
end
