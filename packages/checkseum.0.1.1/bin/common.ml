let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len =
  invalid_arg "Invalid bounds (off: %d, len: %d)" off len

let strf = Format.asprintf
let pp = Format.fprintf
let io_buffer_size = 65536

module X : sig
  type adler32 = Checkseum.Adler32.t
  type crc32c = Checkseum.Crc32c.t
  type crc32 = Checkseum.Crc32.t
  type src = [`Channel of in_channel | `Manual | `String of string]
  type 'value r = [`Await | `End of 'value]
  type 'a kind
  type 'value t

  val src : 'value t -> Bytes.t -> int -> int -> unit
  val encoder : 'value kind -> default:'value -> src -> 'value t
  val encode : 'value t -> 'value r
  val adler32 : adler32 kind
  val crc32c : crc32c kind
  val crc32 : crc32c kind
end = struct
  type adler32 = Checkseum.Adler32.t
  type crc32c = Checkseum.Crc32c.t
  type crc32 = Checkseum.Crc32.t

  type 'a kind =
    | Adler32 : adler32 kind
    | Crc32c : crc32c kind
    | Crc32 : crc32 kind

  type src = [`Channel of in_channel | `Manual | `String of string]
  type 'value r = [`Await | `End of 'value]

  type 'value t =
    { src: src
    ; kind: 'value kind
    ; mutable i: Bytes.t
    ; mutable i_off: int
    ; mutable i_pos: int
    ; mutable i_len: int
    ; mutable value: 'value
    ; mutable k: 'value t -> 'value r }

  let end_of_input encoder =
    encoder.i <- Bytes.empty ;
    encoder.i_off <- 0 ;
    encoder.i_pos <- 0 ;
    encoder.i_len <- min_int

  let src encoder source off len =
    if off < 0 || len < 0 || off + len > Bytes.length source then
      invalid_bounds off len
    else if len = 0 then end_of_input encoder
    else (
      encoder.i <- source ;
      encoder.i_off <- off ;
      encoder.i_pos <- 0 ;
      encoder.i_len <- len - 1 )

  let refill k encoder =
    match encoder.src with
    | `Manual ->
        encoder.k <- k ;
        `Await
    | `String _ -> end_of_input encoder ; k encoder
    | `Channel ic ->
        let len = input ic encoder.i 0 (Bytes.length encoder.i) in
        src encoder encoder.i 0 len ;
        k encoder

  let ret k encoder =
    encoder.k <- k ;
    encoder.k encoder

  let r : type a. a kind -> a -> int -> int -> int -> Bytes.t -> a =
   fun kind t off pos rem src ->
    match kind with
    | Adler32 -> Checkseum.Adler32.digest_bytes src (off + pos) rem t
    | Crc32c -> Checkseum.Crc32c.digest_bytes src (off + pos) rem t
    | Crc32 -> Checkseum.Crc32.digest_bytes src (off + pos) rem t

  let i_rem encoder = encoder.i_len - encoder.i_pos + 1

  let rec encode encoder =
    let rem = i_rem encoder in
    if rem <= 0 then
      if rem < 0 then `End encoder.value else refill encode encoder
    else
      let value =
        r encoder.kind encoder.value encoder.i_off encoder.i_pos rem encoder.i
      in
      encoder.i_pos <- encoder.i_pos + rem ;
      encoder.value <- value ;
      ret encode encoder

  let encoder : type a. a kind -> default:a -> src -> a t =
   fun kind ~default src ->
    let i, i_off, i_pos, i_len =
      match src with
      | `Manual -> (Bytes.empty, 0, 1, 0)
      | `Channel _ -> (Bytes.create io_buffer_size, 0, 1, 0)
      | `String s -> (Bytes.unsafe_of_string s, 0, 0, String.length s - 1)
    in
    { src
    ; kind
    ; i
    ; i_off
    ; i_pos
    ; i_len
    ; value= default
    ; k= encode }

  let encode encoder = encoder.k encoder
  let adler32 = Adler32
  let crc32c = Crc32c
  let crc32 = Crc32
end
