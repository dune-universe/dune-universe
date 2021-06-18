type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  mutable buffer : bigstring;
  mutable pos : int;
  mutable len : int;
  init : bigstring;
}

module View = struct
  type t = { buffer : bigstring; pos : int; len : int; continue : int -> unit }

  let make buffer ~pos ~len continue = { buffer; pos; len; continue }
end

let create size =
  let buffer = Base_bigstring.create size in
  { buffer; pos = 0; len = size; init = buffer }

let of_bigstring buffer =
  { buffer; pos = 0; len = Base_bigstring.length buffer; init = buffer }

let contents buf = Base_bigstring.sub buf.buffer ~pos:0 ~len:buf.pos

let contents_string buf =
  Base_bigstring.To_string.sub buf.buffer ~pos:0 ~len:buf.pos

let length buf = buf.pos
let clear buf = buf.pos <- 0
let capacity buf = buf.len

let reset buf =
  buf.pos <- 0;
  buf.buffer <- buf.init;
  buf.len <- Base_bigstring.length buf.buffer

let resize buf size =
  let new_len = (buf.len + size) * 2 in
  let new_buffer = Base_bigstring.create new_len in
  Base_bigstring.blit ~src:buf.buffer ~src_pos:0 ~dst:new_buffer ~dst_pos:0
    ~len:buf.len;
  buf.buffer <- new_buffer;
  buf.len <- new_len

let add_char buf c =
  let pos = buf.pos in
  if pos >= buf.len then resize buf 1;
  Base_bigstring.set buf.buffer pos c;
  buf.pos <- pos + 1

let add_string buf s =
  let len = String.length s in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Base_bigstring.From_string.blit ~src:s ~src_pos:0 ~dst:buf.buffer
    ~dst_pos:buf.pos ~len;
  buf.pos <- new_pos

let fill t =
  if t.pos >= t.len then resize t 0;
  View.make t.buffer ~pos:t.pos
    ~len:(Base_bigstring.length t.buffer - t.pos)
    (fun count -> t.pos <- t.pos + count)

let add_bigstring buf ?pos ?len s =
  let buf_len = Base_bigstring.length s in
  let pos = Option.value pos ~default:0 in
  if pos < 0 || pos > buf_len then
    invalid_arg
      (Printf.sprintf
         "Bytebuffer.add_bigstring: Invalid pos %d. Buffer length: %d" pos
         buf_len);
  let len = Option.value len ~default:(buf_len - pos) in
  if len < 0 || pos + len > buf_len then
    invalid_arg
      (Printf.sprintf
         "Bytebuffer.add_bigstring: Invalid len %d. offset: %d, buffer_length: \
          %d, requested_length: %d"
         len pos buf_len (pos + len));
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Base_bigstring.unsafe_blit ~src:s ~src_pos:pos ~dst:buf.buffer
    ~dst_pos:buf.pos ~len;
  buf.pos <- new_pos

let unsafe_index ?pos ?len ch t =
  let pos = Option.value pos ~default:0 in
  if pos < 0 || pos > length t then
    invalid_arg (Printf.sprintf "Bytebuffer.index: Invalid pos %d" pos);
  let len = Option.value len ~default:(length t - pos) in
  if len < 0 || pos + len > length t then
    invalid_arg (Printf.sprintf "Bytebuffer.index: Invalid len %d" len);
  Base_bigstring.unsafe_find t.buffer ~pos ~len ch

let index ?pos ?len ch t =
  let idx = unsafe_index ?pos ?len ch t in
  if idx < 0 then None else Some idx

let consume t =
  View.make t.buffer ~pos:0 ~len:t.pos (fun count ->
      if count < 0 || count > t.pos then
        invalid_arg
          "Bytebuffer.consume: Invalid response for bytes consumed in buffer.";
      Base_bigstring.blit ~src:t.buffer ~src_pos:count ~dst_pos:0
        ~len:(length t - count)
        ~dst:t.buffer;
      t.pos <- t.pos - count)

let drop t n =
  if n < 0 || n > t.pos then invalid_arg "Bytebuffer.drop: index out of bounds";
  Base_bigstring.blit ~src:t.buffer ~src_pos:n ~dst_pos:0
    ~len:(length t - n)
    ~dst:t.buffer;
  t.pos <- t.pos - n

let addf t fmt = Format.kasprintf (add_string t) fmt
