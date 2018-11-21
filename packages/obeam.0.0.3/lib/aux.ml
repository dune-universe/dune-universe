(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let alignment align_bytes total_bytes =
  (total_bytes + (align_bytes-1)) / align_bytes * align_bytes

let padding align_bytes total_bytes =
  (alignment align_bytes total_bytes) - total_bytes

let skip_padding buf padding_bits =
  match%bitstring buf with
  | {| _    : padding_bits
     ; rest : -1 : bitstring
     |} ->
     rest
  | {| _ |} ->
     buf

(* size: bytes *)
let content_size_bytes size excluded_fields =
  let i32 = Int32.of_int in
  let ( - ) = Int32.sub in
  let ( * ) = Int32.mul in
  (size - (i32 excluded_fields * i32 4)) |> Int32.to_int

(* size: bytes *)
let content_size_bits size excluded_fields =
  (content_size_bytes size excluded_fields) * 8

let skip_padding_for_size size excluded_fields buf =
  let pad = content_size_bytes size excluded_fields |> padding 4 in
  skip_padding buf (pad*8)

let eol (value, buffer) =
  match Bitstring.bitstring_length buffer with
  | 0 ->
     Ok (value, buffer)
  | _ ->
     Error ("eol / not empty", buffer)

module Z = struct
  (* for ppx_deriving.show *)
  let pp fmt z = Format.fprintf fmt "%s" (Z.to_string z)
  let show z = Z.to_string z

  (* See: http://erlang.org/doc/apps/erts/erl_ext_dist.html#small_big_ext *)
  let of_bitstring ds =
    let b = Z.of_int 256 in
    let rec fold_digits acc n ds =
      match%bitstring ds with
      | {| d : 1*8 : unsigned
         ; ds' : -1 : bitstring
         |} ->
         let acc' = Z.add acc (Z.mul (Z.of_int d) (Z.pow b n)) in
         fold_digits acc' (n + 1) ds'
      | {| _ |} -> acc
    in
    fold_digits Z.zero 0 ds

  include Z
end
