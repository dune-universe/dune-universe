(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Make (Key : Input.Key) (Value : Input.Value) = struct
  type key = Key.t [@@deriving repr]

  type value = Value.t [@@deriving repr]

  type op = Add of (key * value) | Find of (key * bool) | Mem of (key * bool) | Flush
  [@@deriving repr]

  type t = { out : out_channel }

  let v path =
    let out = open_out path in
    { out }

  let size_of = Repr.(size_of op_t |> unstage)

  let encode_int32 = Repr.(encode_bin int32 |> unstage)

  let decode_int32 = Repr.(decode_bin int32 |> unstage)

  let encode_bin = Repr.(encode_bin op_t |> unstage)

  let decode_bin = Repr.(decode_bin op_t |> unstage)

  let record t op =
    match size_of op with
    | None -> assert false
    | Some size ->
        encode_int32 (Int32.of_int size) (output_string t.out);
        encode_bin op (output_string t.out)

  let close t =
    flush t.out;
    close_out t.out

  let operations path =
    let in_ = open_in path in
    let step () =
      try
        let _, size = decode_int32 (really_input_string in_ 4) 0 in
        let op_repr = really_input_string in_ (Int32.to_int size) in
        let _, op = decode_bin op_repr 0 in
        Some (op, ())
      with End_of_file -> None
    in
    Seq.unfold step ()
end
