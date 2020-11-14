(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
 * Copyright (C) 2016 Docker Inc
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
 *
 *)

type t = Cstruct.t list

let pp_t ppf t =
  List.iter (fun t ->
    Format.fprintf ppf "[%d,%d](%d)" t.Cstruct.off t.Cstruct.len (Bigarray.Array1.dim t.Cstruct.buffer)
  ) t

let len = List.fold_left (fun acc c -> Cstruct.len c + acc) 0

let err fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); invalid_arg (Buffer.contents b) in
  Format.kfprintf k ppf fmt

let rec shift t x =
  if x = 0 then t else match t with
  | [] -> err "Cstructs.shift %a %d" pp_t t x
  | y :: ys ->
    let y' = Cstruct.len y in
    if y' > x
    then Cstruct.shift y x :: ys
    else shift ys (x - y')

let sub t off len =
  let t' = shift t off in
  (* trim the length *)
  let rec trim acc ts remaining = match remaining, ts with
    | 0, _ -> List.rev acc
    | _, [] -> err "invalid bounds in Cstructs.sub %a off=%d len=%d" pp_t t off len
    | n, t :: ts ->
      let to_take = min (Cstruct.len t) n in
      (* either t is consumed and we only need ts, or t has data remaining in which
          case we're finished *)
      trim (Cstruct.sub t 0 to_take :: acc) ts (remaining - to_take) in
  trim [] t' len
