(*
 * Copyright (c) 2015 Thomas Leonard <talex5@gmail.com>
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

open Result

module type FRAME_MSG = sig
  type error
  type t
  val flags : t -> Flags.t
  val size : t -> (int, error) result
end

module Make(F : FRAME_MSG) = struct
  type fragment = {
    size : int;
    msg : F.t;
  }

  type frame = {
    total_size : int;
    fragments : fragment list;
  }

  let to_fragments =
    let rec aux acc = function
      | [] -> Ok (List.rev acc)
      | msg :: rest ->
          match F.size msg with
          | Error _ as e -> e
          | Ok size ->
              assert (size > 0 && size <= Io_page.page_size);
              aux ({size; msg} :: acc) rest in
    aux []

  let make_frame ~first ~rest =
    match F.size first with
    | Error e -> Error (e, (first :: rest))
    | Ok total_size ->
    match to_fragments rest with
    | Error e -> Error (e, (first :: rest))
    | Ok rest ->
    let first_size = List.fold_left (fun acc f -> acc - f.size) total_size rest in
    assert (first_size >= 0 && first_size <= Io_page.page_size);
    let first = { size = first_size; msg = first } in
    Ok {
      total_size;
      fragments = first :: rest;
    }

  (* Convert a list of requests into a list of frames.
     If any fragment contains an error, then the whole frame is an error. *)
  let group_frames =
    let more_after r = Flags.(mem more_data) (F.flags r) in
    let rec collect_frags acc = function
      | [] -> failwith "Expecting more data, but no more requests!"
      | r :: rs when more_after r -> collect_frags (r :: acc) rs
      | r :: rs -> List.rev (r :: acc), rs in
    let rec collect_frames acc = function
      | r :: rs when more_after r ->
          let frags, rs = collect_frags [] rs in
          let frame = make_frame ~first:r ~rest:frags in
          collect_frames (frame :: acc) rs
      | r :: rs ->
          let frame = make_frame ~first:r ~rest:[] in
          collect_frames (frame :: acc) rs
      | [] -> List.rev acc in
    collect_frames []
end
