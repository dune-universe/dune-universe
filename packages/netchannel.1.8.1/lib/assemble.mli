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

(** Assemble complete network frames from Xen network messages. *)

module type FRAME_MSG = sig
  type error
  type t
  val flags : t -> Flags.t
  val size : t -> (int, error) result
end

module Make(F : FRAME_MSG) : sig
  type fragment = {
    size : int;
    (** The size field in F.t is complicated (the first message contains
        the size of the whole frame, and any size may contain an error). This
        size field is instead simply the size of the fragment. *)

    msg : F.t;
  }

  type frame = {
    total_size : int;
    fragments : fragment list;
  }

  val group_frames : F.t list -> (frame, (F.error * F.t list)) result list
  (** Convert a list of requests into a list of frames.
      If any fragment contains an error, then the whole frame is an error.
      The error value includes all the messages in the frame, so they can
      be released. *)
end

