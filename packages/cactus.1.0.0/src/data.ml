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

module type K = sig
  type t
  (** The type for keys. *)

  type input_key

  val size : int

  val min : t (* minimal key *)

  val of_input : input_key -> t

  val to_input : t -> input_key

  val empty : t

  val equal : t -> t -> bool
  (** The equality function for keys. *)

  val compare : t -> t -> int

  val set : marker:(unit -> unit) -> bytes -> off:int -> t -> unit

  val get : bytes -> off:int -> t

  val dump : t -> string

  val pp : Format.formatter -> t -> unit
end

module type V = sig
  type t
  (** The type for values. *)

  type input_value

  val size : int

  val of_input : input_value -> t

  val to_input : t -> input_value

  val set : marker:(unit -> unit) -> bytes -> off:int -> t -> unit

  val get : bytes -> off:int -> t

  val pp : Format.formatter -> t -> unit
end

module type Entry = sig
  type input_key

  type input_value

  module Key : K with type input_key = input_key

  module Value : V with type input_value = input_value
end

module Make : functor (InKey : Input.Key) (InValue : Input.Value) ->
  Entry with type input_key = InKey.t and type input_value = InValue.t =
functor
  (InKey : Input.Key)
  (InValue : Input.Value)
  ->
  struct
    type input_key = InKey.t

    type input_value = InValue.t

    module Key = struct
      type t = string

      type input_key = InKey.t

      let size = InKey.encoded_size

      let empty = ""

      let min = Utils.min_key size

      let equal = String.equal

      let compare = String.compare

      let set ~marker buff ~off t =
        marker ();
        Bytes.blit_string t 0 buff off size

      let get buff ~off = Bytes.sub_string buff off size

      let of_input k =
        let ret = InKey.encode k in
        assert (size = String.length ret);
        ret

      let to_input = InKey.decode

      let dump s = s

      open Fmt

      let ascii =
        String.map (fun c ->
            let code = Char.code c in
            if 0x20 <= code && code <= 0x7E then c else '.')

      let pp ppf s = pf ppf "%a" (string |> styled (`Bg `Green) |> styled `Reverse) (ascii s)
    end

    module Value = struct
      type t = string

      type input_value = InValue.t

      let size = InValue.encoded_size

      let set ~marker buff ~off t =
        marker ();
        Bytes.blit_string t 0 buff off size

      let get buff ~off = Bytes.sub_string buff off size

      let of_input v =
        let ret = InValue.encode v in
        assert (size = String.length ret);
        ret

      let to_input = InValue.decode

      open Fmt

      let pp = string |> styled (`Bg `Blue) |> styled `Reverse
    end
  end
