(******************************************************************************)
(* Copyright (c) 2014-2016 Skylable Ltd. <info-copyright@skylable.com>        *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

module Make(Key:Map.OrderedType)(M: Types.Monad) = struct
  module KeyMap = Map.Make(Key)
  type 'a t = {
    mutable map : 'a M.t KeyMap.t
  }
  open! M

  let create () = {
    map = KeyMap.empty
  }

  let find_opt cache key =
    if KeyMap.mem key cache.map then
      Some (KeyMap.find key cache.map)
    else None

  let of_result = function
  | Ok v -> return v
  | Error e -> fail e

  let bind cache key f =
    match find_opt cache key with
    | Some data -> data
    | None ->
        let pending = f key in
        cache.map <- KeyMap.add key pending cache.map;
        (* ensure ordering: first add the key, then remove it *)
        pending >>? fun result ->
        cache.map <- KeyMap.remove key cache.map;
        of_result result
end
