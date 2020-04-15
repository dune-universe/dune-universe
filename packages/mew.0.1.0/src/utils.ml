(*
 * utils.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew.
 * This module implements strict impure trie tree data structure.
 *)

module Queue = struct
  let rec drop n q=
    if n > 0 then
      (ignore (Queue.take q);
      drop (n-1) q)

  let to_list_rev q= Queue.fold (fun l key-> key::l) [] q
  let to_list q= q |> to_list_rev |> List.rev
end

