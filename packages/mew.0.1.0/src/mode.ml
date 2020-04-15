(*
 * mode.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew.
 *)

module type Name = sig
  type t
  val compare : t -> t -> int
end

module Make(Key:Key.S) (Name:Name) = struct
  module KeyTrie = Trie.Make(Key)

  type name= Name.t

  type action=
    | Switch of name
    | Key of Key.t
    | KeySeq of Key.t Queue.t
    | Custom of (unit -> unit)

  type t= {
    name: name;
    timeout: float option;
    bindings: action KeyTrie.node;
  }

  module Modes = Map.Make(Name)
  type modes= t Modes.t

  let name m= m.name
  let timeout m= m.timeout
  let bindings m= m.bindings
  let compare m1 m2= compare m1.name m2.name

  let default_mode modes= Modes.bindings modes |> List.hd

  let bind mode keyseq action= KeyTrie.set mode.bindings keyseq action
  let unbind mode keyseq= KeyTrie.unset mode.bindings keyseq
end

