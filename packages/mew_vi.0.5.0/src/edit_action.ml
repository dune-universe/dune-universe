(*
 * edit_action.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


type t=
  | Dummy
  | Bypass of Key.t list
  | Vi of Vi_action.t list

