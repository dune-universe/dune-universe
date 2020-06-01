(*
 * mode.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


module Name = struct
  type t=
    | Normal
    | Visual
    | Insert
    | Commandline

  let compare= compare
end

include Mew.Mode.Make(Key)(Name)

