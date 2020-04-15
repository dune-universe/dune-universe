(*
 * modal.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew.
 *)

module type S = sig
  module Key : Key.S
  module Name : Mode.Name
  module Mode : module type of Mode.Make(Key)(Name)
end

