(*
 * modal.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


module Key = Key
module Name = Mode.Name
module Mode = Mew.Mode.Make(Key)(Name)

