(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

include Travesty.Traversable.Make1_container (struct
  include Base.Option

  module On (M : Base.Applicative.S) = struct
    let map_m xo ~f =
      match xo with
      | None -> M.return None
      | Some x -> M.(x |> f >>| Base.Option.return)
  end
end)

include Travesty.Filter_mappable.Make1 (struct
  type 'a t = 'a option

  let filter_map = Base.Option.bind
end)

include Travesty.Monad_exts.Extend (Base.Option)

let value_f (o : 'a option) ~(default_f : unit -> 'a) : 'a =
  match o with Some a -> a | None -> default_f ()

let value_l (o : 'a option) ~(default_l : 'a Lazy.t) : 'a =
  match o with Some a -> a | None -> Lazy.force default_l

let first_some_of_thunks thunks =
  Base.List.fold_until thunks ~init:()
    ~f:(fun () thunk ->
      Base.Option.value_map (thunk ())
        ~default:(Base.Container.Continue_or_stop.Continue ()) ~f:(fun x ->
          Stop (Some x) ) )
    ~finish:(Base.Fn.const None)
