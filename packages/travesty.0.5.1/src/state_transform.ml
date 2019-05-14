(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
include State_transform_intf

module Make2 (M : Monad.S) : S2 with module Inner = M = struct
  module Inner = M

  module T = struct
    type ('a, 's) t = 's -> ('s * 'a) Inner.t

    include Monad.Make2 (struct
      type nonrec ('a, 's) t = ('a, 's) t

      let map' wc ~f state =
        let open Inner.Let_syntax in
        let%map state', a = wc state in
        (state', f a)

      let map = `Custom map'

      let bind wc ~f state =
        let open Inner.Let_syntax in
        let%bind state', a = wc state in
        (f a) state'

      let return a state = Inner.return (state, a)
    end)
  end

  include T

  let run' f ctx = f ctx

  let run f ctx = f ctx |> Inner.map ~f:snd

  module Monadic = struct
    let make = Fn.id

    let peek f ctx =
      let open Inner.Let_syntax in
      let%map v = f ctx in
      (ctx, v)

    let modify f ctx =
      let open Inner.Let_syntax in
      let%map ctx' = f ctx in
      (ctx', ())

    let return (x : 'a Inner.t) ctx =
      Inner.Let_syntax.(
        let%map x' = x in
        (ctx, x'))
  end

  let make f = Monadic.make (Fn.compose Inner.return f)

  let peek f = Monadic.peek (Fn.compose Inner.return f)

  let modify f = Monadic.modify (Fn.compose Inner.return f)

  let fix ~f init =
    let rec mu a ctx =
      let mu_monad x = Monadic.make (mu x) in
      let f' =
        Let_syntax.(
          let%bind a' = f mu_monad a in
          let%map ctx' = peek Fn.id in
          (ctx', a'))
      in
      run f' ctx
    in
    Monadic.make (mu init)
end

module To_S (M : S2) (B : Base.T) :
  S
  with type state = B.t
   and type 'a t = ('a, B.t) M.t
   and module Inner = M.Inner = struct
  type state = B.t

  module M1 = struct
    type 'a t = ('a, state) M.t

    include Monad_exts.S2_to_S (M) (B)
  end

  include M1
  include Monad_exts.Extend (M1)

  let lift = M.return

  include (
    M :
      Generic
      with type ('a, 's) t := 'a t
       and type 's state := state
       and module Inner = M.Inner )
end

module Make (B : Basic) :
  S with type state = B.t and module Inner = B.Inner =
  To_S (Make2 (B.Inner)) (B)
