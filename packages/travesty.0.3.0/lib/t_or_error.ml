(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel

include Or_error

module On_ok = Traversable.Make_container1 (struct
    type nonrec 'a t = 'a t

    module On_monad (M : Monad.S) = struct
      let map_m err ~f = match err with
        | Ok v -> M.(f v >>| Or_error.return)
        | Error x -> M.return (Error x)
    end
  end)

include T_monad.Extend (Or_error)

let%expect_test "tee_m example" =
  let fail_if_negative x =
    when_m (Int.is_negative x)
      ~f:(fun () -> Or_error.error_string "value is negative!")
  in
  Sexp.output_hum Stdio.stdout
    [%sexp (
      Or_error.(
        42 |> tee_m ~f:fail_if_negative >>| (fun x -> x * x)
      )
      : int t
    )];
  [%expect {| (Ok 1764) |}]
;;
