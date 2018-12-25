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

module type Extensions1 = sig
  type 'a t
  val max_measure : measure:('a -> int) -> ?default:int -> 'a t -> int
  val any : 'a -> predicates:('a -> bool) t -> bool
  val all : 'a -> predicates:('a -> bool) t -> bool
  val none : 'a -> predicates:('a -> bool) t -> bool
  val at_most_one : 'a t -> 'a option Or_error.t
  val one : 'a t -> 'a Or_error.t
  val two : 'a t -> ('a * 'a) Or_error.t
end

module Extend1 (C : Container.S1)
  : Extensions1 with type 'a t := 'a C.t = struct

  let max_measure ~measure ?(default=0) xs =
    xs
    |> C.max_elt ~compare:(T_fn.on measure Int.compare)
    |> Option.value_map ~f:measure ~default:default

  let too_few_error () =
    Error.of_string "Expected one element; got none"
  ;;

  let too_many_error _a =
    Container.Continue_or_stop.Stop
      ( Or_error.error_string
          "Expected one element; got too many"
      )
  ;;

  let at_most_one xs =
    C.fold_until xs
      ~init:`None_yet
      ~f:(function
          | `None_yet -> fun x -> Continue (`One x)
          | `One _    -> too_many_error

        )
      ~finish:(function
          | `None_yet -> Ok None
          | `One x    -> Ok (Some x)
        )
  ;;

  let one xs =
    Or_error.(
      xs
      |> at_most_one
      >>= Result.of_option ~error:(too_few_error ())
    )
  ;;

  let two xs =
    C.fold_until xs
      ~init:`None_yet
      ~f:(function
          | `None_yet -> fun x -> Continue (`One x)
          | `One x    -> fun y -> Continue (`Two (x, y))
          | `Two _    -> too_many_error
        )
      ~finish:(function
          | `None_yet | `One _ -> Result.Error (too_few_error ())
          | `Two (x, y)        -> Ok (x, y)
        )
  ;;

  let any  x ~predicates = C.exists predicates ~f:(fun p -> p x)
  let all  x ~predicates = C.for_all predicates ~f:(fun p -> p x)
  let none x ~predicates = not (any x ~predicates)
end
