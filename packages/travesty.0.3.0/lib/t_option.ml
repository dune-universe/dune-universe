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

type 'a t = 'a option

include Traversable.Extend_container1 (struct
    include Option

    module On_monad (M : Monad.S) = struct
      let map_m xo ~f =
        let open M.Let_syntax in
        Option.fold xo
          ~init:(return None)
          ~f:(fun state x ->
              let%bind _ = state in
              let%map  x' = f x in
              Some x')
      ;;
    end
  end)
;;

let%expect_test "generated option map behaves properly: Some" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (map ~f:(fun x -> x * x) (Some 12) : int option)];
  [%expect {| (144) |}]
;;

let%expect_test "generated option map behaves properly: None" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (map ~f:(fun x -> x * x) None : int option)];
  [%expect {| () |}]
;;

let%expect_test "generated option count behaves properly: Some/yes" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (count ~f:Int.is_positive (Some 42) : int)];
  [%expect {| 1 |}]
;;

let%expect_test "generated option count behaves properly: Some/no" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (count ~f:Int.is_positive (Some (-42)) : int)];
  [%expect {| 0 |}]
;;

let%expect_test "map_m: returning identity on Some/Some" =
  let module M = On_monad (Option) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (M.map_m ~f:(Option.some) (Some "hello") : string option option)];
  [%expect {| ((hello)) |}]
;;

include Filter_mappable.Make1 (struct
    type 'a t = 'a option
    let filter_map = Option.bind
  end)
;;

let%expect_test "exclude: Some -> None" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (exclude (Some 9) ~f:Int.is_positive : int option)];
  [%expect {| () |}]
;;

let%expect_test "exclude: Some -> Some" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (exclude (Some 0) ~f:Int.is_positive : int option)];
  [%expect {| (0) |}]
;;

let first_some_of_thunks thunks =
  List.fold_until thunks
    ~init:()
    ~f:(fun () thunk ->
        Option.value_map (thunk ())
          ~default:(Container.Continue_or_stop.Continue ())
          ~f:(fun x -> Stop (Some x)))
    ~finish:(Fn.const None)
;;

let%expect_test "first_some_of_thunks: short-circuiting works" =
  Sexp.output_hum Out_channel.stdout
    [%sexp
      (first_some_of_thunks
         [ Fn.const None
         ; Fn.const (Some "hello")
         ; Fn.const (Some "world")
         ; (fun () -> failwith "this shouldn't happen")
         ] : string option)];
  [%expect {| (hello) |}]
;;
