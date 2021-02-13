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

open Base
open Base_quickcheck

module Make0 (I : sig
  val here : Lexing.position
  (** Should point to where the law tests were generated. *)

  type t [@@deriving sexp, compare, quickcheck]

  module Left : sig
    type t [@@deriving quickcheck]
  end

  module Right : sig
    type t [@@deriving quickcheck]
  end

  include
    Travesty.Bi_mappable_types.S0
      with type t := t
       and type left := Left.t
       and type right := Right.t
end) =
struct
  (* Bifunctor laws *)

  let%test_unit "bi_map id id === id" =
    Test.run_exn
      (module I)
      ~f:(fun x ->
        [%test_eq: I.t] ~here:[[%here]; I.here]
          (I.bi_map ~left:Fn.id ~right:Fn.id x)
          x )

  let%test_unit "left id === id" =
    Test.run_exn
      (module I)
      ~f:(fun x ->
        [%test_eq: I.t] ~here:[[%here]; I.here] (I.map_left ~f:Fn.id x) x )

  let%test_unit "right id === id" =
    Test.run_exn
      (module I)
      ~f:(fun x ->
        [%test_eq: I.t] ~here:[[%here]; I.here] (I.map_right ~f:Fn.id x) x )

  let%test_unit "bi_map f g === compose (left f) (right g)" =
    Test.run_exn
      ( module struct
        type t = (I.Left.t -> I.Left.t) * (I.Right.t -> I.Right.t) * I.t
        [@@deriving quickcheck, sexp]
      end )
      ~f:(fun (f, g, x) ->
        [%test_eq: I.t] ~here:[[%here]; I.here]
          (I.bi_map ~left:f ~right:g x)
          (I.map_left ~f (I.map_right ~f:g x)) )

  let%test_unit "bi_map (compose f g) (compose h i) === compose (bimap f h) \
                 (bimap g i)" =
    Test.run_exn
      ( module struct
        type t =
          (I.Left.t -> I.Left.t)
          * (I.Left.t -> I.Left.t)
          * (I.Right.t -> I.Right.t)
          * (I.Right.t -> I.Right.t)
          * I.t
        [@@deriving quickcheck, sexp]
      end )
      ~f:(fun (f, g, h, i, x) ->
        let f_o_g = Fn.compose f g in
        let h_o_i = Fn.compose h i in
        let bfh_o_bgi =
          Fn.compose (I.bi_map ~left:f ~right:h) (I.bi_map ~left:g ~right:i)
        in
        [%test_eq: I.t] ~here:[[%here]; I.here]
          (I.bi_map ~left:f_o_g ~right:h_o_i x)
          (bfh_o_bgi x) )

  let%test_unit "left (compose f g) === compose (left f) (left g)" =
    Test.run_exn
      ( module struct
        type t = (I.Left.t -> I.Left.t) * (I.Left.t -> I.Left.t) * I.t
        [@@deriving quickcheck, sexp]
      end )
      ~f:(fun (f, g, x) ->
        [%test_eq: I.t] ~here:[[%here]; I.here]
          (I.map_left ~f:(Fn.compose f g) x)
          (Fn.compose (I.map_left ~f) (I.map_left ~f:g) x) )

  let%test_unit "right (compose f g) === compose (right f) (right g)" =
    Test.run_exn
      ( module struct
        type t = (I.Right.t -> I.Right.t) * (I.Right.t -> I.Right.t) * I.t
        [@@deriving quickcheck, sexp]
      end )
      ~f:(fun (f, g, x) ->
        [%test_eq: I.t] ~here:[[%here]; I.here]
          (I.map_right ~f:(Fn.compose f g) x)
          (Fn.compose (I.map_right ~f) (I.map_right ~f:g) x) )
end
