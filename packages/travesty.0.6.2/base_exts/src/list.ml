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
open Travesty
module Zip = Travesty_containers.Zipper.Plain

module TC = Traversable.Make1_container (struct
  include List

  module On_monad (M : Monad.S) = struct
    let map_m xs ~f =
      let open M.Let_syntax in
      let%map xs_final =
        List.fold_left xs ~init:(return []) ~f:(fun state x ->
            let%bind xs' = state in
            let%map x' = f x in
            x' :: xs')
      in
      List.rev xs_final
  end
end)

let replace_out_of_range (xs : 'a list) (at : int) (_zipper : 'a Zip.t) :
    'a Zip.t Or_error.t =
  Or_error.error_s
    [%message
      "Replace failed: index out of range"
        ~insert_at:(at : int)
        ~list_length:(List.length xs : int)]

module With_errors = struct
  include TC.With_errors

  let replace_m (xs : 'a list) (at : int) ~(f : 'a -> 'a option Or_error.t) :
      'a list Or_error.t =
    let open Or_error.Let_syntax in
    let z_init = Zip.of_list xs in
    let%bind z_move =
      Zip.On_error.step_m z_init ~steps:at
        ~on_empty:(replace_out_of_range xs at)
    in
    let%map z_repl =
      Zip.On_error.map_m_head z_move ~f
        ~on_empty:(replace_out_of_range xs at)
    in
    Zip.to_list z_repl
end

include (TC : module type of TC with module With_errors := With_errors)

include Filter_mappable.Make1 (struct
  type 'a t = 'a list

  let filter_map = List.filter_map
end)

let replace (xs : 'a list) (at : int) ~(f : 'a -> 'a option) :
    'a list Or_error.t =
  With_errors.replace_m xs at ~f:(Fn.compose Or_error.return f)

let prefixes xs = List.mapi ~f:(fun i _ -> List.take xs (i + 1)) xs

let insert (xs : 'a list) (at : int) (value : 'a) : 'a list Or_error.t =
  let open Or_error.Let_syntax in
  let z_init = Zip.of_list xs in
  let%map z_move =
    Zip.On_error.step_m z_init ~steps:at ~on_empty:(fun _ ->
        Or_error.error_s
          [%message
            "Insert failed: index out of range" ~here:[%here]
              ~insert_at:(at : int)
              ~list_length:(List.length xs : int)])
  in
  let z_ins = Zip.push z_move ~value in
  Zip.to_list z_ins
