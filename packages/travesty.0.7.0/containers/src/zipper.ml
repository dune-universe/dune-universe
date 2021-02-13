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
open Zipper_types

module type Basic_cell = sig
  type 'a t [@@deriving sexp]

  val make : 'a -> 'a t

  val data : 'a t -> 'a

  val of_data_list : 'a list -> 'a t list

  val to_data_list : 'a t list -> 'a list

  include Travesty.Traversable_types.S1 with type 'a t := 'a t
end

let rev_transfer amount ~src ~dst =
  if Int.(List.length src < amount) then None
  else
    let to_transfer, src' = List.split_n src amount in
    let dst' = List.rev_append to_transfer dst in
    Some (src', dst')

module Make (Cell : Basic_cell) = struct
  type 'a t = {left: 'a Cell.t list; right: 'a Cell.t list}
  [@@deriving fields, sexp]

  let make ~left ~right =
    {left= Cell.of_data_list left; right= Cell.of_data_list right}

  let of_list lst = make ~left:[] ~right:lst

  let left_list zipper = Cell.to_data_list (left zipper)

  let right_list zipper = Cell.to_data_list (right zipper)

  let to_list zipper = List.rev_append (left_list zipper) (right_list zipper)

  let head zipper = List.hd (right zipper)

  let set_head_cell_on_right right new_head =
    match (right, new_head) with
    | [], None -> []
    | [], Some head' -> [head']
    | _ :: rest, None -> rest
    | _ :: rest, Some head' -> head' :: rest

  let set_head_cell zipper new_head =
    {zipper with right= set_head_cell_on_right zipper.right new_head}

  let push zipper ~value =
    {zipper with right= Cell.make value :: zipper.right}

  let left_length zipper = List.length zipper.left

  let right_length zipper = List.length zipper.right

  let is_at_start zipper = List.is_empty zipper.left

  let is_at_end zipper = List.is_empty zipper.right

  (* We split On_monad into two bits so we can use the option-monad
     specialisation of some of the monadic operations to define some of the
     others. *)
  module On_monad_base (M : Monad.S) = struct
    module CM = Cell.On_monad (M)
    module CO = Cell.On_monad (Option)

    let pop_m zipper ~on_empty =
      match zipper.right with
      | [] -> on_empty zipper
      | x :: xs -> M.return (Cell.data x, {zipper with right= xs})
  end

  module On_option_base = On_monad_base (Option)

  let pop_opt zipper = On_option_base.pop_m ~on_empty:(Fn.const None) zipper

  let peek_opt ?(steps = 0) zipper =
    let open Option.Let_syntax in
    let%map cell =
      if steps < 0 then List.nth zipper.left (Int.abs steps - 1)
      else List.nth zipper.right steps
    in
    Cell.data cell

  module On_monad (M : Monad.S) = struct
    include On_monad_base (M)

    let peek_m ?steps zipper ~on_empty =
      match peek_opt ?steps zipper with
      | Some v -> M.return v
      | None -> on_empty zipper

    let step_m ?(steps = 1) zipper ~on_empty =
      let amount = Int.abs steps in
      match Ordering.of_int (Int.compare steps 0) with
      | Less -> (
        match rev_transfer amount ~src:zipper.left ~dst:zipper.right with
        | Some (l, r) -> M.return {left= l; right= r}
        | None -> on_empty zipper )
      | Equal -> M.return zipper
      | Greater -> (
        match rev_transfer amount ~src:zipper.right ~dst:zipper.left with
        | Some (r, l) -> M.return {left= l; right= r}
        | None -> on_empty zipper )

    let push_left_m zipper ~value ~on_empty =
      push zipper ~value |> step_m ~on_empty

    let map_m_head_cell zipper ~f ~on_empty =
      match head zipper with
      | None -> on_empty zipper
      | Some h -> M.(f h >>| set_head_cell zipper)

    let map_m_head zipper ~f ~on_empty =
      map_m_head_cell zipper
        ~f:M.(fun h -> CM.map_m ~f h >>| CO.sequence_m)
        ~on_empty
  end

  module On_ident = On_monad (Monad.Ident)
  module On_error = On_monad (Or_error)
  module On_option = On_monad (Option)

  let to_two_lists zipper = (left_list zipper, right_list zipper)

  let map_head = On_ident.map_m_head ~on_empty:Fn.id

  let pop zipper =
    On_error.pop_m zipper ~on_empty:(fun _ ->
        Or_error.error_string "Tried to pop an exhausted zipper" )

  let step ?steps zipper =
    On_error.step_m ?steps zipper ~on_empty:(fun zipper ->
        Or_error.error_s
          [%message
            "Zipper stepping went out of bounds"
              ~steps:(Option.value ~default:1 steps : int)
              ~left_bound:(left_length zipper : int)
              ~right_bound:(right_length zipper : int)] )

  (* Pushing left shouldn't fail, since the right list will always be
     nonempty after the push. *)
  let push_left = On_ident.push_left_m ~on_empty:(fun _ -> assert false)
end

module Plain_cell : Basic_cell = struct
  include Singleton

  let make = Fn.id

  let data = Fn.id

  let of_data_list = Fn.id

  let to_data_list = Fn.id
end

module Plain : S = Make (Plain_cell)

module Make_marked_cell (B : Basic_mark) = struct
  module Mark = struct
    module M = struct
      include B
      include Comparator.Make (B)
    end

    include M

    module Set = struct
      type t = Set.M(M).t [@@deriving sexp]
    end
  end

  (** ['a t] is the type of one cell. Each cell contains the data at the
      given zipper location, as well as any marks that have been attached to
      the cell for later recall. *)
  type 'a t = {data: 'a; marks: Mark.Set.t} [@@deriving fields, sexp]

  let make data = {data; marks= Set.empty (module Mark)}

  let of_data_list = List.map ~f:make

  let to_data_list = List.map ~f:data

  let mark cell ~mark = {cell with marks= Set.add cell.marks mark}

  module T = Travesty.Traversable.Make1 (struct
    type nonrec 'a t = 'a t

    module On (M : Applicative.S) = struct
      let map_m cell ~f = M.(f cell.data >>| fun d -> {cell with data= d})
    end
  end)

  include (T : module type of T with type 'a t := 'a t)
end

module Make_marked (Mark : Basic_mark) = struct
  module Cell = Make_marked_cell (Mark)
  module Main = Make (Cell)

  (* Don't include On_monad, as we extend it. *)
  include (Main : S_non_monadic with type 'a t = 'a Main.t)

  module On_monad (M : Monad.S) = struct
    include Main.On_monad (M)

    let mark_m zipper ~mark ~on_empty =
      map_m_head_cell zipper
        ~f:(fun h -> M.return (Some (Cell.mark ~mark h)))
        ~on_empty

    let recall_m zipper ~mark ~on_empty =
      let rec mu zipper' =
        match List.hd zipper'.Main.right with
        | Some h when Set.mem (Cell.marks h) mark -> M.return zipper'
        | Some _ | None -> M.(step_m ~steps:(-1) zipper' ~on_empty >>= mu)
      in
      mu zipper

    let delete_to_mark_m zipper ~mark ~on_empty =
      let open M.Let_syntax in
      let%map recalled_zipper = recall_m zipper ~mark ~on_empty in
      let amount_to_delete =
        left_length zipper - left_length recalled_zipper
      in
      {zipper with left= List.drop zipper.left amount_to_delete}

    let rec fold_m_until zipper ~f ~init ~finish =
      let open M.Let_syntax in
      match pop_opt zipper with
      | None -> finish init zipper
      | Some (hd, zipper') -> (
          match%bind f init hd zipper' with
          | `Stop final -> M.return final
          | `Drop accum -> fold_m_until zipper' ~f ~init:accum ~finish
          | `Mark (mark, hd', accum) ->
              push zipper' ~value:hd'
              |> mark_m ~mark ~on_empty:M.return
              >>= step_m ~on_empty:M.return
              >>= fold_m_until ~f ~init:accum ~finish
          | `Swap (hd', accum) ->
              push_left_m zipper' ~value:hd' ~on_empty:M.return
              >>= fold_m_until ~f ~init:accum ~finish )
  end

  module On_ident = On_monad (Monad.Ident)
  module On_error = On_monad (Or_error)
  module On_option = On_monad (Option)

  let mark zipper ~mark =
    On_error.mark_m zipper ~mark ~on_empty:(fun _ ->
        Or_error.error_string "Tried to mark an exhausted zipper" )

  let mark_not_found mark =
    Or_error.error_s
      [%message "Couldn't find requested mark" ~mark:(mark : Mark.t)]

  let recall zipper ~mark =
    On_error.recall_m zipper ~mark ~on_empty:(fun _ -> mark_not_found mark)

  let delete_to_mark zipper ~mark =
    On_error.delete_to_mark_m zipper ~mark ~on_empty:(fun _ ->
        mark_not_found mark )

  let fold_until = On_ident.fold_m_until
end

module Int_mark_zipper : S_marked with type mark := int = Make_marked (Int)
