open Int.O
open Bool.O
open Functions.Function1.O

type full = {
  min: int;
  max: int;
  step: int;
}

type t =
  | Empty
  | Ascending of full
  | Descending of full

let empty = Empty

let make ?(start=0) ?(step=1) stop =
  (* We normalize everything here, so that to_list is simpler, and Equate.Poly is appropriate *)
  if (stop - start) * step <= 0 then
    Empty
  else if step > 0 then
    let min = start
    and max = stop - 1 - (stop - 1 - start) mod step in
    assert ((max - min) mod step = 0);
    Ascending {min; max; step}
  else
    let step = -step in
    let min = stop + 1 - (stop + 1 - start) mod step
    and max = start in
    assert ((max - min) mod step = 0);
    Descending {min; max; step}

let repr = function
  | Empty ->
    "[]"
  | Ascending {min; max; step} ->
    Format.apply "[%i to %i step %i]" min max step
  | Descending {min; max; step} ->
    Format.apply "[%i down to %i step -%i]" max min step

include Equate.Poly

let to_list = function
  | Empty -> []
  | Ascending {min; max; step} ->
    let rec aux xs x =
      assert (x >= min);
      if x = min then x::xs else aux (x::xs) (x - step)
    in
    aux [] max
  | Descending {min; max; step} ->
    let rec aux xs x =
      assert (x <= max);
      if x = max then x::xs else aux (x::xs) (x + step)
    in
    aux [] min

let to_array r =
  r |> to_list |> List.to_array

let fold ~init r ~f =
  r |> to_list |> List.fold ~init ~f

let fold_i ~init r ~f =
  r |> to_list |> List.fold_i ~init ~f

let fold_acc ~acc ~init r ~f =
  r |> to_list |> List.fold_acc ~acc ~init ~f

let reduce r ~f =
  r |> to_list |> List.reduce ~f

let reduce_i r ~f =
  r |> to_list |> List.reduce_i ~f

let reduce_acc ~acc r ~f =
  r |> to_list |> List.reduce_acc ~acc ~f

let try_reduce r ~f =
  r |> to_list |> List.try_reduce ~f

let try_reduce_i r ~f =
  r |> to_list |> List.try_reduce_i ~f

let try_reduce_acc ~acc r ~f =
  r |> to_list |> List.try_reduce_acc ~acc ~f

let iter r ~f =
  r |> to_list |> List.iter ~f

let iter_i r ~f =
  r |> to_list |> List.iter_i ~f

let iter_acc ~acc r ~f =
  r |> to_list |> List.iter_acc ~acc ~f

let count r ~f =
  r |> to_list |> List.count ~f

let count_i r ~f =
  r |> to_list |> List.count_i ~f

let count_acc ~acc r ~f =
  r |> to_list |> List.count_acc ~acc ~f

let fold_short ~init r ~f =
  r |> to_list |> List.fold_short ~init ~f

let fold_short_i ~init r ~f =
  r |> to_list |> List.fold_short_i ~init ~f

let fold_short_acc ~acc ~init r ~f =
  r |> to_list |> List.fold_short_acc ~acc ~init ~f

let reduce_short r ~f =
  r |> to_list |> List.reduce_short ~f

let reduce_short_i r ~f =
  r |> to_list |> List.reduce_short_i ~f

let reduce_short_acc ~acc r ~f =
  r |> to_list |> List.reduce_short_acc ~acc ~f

let try_reduce_short r ~f =
  r |> to_list |> List.try_reduce_short ~f

let try_reduce_short_i r ~f =
  r |> to_list |> List.try_reduce_short_i ~f

let try_reduce_short_acc ~acc r ~f =
  r |> to_list |> List.try_reduce_short_acc ~acc ~f

let iter_short r ~f =
  r |> to_list |> List.iter_short ~f

let iter_short_i r ~f =
  r |> to_list |> List.iter_short_i ~f

let iter_short_acc ~acc r ~f =
  r |> to_list |> List.iter_short_acc ~acc ~f

let for_all r ~f =
  r |> to_list |> List.for_all ~f

let for_all_i r ~f =
  r |> to_list |> List.for_all_i ~f

let for_all_acc ~acc r ~f =
  r |> to_list |> List.for_all_acc ~acc ~f

let there_exists r ~f =
  r |> to_list |> List.there_exists ~f

let there_exists_i r ~f =
  r |> to_list |> List.there_exists_i ~f

let there_exists_acc ~acc r ~f =
  r |> to_list |> List.there_exists_acc ~acc ~f

let find r ~f =
  r |> to_list |> List.find ~f

let find_i r ~f =
  r |> to_list |> List.find_i ~f

let find_acc ~acc r ~f =
  r |> to_list |> List.find_acc ~acc ~f

let try_find r ~f =
  r |> to_list |> List.try_find ~f

let try_find_i r ~f =
  r |> to_list |> List.try_find_i ~f

let try_find_acc ~acc r ~f =
  r |> to_list |> List.try_find_acc ~acc ~f

let find_map r ~f =
  r |> to_list |> List.find_map ~f

let find_map_i r ~f =
  r |> to_list |> List.find_map_i ~f

let find_map_acc ~acc r ~f =
  r |> to_list |> List.find_map_acc ~acc ~f

let try_find_map r ~f =
  r |> to_list |> List.try_find_map ~f

let try_find_map_i r ~f =
  r |> to_list |> List.try_find_map_i ~f

let try_find_map_acc ~acc r ~f =
  r |> to_list |> List.try_find_map_acc ~acc ~f

module ToList = struct
  let map r ~f =
    r |> to_list |> List.map ~f

  let map_i r ~f =
    r |> to_list |> List.map_i ~f

  let map_acc ~acc r ~f =
    r |> to_list |> List.map_acc ~acc ~f

  let filter r ~f =
    r |> to_list |> List.filter ~f

  let filter_i r ~f =
    r |> to_list |> List.filter_i ~f

  let filter_acc ~acc r ~f =
    r |> to_list |> List.filter_acc ~acc ~f

  let filter_map r ~f =
    r |> to_list |> List.filter_map ~f

  let filter_map_i r ~f =
    r |> to_list |> List.filter_map_i ~f

  let filter_map_acc ~acc r ~f =
    r |> to_list |> List.filter_map_acc ~acc ~f

  let flat_map r ~f =
    r |> to_list |> List.flat_map ~f

  let flat_map_i r ~f =
    r |> to_list |> List.flat_map_i ~f

  let flat_map_acc ~acc r ~f =
    r |> to_list |> List.flat_map_acc ~acc ~f

  let scan ~init r ~f =
    r |> to_list |> List.scan ~init ~f

  let scan_i ~init r ~f =
    r |> to_list |> List.scan_i ~init ~f

  let scan_acc ~acc ~init r ~f =
    r |> to_list |> List.scan_acc ~acc ~init ~f

  let scan_short ~init r ~f =
    r |> to_list |> List.scan_short ~init ~f

  let scan_short_i ~init r ~f =
    r |> to_list |> List.scan_short_i ~init ~f

  let scan_short_acc ~acc ~init r ~f =
    r |> to_list |> List.scan_short_acc ~acc ~init ~f
end
