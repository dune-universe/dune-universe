exception Time_slot_is_invalid

exception Time_slot_is_empty

type t = int64 * int64

let lt (x1, y1) (x2, y2) =
  (* lexicographical order *)
  x1 < x2 || (x1 = x2 && y1 < y2)

let le x y = lt x y || x = y

let gt x y = lt y x

let ge x y = le y x

let compare x y = if lt x y then -1 else if x = y then 0 else 1

let to_string ((start, end_exc) : t) : string =
  Printf.sprintf "[%Ld, %Ld)" start end_exc

module Check = struct
  let is_valid ((start, end_exc) : t) : bool = start <= end_exc

  let is_not_empty ((start, end_exc) : t) : bool = start <> end_exc

  let check_if_valid (x : t) : t =
    if is_valid x then x else raise Time_slot_is_invalid

  let check_if_not_empty (x : t) : t =
    if is_not_empty x then x else raise Time_slot_is_empty
end

let join (ts1 : t) (ts2 : t) : t option =
  let aux (start1, end_exc1) (start2, end_exc2) =
    if start2 <= end_exc1 then Some (start1, max end_exc1 end_exc2) else None
  in
  let start1, end_exc1 = Check.check_if_valid ts1 in
  let start2, end_exc2 = Check.check_if_valid ts2 in
  if start1 <= start2 then aux (start1, end_exc1) (start2, end_exc2)
  else aux (start2, end_exc2) (start1, end_exc1)

let overlap_of_a_over_b ~(a : t) ~(b : t) : t option * t option * t option =
  let a_start, a_end_exc = Check.check_if_valid a in
  let b_start, b_end_exc = Check.check_if_valid b in
  if a_start = a_end_exc then (None, None, None)
  else if a_end_exc <= b_start then (Some a, None, None)
  else if b_end_exc <= a_start then (None, None, Some a)
  else if a_start < b_start then
    if a_end_exc <= b_end_exc then
      (Some (a_start, b_start), Some (b_start, a_end_exc), None)
    else (Some (a_start, b_start), Some b, Some (b_end_exc, a_end_exc))
  else if a_end_exc <= b_end_exc then (None, Some (a_start, a_end_exc), None)
  else (None, Some (a_start, a_end_exc), Some (b_end_exc, a_end_exc))

module Serialize = struct
  let pack_time_slot (start, end_exc) =
    ( Misc_utils.int32_int32_of_int64 start,
      Misc_utils.int32_int32_of_int64 end_exc )
end

module Deserialize = struct
  let unpack_time_slot (start, end_exc) =
    ( Misc_utils.int64_of_int32_int32 start,
      Misc_utils.int64_of_int32_int32 end_exc )
end
