open Base

type t = {
  chr : string ;
  lo : int ;
  hi : int ;
} [@@deriving equal, compare, sexp]

let to_string { chr ; lo ; hi } =
  Printf.sprintf "%s:%d-%d" chr lo hi

let of_string_exn s =
  try Caml.Scanf.sscanf s "%[^:]:%d-%d" (fun chr lo hi -> { chr ; lo ; hi })
  with _ -> failwith ("GLoc.of_string_exn: " ^ s)

let%test "of_string_exn_1" =
  Caml.(
    of_string_exn "chr1:3053032-3053034"
    =
    { chr = "chr1" ; lo = 3053032 ; hi = 3053034 }
  )

let of_string s =
  try Ok (of_string_exn s)
  with _ -> Error `Parse_error

let range { lo ; hi ; _ } = Range.make ~lo ~hi

let strictly_before x y =
  match String.compare x.chr y.chr with
  | -1 -> true
  |  1 -> false
  |  0 -> x.hi < y.lo
  | _ -> assert false

let%test "strictly_before_1" =
  strictly_before { chr = "a" ; lo = 0 ; hi = 4 } { chr = "b" ; lo = 0 ; hi = 4 }

let%test "strictly_before_2" =
  strictly_before { chr = "a" ; lo = 0 ; hi = 4 } { chr = "a" ; lo = 10 ; hi = 40 }

let%test "strictly_before_3" =
  not (
    strictly_before { chr = "a" ; lo = 0 ; hi = 4 } { chr = "a" ; lo = 4 ; hi = 40 }
  )

let intersects x y =
  String.(x.chr = y.chr)
  && (
    (x.lo <= y.lo && y.lo <= x.hi)
    || (y.lo <= x.lo && x.lo <= y.hi)
  )

let%test "intersects_1" =
  intersects { chr = "a" ; lo = 0 ; hi = 4 } { chr = "a" ; lo = 2 ; hi = 30 }

let inter s s' =
  if not (intersects s s') then None
  else Some { chr = s.chr ; lo = max s.lo s'.lo ; hi = min s.hi s'.hi }

let dist s s' =
  if String.(s.chr <> s'.chr) then None
  else Some (
    if intersects s s' then 0
    else min (abs (s'.lo - s.hi)) (abs (s.lo - s'.hi))
  )

let position ~from loc =
  if String.(loc.chr <> from.chr) then None
  else Some (
    if intersects from loc then 0
    else (
      let a, b = loc.hi - from.lo, loc.lo - from.hi in
      if abs a < abs b then a else b
    )
  )

let relmove l lo hi = { l with lo = l.lo + lo ; hi = l.hi + hi }
let relative l lo hi = { l with lo = l.lo + lo ; hi = l.lo + hi }
let zoom l factor =
  let size = Float.to_int (Float.of_int (l.hi - l.lo + 1) *. factor) in
  let center = (l.lo + l.hi) / 2 in
  { chr = l.chr ; lo = (center - size / 2) ; hi = (center + size / 2) }
let size l = l.hi - l.lo + 1

let included_in s s' =
  if String.(s.chr <> s'.chr) then false
  else (s.lo >= s'.lo && s.hi <= s'.hi)

let union s s' =
  if String.(s.chr = s.chr) then
    Some {
      chr = s.chr ;
      lo = Int.min s.lo s'.lo ;
      hi = Int.max s.hi s'.hi ;
    }
  else
    None

module Elt = struct
  type nonrec t = t
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
  let compare = compare
end
module Map = Core_kernel.Map.Make(Elt)
module Set = Core_kernel.Set.Make(Elt)
