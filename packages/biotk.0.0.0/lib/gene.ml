open Base

let comparison_of_strand = function
  | `Plus -> GLoc.compare
  | `Minus -> Fn.flip GLoc.compare

let rec list_fold_consecutive_pairs xs ~init ~f = match xs with
  | [] | [ _ ] -> init
  | e1 :: (e2 :: _ as t) ->
    f (list_fold_consecutive_pairs t ~f ~init) e1 e2

let non_overlapping_exons ~strand exons =
  let sorted_exons = List.sort exons ~compare:(comparison_of_strand strand) in
  let overlaps =
    list_fold_consecutive_pairs sorted_exons ~init:[] ~f:(fun acc e1 e2 ->
        if GLoc.intersects e1 e2 then (e1, e2) :: acc else acc
      )
  in
  if List.is_empty overlaps then Ok sorted_exons
  else Or_error.error "Exons overlaping" overlaps [%sexp_of: (GLoc.t * GLoc.t) list]

module Transcript = struct
  type t = {
    id : string ;
    chr : string ;
    strand : [ `Plus | `Minus ] ;
    exons : (int * int) list ;
  }

  let unique_chr locs =
    Utils.unique_string locs ~f:(fun loc -> loc.GLoc.chr)
    |> Or_error.tag ~tag:"finding chromosome for transcript"

  let make ~id ~strand exons =
    let open Or_error.Monad_infix in
    unique_chr exons >>= fun chr ->
    non_overlapping_exons ~strand exons >>= fun sorted_exons ->
    Ok { id ; chr ; strand ;
         exons = List.map sorted_exons ~f:(fun r -> r.GLoc.lo, r.hi) }

  let space_between (s1, e1) (s2, e2) =
    if e1 <= s2 then (e1, s2)
    else e2, s1

  let introns g =
    list_fold_consecutive_pairs g.exons ~init:[] ~f:(fun acc e1 e2 ->
        let lo, hi = space_between e1 e2 in
        { GLoc.chr = g.chr ; lo ; hi } :: acc
      )

  let exons g =
    List.map g.exons ~f:(fun (lo, hi) -> GLoc.{ chr = g.chr ; lo ; hi })

  let range t =
    List.fold t.exons ~init:Int.(max_value, min_value) ~f:(fun (acc_lo, acc_hi) (lo, hi) ->
        Int.min acc_lo lo, Int.max acc_hi hi
      )

  let loc t =
    let lo, hi = range t in
    GLoc.{ chr = t.chr ; lo ; hi }

  let upstream t len =
    let t_lo, t_hi = range t in
    let lo, hi = match t.strand with
      | `Plus -> Int.max 0 (t_lo - len), 0
      | `Minus -> t_hi, t_hi + len
    in
    GLoc.{ chr = t.chr ; lo ; hi }

  let downstream t len =
    let t_lo, t_hi = range t in
    let lo, hi = match t.strand with
      | `Minus -> Int.max 0 (t_lo - len), 0
      | `Plus -> t_hi, t_hi + len
    in
    GLoc.{ chr = t.chr ; lo ; hi }
end

type t = {
  id : string ;
  chr : string ;
  strand : [ `Plus | `Minus ] ;
  transcripts : Transcript.t list ;
}

let make ~id ~strand transcripts =
  let open Or_error.Monad_infix in
  List.map transcripts ~f:(fun (id, exons) ->
      Transcript.make ~id ~strand exons
    )
  |> Or_error.all >>= fun transcripts ->
  Utils.unique_string transcripts ~f:(fun t -> t.Transcript.chr)
  |> Or_error.tag ~tag:"finding gene chromosome" >>= fun chr ->
  Ok { id ; chr ; strand ; transcripts }

let range g =
  List.fold g.transcripts ~init:Int.(max_value, min_value) ~f:(fun (acc_lo, acc_hi) t ->
      let t_lo, t_hi = Transcript.range t in
      Int.min acc_lo t_lo, Int.max acc_hi t_hi
    )

let loc g =
  let lo, hi = range g in
  GLoc.{ chr = g.chr ; lo ; hi }

let exons g =
  List.concat_map g.transcripts ~f:Transcript.exons
  |> List.dedup_and_sort ~compare:GLoc.compare

let introns g =
  List.concat_map g.transcripts ~f:Transcript.introns
  |> List.dedup_and_sort ~compare:GLoc.compare

let upstream g len =
  let g_lo, g_hi = range g in
  let lo, hi = match g.strand with
    | `Plus -> Int.max 0 (g_lo - len), 0
    | `Minus -> g_hi, g_hi + len
  in
  GLoc.{ chr = g.chr ; lo ; hi }

let downstream g len =
  let g_lo, g_hi = range g in
  let lo, hi = match g.strand with
    | `Minus -> Int.max 0 (g_lo - len), 0
    | `Plus -> g_hi, g_hi + len
  in
  GLoc.{ chr = g.chr ; lo ; hi }
