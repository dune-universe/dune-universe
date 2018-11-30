open Printf
open Gg

let () = Random.self_init()

let is_finite (x: float) = x -. x = 0. [@@inline]

type point = {
    t: float; (* parameter, MUST be finite *)
    x: float; (* valid ⇔ is_finite x (and thus is_finite y) *)
    y: float;
    mutable cost: float; (* cache the cost for faster updates of
                            segments.  See module {!Cost}. *)
  } (* pure float record ⇒ optimized *)

let dummy_point = { t = nan;  x = nan;  y = nan;  cost = nan }

let is_valid p = is_finite p.x [@@inline]

let point ~t ~x ~y =
  { t;  x = (if is_finite y then x else nan);  y;  cost = 0. } [@@inline]

(* WARNING: Because of mutability, segments may only belong to at most
   one sampling. *)
type segment = {
    (* At least one of [p0] and [p1] must be valid. *)
    p0: point;
    p1: point;
    (* Segments are ordered by increasing values of [t].  There may be
       jumps however in [t] values. *)
    mutable prev: segment; (* previous segment in curve; oneself if first. *)
    mutable next: segment; (* next segment in curve; oneself if last. *)
    (* The segments are all linked together in the direction of the
       parametrisation of the path, even across path "cuts".  A "cut"
       may be expressed by an invalid point (a point outside the
       domain, the boundary of which needs to be refined) or by the
       fact that [p1 != next.p0] (in which case, the parameter [t] may
       be considered starting anew). *)

    mutable witness: segment PQ.witness option;
    mutable weight: float; (* decrease the cost as we split so as to
                              not concentrate the on a single problem *)
  }

let is_first s = s.prev == s [@@inline]
let is_last s = s.next == s [@@inline]

let rec dummy_seg = {
    p0 = dummy_point;  p1 = dummy_point;
    prev = dummy_seg;  next = dummy_seg;  witness = None;  weight = 1. }

(* Segment with [.prev] and [.next] being itself. *)
let segment ~p0 ~p1 ~weight =
  (let rec s = { p0;  p1;  prev = s;  next = s;
                 witness = None; weight } in
   s) [@@inline]

(* The phantom type variable will say whether the sampling correspond
   to a function — and thus can be refined — or not. *)
type 'a t = {
    seg: segment PQ.t; (* DISJOINT segments (except for endpoints). *)
    (* If the queue is empty but not the segment list, costs need
       updating.  When the queue is non-empty, all segments MUST have
       a witness. *)
    mutable first: segment; (* or dummy if [seg] is empty. *)
    mutable last: segment;  (* or dummy if [seg] is empty. *)
    vp: Box2.t;             (* viewport = zone of interest *)
  }

let is_empty t = t.first == dummy_seg [@@inline]

let make_empty () = {
    seg = PQ.make();  first = dummy_seg;  last = dummy_seg;
    vp = Box2.unit }

(** A "connected" sub-path means a sequence of segments such that,
   for all segments [s] but the last one, [s.p1 == s.next.p0] and all
   these points are valid ([p0] of the first segment and [p1] of the
   last segment may be invalid). *)
(* [last_is_cut] id true if the last operation was a [cut].  [cut] is
   applied for any path interruption. *)
let rec fold_points_incr_segments ~prev_p ~last_is_cut f ~cut acc seg =
  let p0 = seg.p0 and p1 = seg.p1 in
  let acc =
    if p0 == prev_p then (* p0 already treated (usual case) *)
      if is_valid p1 then f acc p1
      else if is_last seg then acc else cut acc (* p0 valid *)
    else if is_valid p0 then
      let acc = f (if last_is_cut then acc else cut acc) p0 in
      if is_valid p1 then f acc p1
      else if is_last seg then acc else cut acc
    else (* not(is_valid p0), thus cut and p1 valid *)
      f (if last_is_cut then acc else cut acc) p1 in
  if is_last seg then acc
  else fold_points_incr_segments ~prev_p:p1 ~last_is_cut:(not(is_valid p1))
         f ~cut acc seg.next

(** [fold t ~init f] fold [f] once on each valid point.  The points
   are passed in the order of the curve. *)
let fold_points t ~init ~cut f =
  if is_empty t then init
  else (* [last_is_cut] is true at first because we do not want to
          introduce a [cut] at the beginning of the curve. *)
    fold_points_incr_segments ~prev_p:dummy_point ~last_is_cut:true
         f ~cut init t.first

let rec fold_points_decr_segments ~prev_p ~last_is_cut f ~cut acc seg =
  let p0 = seg.p0 and p1 = seg.p1 in
  let acc =
    if p1 == prev_p then
      if is_valid p0 then f acc p0
      else if is_first seg then acc else cut acc (* No cut at 1st place *)
    else if is_valid p1 then
      let acc = f (if last_is_cut then acc else cut acc) p1 in
      if is_valid p0 then f acc p0
      else if is_first seg then acc else cut acc
    else (* not(is_valid p1), thus cut and p0 valid *)
      f (if last_is_cut then acc else cut acc) p0 in
  if is_first seg then acc
  else fold_points_decr_segments ~prev_p:p0 ~last_is_cut:(not(is_valid p0))
         f ~cut acc seg.prev

(** Same as [fold] but the points are passed in the opposite order of
   the curve. *)
let fold_points_decr t ~init ~cut f =
  if is_empty t then init
  else fold_points_decr_segments ~prev_p:dummy_point ~last_is_cut:true
         f ~cut init t.last


let rec iter_segments s f =
  f s;
  if not(is_last s) then iter_segments s.next f

(** Apply [f] to all segments in the order of the path. *)
let iter t ~f =
  if not(is_empty t) then iter_segments t.first f

let rec map_segments ~prev_p ~prev_fp ~prev_s s f =
  let p0 = if s.p0 == prev_p then prev_fp else f s.p0 in
  let p1 = f s.p1 in
  let s' = segment ~p0 ~p1 ~weight:1. in
  s'.prev <- prev_s;
  prev_s.next <- s';
  if is_last s then (s'.next <- s'; s')
  else map_segments ~prev_p:s.p1 ~prev_fp:p1 ~prev_s:s' s.next f

(** Create a new sampling by applying [f] to all points. *)
let map t ~f =
  if is_empty t then make_empty()
  else
    let p0 = f t.first.p0 in
    let p1 = f t.first.p1 in
    let first' = segment ~p0 ~p1 ~weight:1. in
    if is_last t.first then ( (* single segment *)
      first'.next <- first';
      { seg = PQ.make(); (* costs need recomputing *)
        first = first';  last = first';  vp = Box2.unit }
    )
    else
      let last' = map_segments ~prev_p:t.first.p1 ~prev_fp:p0 ~prev_s:first'
                    t.first.next f in
      { seg = PQ.make();
        first = first';  last = last';  vp = t.vp }


(** Save *)

let to_channel t fh =
  fold_points t ~init:()
    (fun () p -> fprintf fh "%e\t%e\n" p.x p.y)
    ~cut:(fun () -> output_char fh '\n')

let to_file t fname =
  let fh = open_out fname in
  to_channel t fh;
  close_out fh

let to_latex t fname =
  let fh = open_out fname in
  output_string fh "% Written by OCaml Curve_sampling (version 0.1)\n";
  output_string fh "\\begin{pgfscope}\n";
  iter t ~f:(fun s ->
      let p0 = s.p0 and p1 = s.p1 in
      if is_valid p0 && is_valid p1 then
        fprintf fh "\\pgfpathmoveto{\\pgfpointxy{%.16f}{%.16f}}\n\
                    \\pgfpathlineto{\\pgfpointxy{%.16f}{%.16f}}\n\
                    \\pgfusepath{stroke}\n"
           p0.x p0.y p1.x p1.y);
  output_string fh "\\end{pgfscope}\n";
  close_out fh

let to_list t =
  let path, seg = fold_points_decr t ~init:([], [])
    (fun (path, seg) p -> (path, (p.x, p.y) :: seg))
    ~cut:(fun (path, seg) -> (seg :: path, [])) in
  if seg <> [] then seg :: path else path

(** Transform *)

let tr m t =
  map t ~f:(fun p -> let p' = P2.tr m (P2.v p.x p.y) in
                     {t = p.t;  x = P2.x p';  y = P2.y p';  cost = nan})

(* Constructing samplings
 ***********************************************************************)

(* Compute a sampling from a sequence of points.  No costs are computed. *)
module Of_sequence = struct
  type state = { mutable first: segment;
                 mutable p: point;  (* last point *)
                 mutable last: segment; (* last segment *)
                 mutable add : state -> point -> unit }

  let add_point st p =
    if is_valid p || is_valid st.p then (
      (* The caller is responsible to setup [p] so that [is_valid(p)]
         is meaningful and to pass points in the increasing order of
         [t].  One of the two points must be valid or the segment is
         dropped. *)
      let rec s = { p0 = st.p;  p1 = p;  prev = st.last;  next = s;
                    witness = None;  weight = 1. } in
      st.last.next <- s;
      st.last <- s;
    );
    st.p <- p

  (** "Jump" from the previous point to [p].  This will introduce a
     "cut" in the path ([p0] of next segment ≠ [p1] of last segment). *)
  let jump st p = st.p <- p

  let add_first_segment st p =
    let s = segment ~p0:st.p ~p1:p ~weight:1. in
    st.first <- s;
    st.last <- s;
    st.add <- add_point

  (* Function used until an initial segment is added. *)
  let add_init st p =
    assert(st.first == dummy_seg);
    if is_valid p then (
      if is_finite st.p.t then
        (* The previous point is maybe outside the domain (thus
           "invalid") but corresponds to a valid [t], so the segment
           may be refined to find the boundary of the domain.  In
           particular, it is not a dummy point. *)
        add_first_segment st p
    )
    else if is_valid st.p then (
      (* [p] is not valid (but we assume [is_finite p.t]) but a first
         valid point was added previously. *)
      add_first_segment st p
    );
    st.p <- p

  let init() = { first = dummy_seg;  p = dummy_point;  last = dummy_seg;
                 add = add_init }

  let add st p = st.add st p

  let last_point st = st.p [@@inline]

  let close st =
    { seg = PQ.make(); (* costs must be computed *)
      first = st.first; last = st.last;  vp = Box2.unit }

  let close_with_viewport st vp =
    { seg = PQ.make(); (* costs must be computed *)
      first = st.first; last = st.last;  vp }
end

(** Generic box clipping *)
let clip t b : [`Pt] t =
  if Box2.is_empty b then invalid_arg "Curve_sampling.crop: empty box";
  if is_empty t then make_empty()
  else (
    let st = Of_sequence.init() in
    let s = ref t.first in
    let continue = ref true in
    while !continue do
      (* Use Liang–Barsky algorithm to clip the segment. *)
      let p0 = !s.p0 and p1 = !s.p1 in
      let x0 = p0.x and x1 = p1.x in
      let y0 = p0.y and y1 = p1.y in
      if p0 == Of_sequence.last_point st then (
        (* [p0] is the continuation of the previous segment and is in
           [b] (or possibly invalid) because it was added.  Thus
           [t0=0] (see the "else" clause) and we do not determine it. *)
        if not(is_valid p1) then ( (* thus [p0] valid and already added *)
          Of_sequence.add st p1
        )
        else if not (is_valid p0) then ( (* thus [p1] valid *)
          if Box2.mem (P2.v p1.x p1.y) b then Of_sequence.add st p1
        )
        else (
          (* [p0] is valid as was added.  Thus is in [b] and no tests
             on this point are needed and the current segment will not
             be dropped. *)
          let t1 = ref 1. in
          (* Coordinate X. *)
          let dx = x1 -. x0 in
          (* Box2.minx b ≤ x0 ≤ Box2.maxx b *)
          if dx > 0. (* x0 < x1 *) then (
            let r1 = (Box2.maxx b -. x0) /. dx in
            if r1 < !t1 then t1 := r1
          )
          else if dx < 0. (* i.e., x0 > x1 *) then (
            let r1 = (Box2.minx b -. x0) /. dx in
            if r1 < !t1 then t1 := r1;
          );
          let dy = y1 -. y0 in
          (* Coordinate Y. *)
          if dy > 0. (* i.e., y0 < y1 *) then (
            let r1 = (Box2.maxy b -. y0) /. dy in
            if r1 < !t1 then t1 := r1
          )
          else if dy < 0. (* i.e., y0 > y1 *) then (
            let r1 = (Box2.miny b -. y0) /. dy in
            if r1 < !t1 then t1 := r1
          );
          (* Add the endpoint of the segment. *)
          (* The value of [t1] os only a linear estimate.  Thus the
             resulting sampling is a [`Pt] one and it cannot be refined. *)
          Of_sequence.add st (if !t1 = 1. then p1 (* whole segment *)
                              else { t = p0.t +. !t1 *. (p1.t -. p0.t);
                                     x = x0 +. !t1 *. dx;  y = y0 +. !t1 *. dy;
                                     cost = 0. })
        )
      )
      else (
        (* [p0] was not added (jump, previous segment cut or dropped).
           We have to deal with both [p0] and [p1]. *)
        if not(is_valid p1) then ( (* thus [p0] valid *)
          if Box2.mem (P2.v x0 y0) b then (
            Of_sequence.jump st p0;  Of_sequence.add st p1)
        )
        else if not (is_valid p0) then ( (* thus [p1] valid *)
          if Box2.mem (P2.v p1.x p1.y) b then (
            Of_sequence.jump st p0;  Of_sequence.add st p1)
        )
        else (
          let t0 = ref 0. in
          let t1 = ref 1. in  (* convention: t1 < 0 ⇒ drop segment *)
          (* Coordinate X. *)
          let dx = x1 -. x0 in
          if dx = 0. then (
            if x0 < Box2.minx b || x0 > Box2.maxx b then
              t1 := -1.; (* drop [s] *)
          )
          else if dx > 0. (* x0 < x1 *) then (
            let r0 = (Box2.minx b -. x0) /. dx in
            let r1 = (Box2.maxx b -. x0) /. dx in (* r0 ≤ r1 *)
            if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment [s] *)
            else (if r0 > !t0 then t0 := r0;
                  if r1 < !t1 then t1 := r1; )
          )
          else (* dx < 0 i.e., x0 > x1 *) (
            let r0 = (Box2.maxx b -. x0) /. dx in
            let r1 = (Box2.minx b -. x0) /. dx in
            if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
            else (if r0 > !t0 then t0 := r0;
                  if r1 < !t1 then t1 := r1; )
          );
          let dy = y1 -. y0 in
          if !t1 >= 0. (* segment not dropped *) then (
            (* Coordinate Y. *)
            if dy = 0. (* y0 = y1 *) then (
              if y0 < Box2.miny b || y0 > Box2.maxy b then
                t1 := -1.; (* drop [s] *)
            )
            else if dy > 0. (* i.e., y0 < y1 *) then (
              let r0 = (Box2.miny b -. y0) /. dy in
              let r1 = (Box2.maxy b -. y0) /. dy in (* r0 ≤ r1 *)
              if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
              else (if r0 > !t0 then t0 := r0;
                    if r1 < !t1 then t1 := r1)
            )
            else (* dy < 0. i.e., y0 > y1 *) (
              let r0 = (Box2.maxy b -. y0) /. dy in
              let r1 = (Box2.miny b -. y0) /. dy in
              if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
              else (if r0 > !t0 then t0 := r0;
                    if r1 < !t1 then t1 := r1)
            )
          );
          if !t1 >= 0. (* segment not dropped *) then (
            (* FIXME: The values of [t0] and [t1] are only linear
               estimates. Thus the resulting sampling is a [`Pt] one
               and it cannot be refined. *)
            if !t0 = 0. then (
              Of_sequence.jump st p0;
              if !t1 = 1. then (* whole segment *)
                Of_sequence.add st p1
              else
                Of_sequence.add st { t = p0.t +. !t1 *. (p1.t -. p0.t);
                                     x = x0 +. !t1 *. dx;  y = y0 +. !t1 *. dy;
                                     cost = 0. }
            )
            else ( (* t0 > 0 *)
              if !t1 = 1. then (
                Of_sequence.jump st { t = p0.t +. !t0 *. (p1.t -. p0.t);
                                      x = x0 +. !t0 *. dx;  y = y0 +. !t0 *. dy;
                                      cost = 0. };
                Of_sequence.add st p1
              )
              else (
                let ds = p1.t -. p0.t in
                Of_sequence.jump st { t = p0.t +. !t0 *. ds;
                                      x = x0 +. !t0 *. dx;  y = y0 +. !t0 *. dy;
                                      cost = 0. };
                Of_sequence.add st { t = p0.t +. !t1 *. ds;
                                     x = x0 +. !t1 *. dx;  y = y0 +. !t1 *. dy;
                                     cost = 0. };
              )
            )
          )
        )
      );
      if is_last !s then continue := false
      else s := !s.next;
    done;
    Of_sequence.close_with_viewport st b
  )


(** Uniform sampling *)
let uniform ?(n=100) f a b =
  if not(is_finite a && is_finite b) then
    invalid_arg "Curve_sampling.uniform: the endpoints a and b must be finite";
  if a = b then invalid_arg "Curve_sampling.uniform: empty interval";
  if n < 2 then
    invalid_arg "Curve_sampling.uniform: need at least 2 evaluations";
  let a, b = if a < b then a, b else b, a in
  let dx = (b -. a) /. float(n-1) in
  let st = Of_sequence.init () in
  for i = 0 to n - 1 do
    let x = a +. float i *. dx in
    let y = f x in
    Of_sequence.add st (point ~t:x ~x ~y)
  done;
  Of_sequence.close st

let of_path p =
  let st = Of_sequence.init () in
  List.iteri (fun i (x,y) ->
      Of_sequence.add st (point ~t:(float i) ~x ~y)
    ) p;
  Of_sequence.close st

;;
#if OCAML_VERSION >= (4, 7, 0)
(* Conversion from and to [Seq]. *)

let rec take_of_seq st i n seq =
  if i < n then
    match seq () with
    | Seq.Nil -> ()
    | Seq.Cons ((x,y), seq) ->
       Of_sequence.add st (point ~t:(float i) ~x ~y);
       take_of_seq st (i + 1) n seq

let of_seq ?(n=max_int) seq =
  let st = Of_sequence.init () in
  take_of_seq st 0 n seq;
  Of_sequence.close st

(* This is supposed to return a sequence from a "connected" sub-path
   defined by [first] and [last].  See [fold_points_incr_segments]. *)
let rec seq_of_subpath first last () =
  let p1 = first.p1 in
  if first == last then
    if is_valid p1 then Seq.Cons((p1.x, p1.y), Seq.empty) else Seq.Nil
  else
    Seq.Cons((p1.x, p1.y), seq_of_subpath first.next last)

let seq_of_subpath_start first last () =
  let p0 = first.p0 in
  if is_valid p0 then Seq.Cons((p0.x, p0.y), seq_of_subpath first last)
  else seq_of_subpath first last ()

let rec seq_of_paths seg () =
  (* Determine the next connected range. *)
  let seg_end = ref seg in
  while not(is_last !seg_end) && is_valid(!seg_end.p1)
        && !seg_end.p1 == !seg_end.next.p0 do
    seg_end := !seg_end.next
  done;
  Seq.Cons(seq_of_subpath_start seg !seg_end,
           if is_last !seg_end then Seq.empty
           else seq_of_paths !seg_end.next)

let to_seq t = if is_empty t then Seq.empty
               else seq_of_paths t.first
#endif

let rec add_points_before st t = function
  | [] -> []
  | (p :: tl) as points ->
     if p.t < t then (Of_sequence.add st p;  add_points_before st t tl)
     else points

(* [points] is a list of pre-computed points to be inserted in the
   sampling.  The points are assumed to be sorted in increasing
   order. *)
let almost_uniform ~n ?viewport ~points f a b =
  (* Assume [a] and [b] are finite and [a] < [b]. *)
  (* Bounding box of initial sampling; to be used as viewport *)
  let xmin = ref infinity in
  let xmax = ref neg_infinity in
  let ymin = ref infinity in
  let ymax = ref neg_infinity in
  let points = ref points in
  let st = Of_sequence.init () in
  let[@inline] add_pt t =
    points := add_points_before st t !points;
    let p = f t in
    Of_sequence.add st p;
    if p.x < !xmin then xmin := p.x;  (* ⇒ [x] not NaN *)
    if p.x > !xmax then xmax := p.x;
    if p.y < !ymin then ymin := p.y;
    if p.y > !ymax then ymax := p.y in
  let dt = (b -. a) /. float(n-1) in
  (* Slightly randomize points except for the first and last ones. *)
  add_pt a;
  for i = 1 to n - 2 do
    add_pt (a +. (float i +. Random.float 0.125 -. 0.0625) *. dt);
  done;
  add_pt b;
  List.iter (fun p -> Of_sequence.add st p) !points;
  let vp = match viewport with
    | None ->
       if is_finite !xmin && is_finite !xmax
          && is_finite !ymin && is_finite !ymax then
         Box2.v (P2.v !xmin !ymin) (Size2.v (!xmax -. !xmin) (!ymax -. !ymin))
       else
         Box2.unit
  | Some vp -> vp in
  Of_sequence.close_with_viewport st vp


module Cost = struct
  (* The cost of a point is a measure of the curvature of the curve at
     this point.  This requires the points before and after to be
     valid.  In case the point is invalid, or first, or last, it has a
     cost of 0.  If it is an endpoint of a segment with the other
     point invalid, the cost is set to 1. because the segment with the
     invalid point needs to be cut of too long to better determine the
     boundary.

     The cost of a point is apportioned to the segments of which it is
     an endpoint according to their relative lengths.  More precisely,
     the cost c of a point p is distributed on the segments s1 and s2
     (of respective lengths l1 and l2) it is an endpoint of as

       c * l1/(l1+l2) for s1 and c * l2/(l1+l2) for s2.

     In order to be able to update the cost of s1 without accessing
     s2, p.cost holds c/(l1+l2). *)
  type t = Box2.t -> point -> point -> point -> float

  (* Assume the 3 points are valid (no nan nor infinities).  However,
     some point (x,y) values may be identical. *)
  let estimate: t = fun vp p1 pm p2 ->
    let dx1m = (p1.x -. pm.x) /. Box2.w vp
    and dy1m = (p1.y -. pm.y) /. Box2.h vp in
    let dx2m = (p2.x -. pm.x) /. Box2.w vp
    and dy2m = (p2.y -. pm.y) /. Box2.h vp in
    let len1m = hypot dx1m dy1m in
    let len2m = hypot dx2m dy2m in
    (dx1m *. dx2m +. dy1m *. dy2m) /. (len1m *. len2m) +. 1.

  let _dist_line: t = fun vp p1 pm p2 ->
    (* x ← (x - Box2.minx vp) / (Box2.h vp) and similarly for y *)
    let dx21 = p2.x -. p1.x and dy21 = p2.y -. p1.y in
    let d21 = hypot (dx21 /. Box2.w vp) (dy21 /. Box2.h vp) in
    if d21 = 0. then 0. (* p1 and p2 have the same (x,y) *)
    else
      let c = p2.x *. p1.y -. p2.y *. p1.x in
      abs_float(dy21 *. pm.x -. dx21 *. pm.y +. c) /. d21

  (** Compute the cost of a segment according to the costs of its
     endpoints. *)
  let segment s =
    let dt = s.p1.t -. s.p0.t in
    dt**1.25 *. (s.p0.cost +. s.p1.cost)

  (** Assume the costs of the endpoints of [s] are up-to-date and
     insert [s] with the right priority.  If the segment is outside
     the viewport ([in_vp] is [false]), add it but never look at it
     again (the cost is low). *)
  let add_with_witness sampling s ~in_vp =
    let cost = if in_vp then segment s else neg_infinity in
    let w = PQ.witness_add sampling.seg cost s in
    s.witness <- Some w

  (** Update the cost of all points in the sampling and add segments
     to the priority queue. *)
  let compute t ~in_vp =
    if not(is_empty t) then (
      t.first.p0.cost <- 0.;
      let s = ref t.first in
      let p0_in_vp = ref (in_vp t.first.p0) in
      let p_in_vp = ref false in
      while not(is_last !s) do
        (* Not the last segment, so !s.next can be used. *)
        let p = !s.p1 in
        p_in_vp := false;
        if is_valid p then (
          p_in_vp := in_vp p;
          if p == !s.next.p0 then
            if is_valid !s.p0 && is_valid !s.next.p1 then
              p.cost <- estimate t.vp !s.p0 p !s.next.p1
            else p.cost <- 1. (* cut before or after [p] *)
          else ( (* Clean jump; seen as concatenation of 2 paths *)
            p.cost <- 0.;  !s.next.p0.cost <- 0.)
        )
        else p.cost <- 0.; (* [p] not valid *)
        add_with_witness t !s ~in_vp:(!p0_in_vp || !p_in_vp);
        s := !s.next;
        p0_in_vp := !p_in_vp;
      done;
      (* Last segment. *)
      t.last.p1.cost <- 0.;
      add_with_witness t t.last ~in_vp:(!p0_in_vp || in_vp t.last.p1);
    )

  (* Update the cost of [s.p0] and the cost of [s.prev]. *)
  let update_prev s cost =
    if not(is_first s) && s.prev.p1 == s.p0 then (
      (* If [s] is first or there is a cut before the right cost has
         already been set. *)
      s.p0.cost <- cost;
      (match s.prev.witness with
       | Some w -> PQ.increase_priority (segment s.prev) w
       | None -> assert false);
    ) [@@inline]

  let update_next s cost =
    if not(is_last s) && s.next.p0 == s.p1 then (
      s.p1.cost <- cost;
      (match s.next.witness with
       | Some w -> PQ.increase_priority (segment s.next) w
       | None -> assert false);
    ) [@@inline]
end


(* Adaptive sampling 2D
 ***********************************************************************)

(** Replace the segment [s] removed from the sampling [t] by [s']. *)
let replace_seg_by t ~s ~s' =
  if is_first s then (s'.prev <- s';  t.first <- s') else s.prev.next <- s';
  if is_last s  then (s'.next <- s';  t.last <- s')  else s.next.prev <- s'

(** Replace the segment [s] removed from the sampling [t] by 2
   segments, [s1] followed by [s2]. *)
let replace_seg_by2 t ~s ~s0 ~s1 =
  if is_first s then (s0.prev <- s0;  t.first <- s0) else s.prev.next <- s0;
  if is_last s  then (s1.next <- s1;  t.last <- s1)  else s.next.prev <- s1

let refine_gen ~n f ~in_vp sampling =
  let n = ref n in
  while !n > 0 do
    let s = PQ.delete_max sampling.seg in
    let p0 = s.p0 and p1 = s.p1 in
    (* let t = p0.t +. (0.4375 +. Random.float 0.125) *. (p1.t -. p0.t) in *)
    (* let t = p0.t +. (0.46875 +. Random.float 0.0625) *. (p1.t -. p0.t) in *)
    let t = p0.t +. 0.5 *. (p1.t -. p0.t) in
    (* let t = if is_last s || not(is_valid p0 && is_valid p1 && p1 == s.next.p0
     *                            && is_valid s.next.p1) then t else
     *           let p2 = s.next.p1 in
     *           (\* let n1 = p0.y -. p1.y and n2 = p1.x -. p0.x in
     *            * let v0 = n1 *. p0.x +. n2 *. p0.y in
     *            * let v1 = n1 *. p1.x +. n2 *. p1.y in
     *            * let v2 = n1 *. p2.x +. n2 *. p2.y in
     *            * let t' = _arg_max_quad p0.t v0 p1.t v1 p2.t v2 in *\)
     *           let t' = _arg_max_quad p0.t p0.y p1.t p1.y p2.t p2.y in
     *           let r = (t' -. p0.t) /. (p1.t -. p0.t) in
     *           if 0.1 <= r && r <= 0.9 then (
     *             printf "%e ∈ [%e, %e]\n" t' p0.t p1.t;
     *             t') else t in *)
    let p = f t in (* the caller is responsible to return a suitable point *)
    decr n;
    if is_valid p0 then
      if is_valid p1 then (
        let rec s0 = { p0; p1 = p;  prev = s.prev;  next = s1;
                       witness = None;  weight = 1. }
        and s1 = { p0 = p;  p1;  prev = s0;  next = s.next;
                   witness = None;  weight = 1. } in
        replace_seg_by2 sampling ~s ~s0 ~s1;
        (* Update costs of [p0] and [p1] and possibly of [prev] and
           [next] segments. *)
        let p_in_vp = ref false in
        if is_valid p then (
          (* FIXME: be more efficient, e.g. decrease the number of
             times lengths are computed and try to reduce the number
             of tests. *)
          p_in_vp := in_vp p;
          p.cost <- Cost.estimate sampling.vp p0 p p1;
          if is_valid s.prev.p0 then
            Cost.update_prev s0 (Cost.estimate sampling.vp s.prev.p0 p0 p);
          if is_valid s.next.p1 then
            Cost.update_next s1 (Cost.estimate sampling.vp p p1 s.next.p1);
        )
        else ( (* [p] is invalid.  This creates a cut between [p0] and [p1]. *)
          p.cost <- 0.;
          Cost.update_prev s0 1.;
          Cost.update_next s1 1.;
        );
        Cost.add_with_witness sampling s0 ~in_vp:(!p_in_vp || in_vp p0);
        Cost.add_with_witness sampling s1 ~in_vp:(!p_in_vp || in_vp p1);
      )
      else (* [p0] valid but not [p1]. *)
        if is_valid p then (
          let rec s0 = { p0; p1 = p;  prev = s.prev;  next = s1;
                         witness = None;  weight = 1. }
          and s1 = { p0 = p;  p1;  prev = s0;  next = s.next;
                     witness = None;  weight = 1. } in
          replace_seg_by2 sampling ~s ~s0 ~s1;
          p.cost <- 1.;
          Cost.update_prev s0 1.;
          let p_in_vp = in_vp p in
          Cost.add_with_witness sampling s0 ~in_vp:(p_in_vp || in_vp p0);
          Cost.add_with_witness sampling s1 ~in_vp:p_in_vp;
        )
        else ( (* [p] invalid, drop segment [p, p1].  Cost(p0) stays
                  1.  We can see this as reducing the uncertainty of
                  the boundary in the segment [p0, p1]. *)
          let s0 = { p0; p1 = p;  prev = s.prev;  next = s.next;
                     witness = None;  weight = 0.5 *. s.weight } in
          replace_seg_by sampling ~s ~s':s0;
          p.cost <- 0.;
          Cost.add_with_witness sampling s0 ~in_vp:(in_vp p0);
        )
    else ( (* [p0] not valid, thus [p1] is valid. *)
      if is_valid p then (
        let rec s0 = { p0; p1 = p;  prev = s.prev;  next = s1;
                       witness = None;  weight = 1. }
        and s1 = { p0 = p;  p1;  prev = s0;  next = s.next;
                   witness = None;  weight = 1. } in
        replace_seg_by2 sampling ~s ~s0 ~s1;
        p.cost <- 1.;
        Cost.update_next s1 1.;
        let p_in_vp = in_vp p in
        Cost.add_with_witness sampling s0 ~in_vp:p_in_vp;
        Cost.add_with_witness sampling s1 ~in_vp:(p_in_vp || in_vp p1);
      )
      else ( (* [p] invalid, drop segment [p0, p].  Cost(p1) stays 1. *)
        let s1 = { p0 = p;  p1;  prev = s.prev;  next = s.next;
                   witness = None;  weight = 0.5 *. s.weight } in
        replace_seg_by sampling ~s ~s':s1;
        p.cost <- 0.;
        Cost.add_with_witness sampling s1 ~in_vp:(in_vp p1);
      )
    )
  done;
  sampling

let always_in_vp _p = true

let param_gen fn_name ?(n=100) ?viewport ~init ~init_pt f a b =
  if not(is_finite a && is_finite b) then
    invalid_arg(fn_name ^ ": a and b must be finite");
  if a = b then invalid_arg(fn_name ^ ": empty interval [a,b]");
  let a, b = if a < b then a, b else b, a in
  (* Make sure all t are finite and in the interval [a,b]. *)
  let points = List.fold_left (fun l t ->
                   if a <= t && t <= b then f t :: l
                   else l) [] init in
  let points = List.fold_left (fun l p ->
                   if a <= p.t && p.t <= b then p :: l else l) points init_pt in
  let points = List.sort (fun p1 p2 -> compare p1.t p2.t) points in
  let n0 = truncate(0.1 *. float n) in
  let n0 = if n0 <= 10 then 10 else n0 in
  let sampling = almost_uniform ~n:n0 ?viewport ~points f a b in
  (* to_file sampling ("/tmp/" ^ Filename.basename Sys.argv.(0) ^ "0.dat"); *)
  let in_vp = match viewport with
    | None -> always_in_vp
    | Some vp -> (fun p -> Box2.mem (P2.v p.x p.y) vp) in
  Cost.compute sampling ~in_vp;
  refine_gen ~n:(n - n0) f sampling ~in_vp

let fn ?n ?viewport ?(init=[]) ?(init_pt=[]) f a b =
  let init_pt = List.map (fun (x,y) -> point ~t:x ~x ~y) init_pt in
  let f x = let y = f x in point ~t:x ~x ~y in
  param_gen "Curve_sampling.fn" ?n ?viewport ~init ~init_pt f a b

let param ?n ?viewport ?(init=[]) ?(init_pt=[]) f a b =
  let init_pt = List.map (fun (t,(x,y)) -> point ~t ~x ~y) init_pt in
  let f t = let (x, y) = f t in point ~t ~x ~y in
  param_gen "Curve_sampling.param" ?n ?viewport ~init ~init_pt f a b



(** Sub-module using Gg point representation. *)
module P2 = struct

  let uniform ?(n=100) f a b =
    if not(is_finite a && is_finite b) then
      invalid_arg "Curve_sampling.P2.uniform: the endpoints a and b \
                   must be finite";
    if a = b then invalid_arg "Curve_sampling.P2.uniform: empty interval";
    if n < 2 then
      invalid_arg "Curve_sampling.P2.uniform: need at least 2 evaluations";
    let a, b = if a < b then a, b else b, a in
    let dt = (b -. a) /. float(n-1) in
    let st = Of_sequence.init () in
    for i = 0 to n - 1 do
      let t = a +. float i *. dt in
      let p = f t in
      Of_sequence.add st (point ~t ~x:(P2.x p) ~y:(P2.y p))
    done;
    Of_sequence.close st

  let of_path p =
    let st = Of_sequence.init () in
    List.iteri (fun i p ->
        Of_sequence.add st (point ~t:(float i) ~x:(P2.x p) ~y:(P2.y p))
      ) p;
    Of_sequence.close st

  type point_or_cut = Point of P2.t | Cut

  let to_Point p = Point (P2.v p.x p.y) [@@inline]

  let to_list t =
    fold_points_decr t ~init:[] (fun l p -> to_Point p :: l)
                           ~cut:(fun l -> Cut :: l)
  ;;
#if OCAML_VERSION >= (4, 7, 0)
  (* Conversion from and to [Seq]. *)

  let rec take_of_seq st i n seq =
    if i < n then
      match seq () with
      | Seq.Nil -> ()
      | Seq.Cons (p, seq) ->
         Of_sequence.add st (point ~t:(float i) ~x:(P2.x p) ~y:(P2.y p));
         take_of_seq st (i + 1) n seq

  let of_seq ?(n=max_int) seq =
    let st = Of_sequence.init () in
    take_of_seq st 0 n seq;
    Of_sequence.close st

  (* See [fold_points_incr_segments]. *)
  let rec seq_of_seg ~prev_p1 ~last_is_cut seg () =
    if seg.p0 == prev_p1 then (* p0 treated *)
      seq_p1 seg ()
    else if last_is_cut then
      if is_valid seg.p0 then seq_valid_p0 seg ()
      else seq_valid_p1 seg () (* invalid p0 ⇒ valid p1 *)
    else
      Seq.Cons(Cut, if is_valid seg.p0 then seq_valid_p0 seg
                    else seq_valid_p1 seg)
  and seq_maybe_last ~last_is_cut seg () =
    if is_last seg then Seq.Nil
    else seq_of_seg ~prev_p1:seg.p1 ~last_is_cut seg.next ()
  and seq_valid_p0 seg () =
    Seq.Cons(to_Point seg.p0, seq_p1 seg)
  and seq_p1 seg () =
    if is_valid seg.p1 then
      Seq.Cons(to_Point seg.p1, seq_maybe_last seg ~last_is_cut:false)
    else (* p1 invalid ⇒ p0 valid ⇒ no cut right before.  However, do
            not output a Cut if last. *)
      if is_last seg then Seq.Nil
      else Seq.Cons(Cut, seq_of_seg ~prev_p1:seg.p1 ~last_is_cut:true seg.next)
  and seq_valid_p1 seg () =
    Seq.Cons(to_Point seg.p1, seq_maybe_last seg ~last_is_cut:false)

  let to_seq t =
    if is_empty t then Seq.empty
    else seq_of_seg t.first ~prev_p1:dummy_point ~last_is_cut:true
#endif

  let param ?n ?viewport ?(init=[]) ?(init_pt=[]) f a b =
    let init_pt =
      List.map (fun (t,p) -> point ~t ~x:(P2.x p) ~y:(P2.y p)) init_pt in
    let f t = let p = f t in point ~t ~x:(P2.x p) ~y:(P2.y p) in
    param_gen "Curve_sampling.P2.param" ?n ?viewport ~init ~init_pt f a b
end



module Internal = struct
  let write_points t fname =
    let fh = open_out fname in
    fold_points t ~init:()
      (fun () p -> fprintf fh "%e\t%e\t%e\n" p.x p.y p.cost)
      ~cut:(fun () -> output_char fh '\n');
    close_out fh

  let by_t (_, s1) (_, s2) = compare s1.p0.t s2.p0.t

  let write_segments t fname =
    let fh = open_out fname in
    (* Use the costs from the priority queue because some may have
       been modified (e.g. for dropped segments). *)
    let segs = PQ.foldi t.seg ~init:[] ~f:(fun l cost seg ->
                   (cost, seg) :: l) in
    let segs = List.sort by_t segs in
    List.iter (fun (cost, seg) ->
        let p0 = seg.p0 and p1 = seg.p1 in
        let tm = p0.t +. 0.5 *. (p1.t -. p0.t) in
        fprintf fh "%e\t%e\t%e\t%e\t%e\t%e\t%e\t%e\n"
          tm p0.t p0.x p0.y  p1.t p1.x p1.t cost;
      )
      segs;
    close_out fh

  let cost_max t = PQ.max_priority t.seg
end
