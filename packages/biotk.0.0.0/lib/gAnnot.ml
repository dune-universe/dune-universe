open Core_kernel
open CFStream
open Stream.Infix

module Map = struct
  include Map.Make(String)

  let to_stream t = Stream.of_list (to_alist t)
  let of_stream xs =
    Stream.fold xs ~init:empty ~f:(fun accu (key,data) -> set accu ~key ~data)
end

module Accu = Biocaml_unix.Accu

module Selection = struct
  module Iset = Biocaml_unix.Iset
  type t = Iset.t Map.t

  let empty = Map.empty
  let add sel GLoc.{ chr ; lo ; hi } =
    let set_chr =
      match Map.find sel chr with
      | None -> Iset.empty
      | Some s -> s
    in
    let set_chr = Iset.add_range set_chr lo hi in
    Map.set sel ~key:chr ~data:set_chr

  let inter u v =
    Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
        match Map.find v k with
        | Some set_v -> Map.set accu ~key:k ~data:(Iset.inter set_u set_v)
        | None -> accu
      )

  let union u v =
    let keys = List.dedup_and_sort ~compare:String.compare (Map.keys u @ Map.keys v) in
    List.fold keys ~init:Map.empty ~f:(fun accu k ->
        Map.set accu ~key:k ~data:(
          Iset.union
            (Option.value (Map.find u k) ~default:Iset.empty)
            (Option.value (Map.find v k) ~default:Iset.empty)
        )
      )

  let diff u v =
    Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
        let set_u' =
          match Map.find v k with
          | Some set_v -> Iset.diff set_u set_v
          | None -> set_u
        in
        Map.set ~key:k ~data:set_u' accu
      )

  let size x =
    Map.fold x ~init:0 ~f:(fun ~key:_ ~data:set accu -> Iset.cardinal set + accu)

  let overlap sel GLoc.{ chr ; lo ; hi } = Iset.(
      match Map.find sel chr with
      | Some x ->
        inter (add_range empty lo hi) x
        |> cardinal
      | None -> 0
    )

  let intersects sel GLoc.{ chr ; lo ; hi } =
    Option.value_map
      (Map.find sel chr)
      ~default:false
      ~f:(fun x -> Iset.intersects_range x lo hi)

  let to_stream sel =
    Map.to_stream sel
    |> Stream.map ~f:(fun (chr, s) ->
        Stream.map (Iset.to_stream s) ~f:(fun (lo, hi) ->
            GLoc.{ chr ; lo ; hi }
          )
      )
    |> Stream.concat

  let of_stream e =
    let accu =
      Accu.create
        ~bin:(fun x -> x.GLoc.chr)
        ~zero:Iset.empty
        ~add:GLoc.(fun loc x -> Iset.add_range x loc.lo loc.hi)
        ()
    in
    Stream.iter ~f:(fun loc -> Accu.add accu loc loc) e ;
    Map.of_stream (Accu.stream accu)
end

module LMap = struct
  module T = Biocaml_unix.Interval_tree

  type 'a t = 'a T.t Map.t

  let empty = Map.empty

  let intersects lmap { GLoc.chr ; lo ; hi }  =
    Option.value_map
      (Map.find lmap chr)
      ~default:false
      ~f:(fun x -> T.intersects x ~low:lo ~high:hi)

  let closest lmap { GLoc.chr ; lo ; hi } =
    Option.bind
      (Map.find lmap chr)
      ~f:(fun x ->
          try
            let lo, hi, label, d = T.find_closest lo hi x in
            Some ({ GLoc.chr ; lo ; hi }, label, d)
          with T.Empty_tree -> None
        )

  let intersecting_elems lmap { GLoc.chr ; lo ; hi } =
    match Map.find lmap chr with
    | Some x ->
      T.find_intersecting_elem lo hi x
      /@ (fun (lo, hi, x) -> { GLoc.chr ; lo ; hi }, x)
    | None -> Stream.empty ()

  let to_stream lmap =
    (Map.to_stream lmap)
    /@ (fun (chr, t) ->
        Stream.map
          ~f:(fun (lo, hi, x) -> { GLoc.chr ; lo ; hi }, x)
          (T.to_stream t))
    |> Stream.concat

  let of_stream e =
    let accu =
      Accu.create
        ~bin:GLoc.(fun l -> l.chr)
        ~zero:T.empty
        ~add:GLoc.(fun (l, v) -> T.add ~data:v ~low:l.lo ~high:l.hi)
        ()
    in
    Stream.iter ~f:(fun (loc, value) -> Accu.add accu loc (loc, value)) e ;
    Map.of_stream (Accu.stream accu)

  let add m k v =
    let chr = k.GLoc.chr  in
    let t = Option.value ~default:T.empty (Map.find m chr) in
    let t = T.(add t ~data:v ~low:k.lo ~high:k.hi) in
    Map.set m ~key:chr ~data:t
end

module LSet = struct
  module T = Biocaml_unix.Interval_tree

  type t = unit T.t Map.t

  let empty = Map.empty

  let intersects = LMap.intersects

  let closest lset loc =
    Option.map (LMap.closest lset loc) ~f:(fun (loc', (), d) -> loc', d)

  let intersecting_elems lset loc =
    LMap.intersecting_elems lset loc /@ fst

  let to_stream lset = LMap.to_stream lset /@ fst
  let of_stream e = e /@ (fun x -> x, ()) |> LMap.of_stream

end


module LAssoc = struct
  type 'a t = (GLoc.t * 'a) list

  let compare (x, _) (y, _) = GLoc.compare x y

  let of_alist xs =
    List.sort ~compare xs

  let of_list xs ~f = of_alist (List.map xs ~f:(fun x -> f x, x))

  let to_alist xs = xs

  let filter xs ~f = List.filter xs ~f:(fun (loc, value) -> f loc value)

  let fold_neighbors xs ys ~init ~f =
    let rec main_loop acc xs ys =
      match xs with
      | [] -> List.rev acc
      | ((x_loc, x_val) as x) :: xs_tail ->
        let ys = drop_until ys x_loc in
        let r = neighbor_loop x ys ~init:(init x_loc x_val) in
        let acc = (fst x, r) :: acc in
        main_loop acc xs_tail ys

    and drop_until ys x_loc =
      match ys with
      | [] -> []
      | (y_loc, _) :: tail_ys ->
        if GLoc.strictly_before y_loc x_loc then drop_until tail_ys x_loc
        else ys

    and neighbor_loop ((x_loc, _) as x) ys ~init =
      match ys with
      | [] -> init
      | (y_loc, y_val) :: tail_ys ->
        if GLoc.intersects y_loc x_loc then
          neighbor_loop x tail_ys ~init:(f y_loc y_val init)
        else init
    in
    main_loop [] xs ys

  let example1 = [
    GLoc.{ chr = "a" ; lo = 0 ; hi = 4 }, 1 ;
    GLoc.{ chr = "a" ; lo = 40 ; hi = 400 }, 1 ;
    GLoc.{ chr = "a" ; lo = 90 ; hi = 110 }, 1 ;
    GLoc.{ chr = "a" ; lo = 91 ; hi = 92 }, 1 ;
    GLoc.{ chr = "b" ; lo = 91 ; hi = 92 }, 1 ;
  ]

  let%test "fold_neighbors_1" =
    Int.(
      fold_neighbors example1 example1 ~init:(fun _ _ -> 0) ~f:(fun _ _ acc -> acc + 1)
      |> List.sum (module Int) ~f:(fun (_, d) -> d)
         = 11
    )

  module type Topology = sig
    type t = GLoc.t
    val dist : t -> t -> int option
    val compare : t -> t -> int
  end

  module Interval_topology = struct
    type t = GLoc.t
    let dist = GLoc.dist
    let compare = GLoc.compare
  end

  module Point_topology = struct
    type t = GLoc.t
    let midpoint u = GLoc.(u.hi + u.lo) / 2

    let dist (u : GLoc.t) (v : GLoc.t) =
      if String.(u.chr = v.chr) then
        Some (Int.abs (midpoint u - midpoint v))
      else None

    let midloc u = u.GLoc.chr, midpoint u

    let compare (u : GLoc.t) (v : GLoc.t) =
      Caml.compare (midloc u) (midloc v)
  end

  module Score = struct
    type t = {
      value : int ;
      weight : int ;
    }

    let compare x y =
      Caml.compare (x.value, y.weight) (y.value, x.weight)

    let%test "matching_score_compare" =
      compare { value = 3 ; weight = 6 } { value = 2 ; weight = 3 } > 0

    let gt x y = compare x y > 0
    let zero = { value = 0 ; weight = 0 }

    let add_match s d = { value = s.value + 1 ; weight = s.weight + d }
  end

  module Matching(T : Topology) = struct
    type choice = Left | Match | Right

    type trace = {
      score : Score.t ;
      choice : choice ;
    }

    let dist (u, _) (v, _) = T.dist u v

    let is_before (u, _) (v, _) =
      T.compare u v <= 0

    let score_step max_dist xs ys score_rec i j =
      if i < 0 then      { score = Score.zero ; choice = Right }
      else if j < 0 then { score = Score.zero ; choice = Left }
      else
        let score_rec i j = (score_rec i j).score in
        let edge_case () =
          if is_before xs.(i) ys.(j) then
            { score = score_rec i (j - 1) ; choice = Right }
          else
            { score = score_rec (i - 1) j ; choice = Left }
        in
        match dist xs.(i) ys.(j) with
        | None -> edge_case ()
        | Some d when d > max_dist -> edge_case ()
        | Some d ->
          let left_score = score_rec (i - 1) j in
          let right_score = score_rec i (j - 1) in
          let match_score = Score.add_match (score_rec (i - 1) (j - 1)) d in
          match Score.(gt left_score match_score, gt left_score right_score, gt match_score right_score) with
          | true, true, _ -> { score = left_score ; choice = Left }
          | false, _, true -> { score = match_score ; choice = Match }
          | _, false, false -> { score = right_score ; choice = Right }
          | false, true, false
          | true, false, true -> assert false

    let memo_rec ff =
      let open Caml in
      let h = Hashtbl.create 0 in
      let rec f x y  =
        try Hashtbl.find h (x,y)
        with Not_found ->
          let v = ff f x y in
          Hashtbl.add h (x,y) v;
          v
      in f

    let matching ~max_dist xs ys =
      let f = memo_rec (score_step max_dist xs ys) in
      let rec loop i j acc =
        if i < 0 && j < 0 then acc
        else
          let trace = f i j in
          match trace.choice with
          | Left  -> loop (i - 1) j (`Left xs.(i) :: acc)
          | Right -> loop i (j - 1) (`Right ys.(j) :: acc)
          | Match -> loop (i - 1) (j - 1) (`Match (xs.(i), ys.(j)) :: acc)
      in
      loop (Array.length xs - 1) (Array.length ys - 1) []
  end

  let matching ~mode ~max_dist xs ys =
    let module T = (val (
        match mode with
        | `Interval -> (module Interval_topology)
        | `Point -> (module Point_topology)
      ) : Topology)
    in
    let module M = Matching(T) in
    M.matching ~max_dist (Array.of_list xs) (Array.of_list ys)

  type matching =
    [ `Match of (GLoc.t * unit) * (GLoc.t * unit)
    |  `Left of (GLoc.t * unit)
    |  `Right of (GLoc.t * unit)] list
  [@@deriving sexp]

  let%test_module "MATCHING" = (module struct
    let loc lo hi = GLoc.{ chr = "chr" ; lo ; hi }, ()
    let ( = ) = Caml.( = )

    let%test "matching_1" = matching ~mode:`Point ~max_dist:10 [] [] = []

    let i1 = loc 1 3

    let%test "matching_2" =
      matching ~mode:`Point ~max_dist:10 [i1] [] = [`Left i1]

    let i2 = loc 2 3

    let i3 = loc 7 8

    let%test "matching_3" =
      matching ~mode:`Point ~max_dist:10 [i1] [i2] = [`Match (i1, i2)]

    let _print_match m =
      print_endline (Sexp.to_string_hum (sexp_of_matching m))

    let%test "matching_4" =
      matching ~mode:`Interval ~max_dist:10 [i1;i3] [i2] = [`Match (i1, i2) ; `Left i3]

    let%test "matching_5" =
      matching ~mode:`Point ~max_dist:10 [i1;i3] [i2] = [`Match (i1, i2) ; `Left i3]

    let i4 = loc 3 4

    let i5 = loc 12 15

    let%test "matching_6" =
      matching ~mode:`Interval ~max_dist:10 [i1;i4;i5] [i2;i3] = [`Match (i1, i2) ; `Match (i4, i3) ; `Left i5]
  end)
end
