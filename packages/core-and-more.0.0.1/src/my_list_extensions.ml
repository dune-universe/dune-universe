open Core
open Util
open Algebra

let sorting
    ~cmp:(cmp:'a comparer)
    (l:'a list)
  : (Permutation.t) =
  let l_with_is = List.mapi ~f:(fun i x -> (x,i)) l in
  let sorted_l_with_is =
    List.sort
      ~compare:(fun (x1,_) (x2,_) -> cmp x1 x2)
      l_with_is
  in
  Permutation.create
    (List.map ~f:snd sorted_l_with_is)

let sorting_and_sort
    ~cmp:(cmp:'a comparer)
    (l:'a list)
  : (Permutation.t * 'a list) =
  let l_with_is = List.mapi ~f:(fun i x -> (x,i)) l in
  let sorted_l_with_is =
    List.sort
      ~compare:(fun (x1,_) (x2,_) -> cmp x1 x2)
      l_with_is
  in
  (Permutation.create
     (List.map ~f:snd sorted_l_with_is),
   List.map ~f:fst sorted_l_with_is)

let rec zip3
    (l1:'a list)
    (l2:'b list)
    (l3:'c list)
  : ('a * 'b * 'c) list option =
  begin match (l1,l2,l3) with
    | (h1::t1,h2::t2,h3::t3) ->
      Option.map ~f:(fun t -> (h1,h2,h3)::t) (zip3 t1 t2 t3)
    | ([],[],[]) -> Some []
    | _ -> None
  end

let zip3_exn
    (l1:'a list)
    (l2:'b list)
    (l3:'c list)
  : ('a * 'b * 'c) list =
  Option.value_exn (zip3 l1 l2 l3)

let rec sublist_on_sorted
    ~(cmp:'a comparer)
    (l1:'a list)
    (l2:'a list)
  : bool =
  begin match (l1,l2) with
    | ([],_) -> true
    | (h1::t1,h2::t2) ->
      begin match make_matchable (cmp h1 h2) with
      | EQ -> sublist_on_sorted ~cmp t1 t2
      | LT -> false
      | GT -> sublist_on_sorted ~cmp l1 t2
      end
    | _ -> false
  end

let sub_multi_set
    ~(cmp:'a comparer)
    (l1:'a list)
    (l2:'a list)
  : bool =
  let l1 = List.sort ~compare:cmp l1 in
  let l2 = List.sort ~compare:cmp l2 in
  sublist_on_sorted ~cmp l1 l2

let or_unequal_lengths_to_option
    (x:'a List.Or_unequal_lengths.t)
  : 'a option =
  begin match x with
    | Ok x -> Some x
    | _ -> None
  end
