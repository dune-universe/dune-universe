module OCSA = OCamlStandard.ArrayLabels

type 'a t = 'a array

let (empty: 'a t) = [||]

let parent i =
  (i - 1) / 2

let children i =
  (2 * i + 1, 2 * i + 2)

module Invariants = struct
  type t =
    | IsMaxHeap

  exception BrokenInvariants of t list

  (*BISECT-IGNORE-BEGIN*)
  let repr = function
    | IsMaxHeap -> "IsMaxHeap"

  let _ = Exception.register_printer (function
    | BrokenInvariants broken_invariants ->
      Some (Format.apply "Broken binary heap invariants: %s" (broken_invariants |> List.map ~f:repr |> List.join_string_list ~sep:", "))
    | _ ->
      None
  )
  (*BISECT-IGNORE-END*)

  let is_max_heap xs ~cmp =
    IntRange.make (OCSA.length xs)
    |> IntRange.for_all ~f:(fun i ->
      match cmp xs.(i) xs.(parent i) with
        | Compare.LT
        | Compare.EQ -> true
        | Compare.GT -> false
    )

  let validate xs ~cmp =
    match
      [
        (is_max_heap ~cmp, IsMaxHeap);
      ]
      |> List.filter_map ~f:(fun (predicate, invariant) ->
        Option.some_if' (not (predicate xs)) invariant
      )
    with
      | [] -> xs
      | broken_invariants -> Exception.raise (BrokenInvariants broken_invariants)
end

let swap xs i j =
  let x = xs.(i) in
  xs.(i) <- xs.(j);
  xs.(j) <- x

let add xs ~cmp x =
  let xs = OCSA.append xs [|x|] in
  let rec aux = function
    | 0 -> ()
    | i ->
      let p = parent i in
      match cmp xs.(i) xs.(p) with
        | Compare.LT
        | Compare.EQ -> ()
        | Compare.GT -> begin
          swap xs i p;
          aux p
        end
  in
  aux ((OCSA.length xs) - 1);
  xs

(* @todo Define DEBUG when compiling with jbuilder --dev *)
(* #ifdef DEBUG *)
let add xs ~cmp x =
  Invariants.(validate ~cmp (add (validate ~cmp xs) ~cmp x))
(* #endif *)

let max xs =
  xs.(0)

let pop_max xs ~cmp =
  let len = OCSA.length xs - 1 in
  if len = 0 then empty else
  let last = xs.(len) in
  let xs = OCSA.sub xs ~pos:0 ~len in
  xs.(0) <- last;
  let rec aux i =
    let (l, r) = children i in
    if r < len then begin
      match (cmp xs.(i) xs.(l), cmp xs.(i) xs.(r)) with
        | ((Compare.GT | Compare.EQ), (Compare.GT | Compare.EQ)) ->
          ()
        | _ ->
          let c =
            match cmp xs.(l) xs.(r) with
              | Compare.LT
              | Compare.EQ -> r
              | Compare.GT -> l
          in
          swap xs i c;
          aux c
    end else if l < len then begin
      match cmp xs.(i) xs.(l) with
        | Compare.GT | Compare.EQ ->
          ()
        | Compare.LT ->
          swap xs i l;
          aux l (* @todo Is this recursion needed? *)
    end
  in
  aux 0;
  xs

let pop_max xs ~cmp =
  Invariants.(xs |> validate ~cmp |> pop_max ~cmp |> validate ~cmp)

module Tests = struct
  open Testing

  let make name ?(init=empty) fs expected =
    name >: (lazy (
      let heap = List.fold ~init ~f:(fun heap f -> f heap) fs in
      check_int_list ~expected (OCSA.to_list heap)
    ))

  let add x xs =
    add xs ~cmp:Int.compare x

  let pop_max xs =
    pop_max xs ~cmp:Int.compare

  let test = "BinaryHeap" >:: [
    make "empty" [] [];
    "add" >:: (
      let make xs expected =
        let name = List.repr ~repr_a:Int.repr xs
        and fs = List.map ~f:(fun x -> add x) xs in
        make name fs expected
      in
      [
        make [0] [0];
        make [0; 1] [1; 0];
        make [1; 0] [1; 0];
        make [0; 1; 2] [2; 0; 1];
        make [0; 2; 1] [2; 0; 1];
        make [1; 0; 2] [2; 0; 1];
        make [1; 2; 0] [2; 1; 0];
        make [2; 0; 1] [2; 0; 1];
        make [2; 1; 0] [2; 1; 0];
        make [0; 0; 0; 0; 0; 0; 0; 1] [1; 0; 0; 0; 0; 0; 0; 0];
        make [1; 0; 0; 0; 0; 0; 0; 0] [1; 0; 0; 0; 0; 0; 0; 0];
      ]
    );
    "pop_max" >:: (
      let make xs expected =
        let name = List.repr ~repr_a:Int.repr xs in
        make name ~init:(OCSA.of_list xs) [pop_max] expected
      in
      [
        make [1] [];
        make [2; 1] [1];
        make [3; 2; 1] [2; 1];
        make [3; 1; 2] [2; 1];
        make [4; 3; 2; 1] [3; 1; 2];
        make [4; 3; 1; 2] [3; 2; 1];
        make [4; 2; 3; 1] [3; 2; 1];
      ]
    );
  ]
end
