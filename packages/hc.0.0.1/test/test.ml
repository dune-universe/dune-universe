open Hc

type hidden = view hash_consed

and view = Leaf of int | Node of int * hidden * hidden

module H = struct
  type t = view

  let equal x y =
    match (x, y) with
    | Leaf m, Leaf n ->
        m = n
    | Node (m, l1, r1), Node (n, l2, r2) ->
        m = n && l1 == l2 && r1 == r2
    | _ ->
        false

  let hash = function
    | Leaf n ->
        n
    | Node (n, l, r) ->
        (19 * ((19 * n) + l.tag)) + r.tag + 2
end

module HTree = Make (H)

let leaf n = HTree.hashcons (Leaf n)

let node v l h = HTree.hashcons (Node (v, l, h))

let extract x = match x.node with Leaf x | Node (x, _, _) -> x

let rec get_fibo n =
  if n < 0 then failwith "get_fibo" ;
  if n < 2 then leaf n
  else
    let a = get_fibo (n - 1) in
    let b = get_fibo (n - 2) in
    node (extract a + extract b) a b

let _ =
  (* 1 *)
  let n1 = leaf 1 in
  let n2 = leaf 2 in
  let n3 = node 3 n1 n2 in
  let n3' = node 3 n1 n2 in
  assert (n3 == n3') ;
  let n4 = node 4 n3 n3' in
  let n4' = node 4 n3' n3 in
  assert (n4 == n4') ;
  let s = HTree.stats () in
  assert (s.num_bindings = 4) ;
  (* 2 *)
  HTree.clear () ;
  let n = 30 in
  let g = get_fibo n in
  let s = HTree.stats () in
  assert (s.num_bindings = n + 1) ;
  let res = extract g in
  assert (res = 832040) ;
  Format.printf "Tests are OK !@."
