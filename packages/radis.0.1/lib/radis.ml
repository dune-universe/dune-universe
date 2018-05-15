(*****************************************************************************)
(*  Copyright (C) 2018 Romain Calascibetta                                   *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation.                               *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

module type KEY =
sig
  type t

  val equal: t -> t -> bool
  val get: t -> int -> char
  val length: t -> int
end

type 'a sequence = ('a -> unit) -> unit

module type S =
sig
  include Map.S

  val to_sequence: 'a t -> (key * 'a) sequence
  val pp: key Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end

module Option =
struct
  let bind f = function Some v -> f v | None -> None
  let map f = function Some v -> Some (f v) | None -> None
  let (>>=) v f = bind f v
  let (>|=) v f = map f v
end

module Make (Key: KEY): S with type key = Key.t =
struct
  type key = Key.t

  module Compare =
  struct
    type t =
      | Eq
      | Prefix
      | Contain
      | Inf of int
      | Sup of int

    let unsafe_code s i : int = Obj.magic (Key.get s i)
    exception Break

    let fast_diff a b off len =
      if off = len then Eq
      else
        let i = ref off in
        let c1 = ref (unsafe_code a off) in
        let c2 = ref (unsafe_code b off) in

        try
          if !c1 <> !c2 then raise Break;
          incr i;

          while !i < len
          do
            c1 := unsafe_code a !i;
            c2 := unsafe_code b !i;

            if !c1 <> !c2 then raise Break else incr i done;
          Eq
        with Break -> if !c1 < !c2 then Inf !i else Sup !i

    let fast_diff a b off lena lenb =
      if lena = lenb
      then fast_diff a b off lena (* = lenb *)
      else
      if lena < lenb
      then match fast_diff a b off lena with
        | Eq -> Prefix | v -> v
      else match fast_diff a b off lenb with
        | Eq -> Contain | v -> v

    let compare = fast_diff
  end

  let table =
    [| 0;1;2;2;3;3;3;3;4;4;4;4;4;4;4;4;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;
       6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;
       7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;
       7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;
       8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;
       8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;
       8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;
       8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8 |]

  let ffs byte = Array.unsafe_get table byte

  let critbit c1 c2 =
    if c1 = c2
    then raise Not_found
    else ffs ((Char.code c1) lxor (Char.code c2)) - 1

  type 'a node =
    | L of Key.t * 'a
    | T of 'a node * Key.t * 'a
    | B of 'a node * 'a node * int * int

  type 'a t = 'a node option
  type nonrec 'a sequence = 'a sequence

  let empty = None

  let is_empty = function
    | None -> true
    | Some _ -> false

  let singleton k v = Some (L (k, v))

  let rec choose = function
    | L (k, v) -> (k, v)
    | T (_, k, v) -> (k, v)
    | B (l, _, _, _) -> choose l

  let choose_opt t = let open Option in t >|= choose

  let choose = function
    | None -> raise Not_found
    | Some tree -> choose tree

  let rec first_key = function
    | L (k, _) -> k
    | T (_, k, _) -> k
    | B (l, _, _, _) ->
      first_key l (* XXX(dinosaure): could be optimized to took the shortest
                     path between [r] and [l]. *)

  let rec bind key off keylen value tree = match tree with
    | L (k, v) ->
         (let kl = Key.length k in
          match Compare.compare key k off keylen kl with
          | Compare.Eq -> L (k, value) (* replace *)
          | Compare.Prefix -> T (tree, key, value)
          | Compare.Contain -> T (L (key, value), k, v)
          | Compare.Inf p ->
             let b = critbit (Key.get key p) (Key.get k p) in
             B (L (key, value), tree, p, b)
          | Compare.Sup p ->
             let b = critbit (Key.get key p) (Key.get k p) in
             B (tree, L (key, value), p, b))
    | T (m, k, v) ->
         (let kl = Key.length k in
          match Compare.compare key k off keylen kl with
          | Compare.Eq -> T (m, k, value) (* replace *)
          | Compare.Prefix -> T (tree, key, value)
          | Compare.Contain -> T (bind key kl keylen value m, k, v)
          | Compare.Inf p ->
             let b = critbit (Key.get key p) (Key.get k p) in
             B (L (key, value), tree, p, b)
          | Compare.Sup p ->
             let b = critbit (Key.get key p) (Key.get k p) in
             B (tree, L (key, value), p, b))
    | B (l, r, i, b) ->
       if keylen > i
       then if ((Char.code (Key.get key i)) land (1 lsl b)) = 0
            then B (bind key i keylen value l, r, i, b)
            else B (l, bind key i keylen value r, i, b)
       else let k = first_key l in
            match Compare.compare key k off keylen keylen with
            | Compare.Eq | Compare.Prefix -> T (tree, key, value)
            | Compare.Contain -> B (bind key i keylen value l, r, i, b)
            | Compare.Inf p ->
               if p = i
               then B (bind key i keylen value l, r, i, b)
               else let bn = critbit (Key.get key p) (Key.get k p) in
                    B (L (key, value), tree, p, bn)
            | Compare.Sup p ->
               if p = i
               then B (l, bind key i keylen value r, i, b)
               else let bn = critbit (Key.get key p) (Key.get k p) in
                    B (tree, L (key, value), p, bn)

  let bind key value = function
    | None -> Some (L (key, value))
    | Some tree ->
      let keylen = Key.length key in
      Some (bind key 0 keylen value tree)

  let add = bind

  let rec lookup key off keylen tree = match tree with
    | L (k, v) ->
       if Key.equal key k then Some v else None
    | B (l, r, i, b) ->
       if keylen > i
       then let dir =
              if ((Char.code (Key.get key i)) land (1 lsl b)) = 0
              then l else r in
            lookup key i keylen dir
       else None
    | T (m, k, v) ->
       if Key.equal k key
       then Some v
       else
         let kl = Key.length k in
         match Compare.compare key k off keylen kl with
         | Compare.Eq | Compare.Prefix -> Some v
         | Compare.Contain -> lookup key kl keylen m
         | Compare.Inf _ | Compare.Sup _ -> None

  let rec find_first_opt f = function
    | L (k, v) -> if f k then Some (k, v) else None
    | T (m, k, v) ->
      if f k then Some (k, v) else find_first_opt f m
    | B (l, r, _, _) ->
      match find_first_opt f l, find_first_opt f r with
      | Some (lk, lv), Some (rk, rv) ->
        (match Compare.compare lk rk 0 (Key.length lk) (Key.length rk) with
         | Compare.Inf _ | Compare.Prefix -> Some (lk, lv)
         | _ -> Some (rk, rv))
      | Some v, None -> Some v
      | None, Some v -> Some v
      | None, None -> None

  let find_first_opt f t = let open Option in t >>= find_first_opt f
  let find_first f t = match find_first_opt f t with
    | Some v -> v
    | None -> raise Not_found

  let rec find_last_opt f = function
    | L (k, v) -> if f k then Some (k, v) else None
    | T (m, k, v) ->
      if f k then Some (k, v) else find_last_opt f m
    | B (l, r, _, _) ->
      match find_last_opt f l, find_last_opt f r with
      | Some (lk, lv), Some (rk, rv) ->
        (match Compare.compare lk rk 0 (Key.length lk) (Key.length rk) with
         | Compare.Sup _ | Compare.Contain -> Some (lk, lv)
         | _ -> Some (rk, rv))
      | Some v, None -> Some v
      | None, Some v -> Some v
      | None, None -> None

  let find_last_opt f t = let open Option in t >>= find_last_opt f
  let find_last f t = match find_last_opt f t with
    | Some v -> v
    | None -> raise Not_found

  exception Empty

  let rec remove key off keylen tree = match tree with
    | L (k, _) ->
       if Key.equal key k then raise Empty else tree
    | B (l, r, i, b) ->
       if keylen > i
       then let dir, ndir =
              if ((Char.code (Key.get key i)) land (1 lsl b)) = 0
              then l, r else r, l in
            try remove key i keylen dir
            with Empty -> ndir
       else tree (* does not exists *)
    | T (m, k, v) ->
       if Key.equal k key
       then m
       else
         let kl = Key.length k in
         match Compare.compare key k off keylen kl with
         | Compare.Eq | Compare.Prefix -> m
         | Compare.Contain ->
            (try remove key kl keylen m
             with Empty -> L (k, v))
         | Compare.Inf _ | Compare.Sup _ -> tree (* does not exists *)

  let remove key = function
    | None -> None
    | Some tree ->
       let keylen = Key.length key in
       try Some (remove key 0 keylen tree)
       with Empty -> None

  let lookup key = function
    | None -> None
    | Some tree ->
      let keylen = Key.length key in
      lookup key 0 keylen tree

  let find key tree = match lookup key tree with
    | Some value -> value
    | None -> raise Not_found

  let find_opt = lookup

  let mem key tree =
    match lookup key tree with
    | None -> false
    | Some _ -> true

  let rec fold f acc = function
    | L (k, v) -> f k v acc
    | T (m, k, v) ->
      let acc' = f k v acc in
      fold f acc' m
    | B (l, r, _, _) ->
      let acc' = fold f acc l in
      fold f acc' r

  let fold f acc = function
    | None -> acc
    | Some tree -> fold f acc tree

  let cardinal m = fold (fun _key _value -> (+) 1) 0 m
  let for_all f = fold (fun k v -> (&&) (f k v)) true
  let exists f = fold (fun k v -> (||) (f k v)) true

  let rec map f = function
    | L (k, v) -> L (k, f k v)
    | T (m, k, v) -> T (map f m, k, f k v)
    | B (l, r, i, b) ->
      B (map f l, map f r, i, b)

  let mapi f = function
    | None -> None
    | Some tree -> Some (map f tree)

  let map f = mapi (fun _ v -> f v)

  let rec iter f = function
    | L (k, v) -> f k v
    | T (m, k, v) -> f k v; iter f m
    | B (l, r, _, _) -> iter f l; iter f r

  let iter f = function
    | None -> ()
    | Some tree -> iter f tree

  let to_sequence
      : 'a t -> (Key.t * 'a) sequence
      = fun tree f -> iter (fun k v -> f (k, v)) tree

  let bindings t = fold (fun k v a -> (k, v) :: a) [] t
  let filter f = fold (fun k v a -> if f k v then add k v a else a) empty

  let equal (type a) (f: a -> a -> bool) (a: a t) (b: a t) =
    try List.for_all2 (fun (k1, v1) (k2, v2) -> match Compare.compare k1 k2 0 (Key.length k1) (Key.length k2) with
        | Compare.Eq -> f v1 v2
        | _ -> false) (bindings a) (bindings b)
    with _exn -> false

  let partition f t =
    fold (fun k v (a, b) -> if f k v then (add k v a, b) else (a, add k v b)) (empty, empty) t

  let split key =
    let keylen = Key.length key in
    let coll k v (l, b, r) =
      let kl = Key.length k in

      match Compare.compare key k 0 keylen kl with
      | Compare.Eq -> l, Some v, r
      | Compare.Inf _ -> add k v l, b, r
      | Compare.Sup _ -> l, b, add k v r
      | Compare.Prefix -> add k v l, b, r
      | Compare.Contain -> l, b, add k v r in
    fold coll (empty, None, empty)

  let rec min_binding = function
    | L (k, v) -> (k, v)
    | B (l, _, _, _) -> min_binding l
    | T (_, k, v) -> (k, v)

  let min_binding_opt t = let open Option in t >|= min_binding
  let min_binding t = match min_binding_opt t with
    | Some v -> v
    | None -> raise Not_found

  let rec max_binding = function
    | L (k, v) -> (k, v)
    | B (_, r, _, _) -> max_binding r
    | T (m, _, _) -> max_binding m

  let max_binding_opt t = let open Option in t >|= max_binding
  let max_binding t = match max_binding_opt t with
    | Some v -> v
    | None -> raise Not_found

  let merge f a b =
    let add m k = function None -> m | Some v -> add k v m in
    let m = fold (fun k1 v1 m -> add m k1 (f k1 (Some v1) (find_opt k1 b))) empty a in
    fold (fun k2 v2 m -> if mem k2 a then m else add m k2 (f k2 None (Some v2))) m b

  let union f a b =
    let m =
      fold (fun k v m ->
          match find_opt k b with
          | None -> add k v m
          | Some w ->
            match f k v w with
            | None -> m
            | Some z -> add k z m)
        a empty in
    fold (fun k v m ->
        match find_opt k a with
        | None -> add k v m
        | Some _ -> m) b m

  let compare cmp a b =
    let rec go a b = match a, b with
      | L (k1, v1), L (k2, v2) ->
        (match Compare.compare k1 k2 0 (Key.length k1) (Key.length k2) with
         | Compare.Eq -> cmp v1 v2
         | Compare.Inf _ | Compare.Prefix -> (-1)
         | Compare.Sup _ | Compare.Contain -> 1)
      | L _, T _ | L _, B _ -> (-1)
      | T (m, k1, v1), T (n, k2, v2) ->
        (match Compare.compare k1 k2 0 (Key.length k1) (Key.length k2) with
         | Compare.Eq -> let c = cmp v1 v2 in if c <> 0 then c else go m n
         | Compare.Inf _ | Compare.Prefix -> (-1)
         | Compare.Sup _ | Compare.Contain -> 1)
      | T _, L _ -> 1
      | T _, B _ -> (-1)
      | B (l1, r1, i1, b1), B (l2, r2, i2, b2) ->
        let c = i1 - i2 in
        if c <> 0
        then c else
          let c = b1 - b2 in
          if c <> 0 then c
          else let c = go l1 l2 in
            if c <> 0
            then c else go r1 r2
      | B _, L _ | B _, T _ -> 1
    in go a b

  let compare cmp a b = match a, b with
    | Some a, Some b -> compare cmp a b
    | Some _, None -> 1
    | None, Some _ -> (-1)
    | None, None -> 0

  let pp ppk ppv ppf radix =
    Fmt.Dump.list (Fmt.Dump.pair ppk ppv) ppf (bindings radix)

  let fold f m a = fold f a m

  let update x f m =
    match f (find_opt x m) with
    | Some z -> add x z m
    | None -> remove x m
end
