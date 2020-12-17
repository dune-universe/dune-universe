open Module_types



module Map (Key: SORTABLE) =
struct

    (* Basics *)
    (**********)

    type 'a pair =
      Key.t * 'a

    let compare (key: Key.t) (p: 'a pair): int =
        Key.compare key (fst p)


    type color =
        | Red
        | Black

    type 'a t =
        (* A tree of type ['a t] always satisfies the red and the black
         * invariant. *)
        | Empty
        | Node of
            color
            * 'a t      (* left subtree *)
            * 'a pair   (* key value pair *)
            * 'a t      (* right subtree *)

    let empty: 'a t =
        Empty

    let node
            (color: color) (left: 'a t) (p: 'a pair) (right: 'a t)
            : 'a t =
        Node (color, left, p, right)


    let is_empty (tree: 'a t): bool =
        match tree with
        | Empty ->
            true
        | _ ->
            false


    let fold_right (f: Key.t -> 'a -> 'b -> 'b) (tree: 'a t) (start: 'b): 'b =
        let rec fld tree start =
            match tree with
            | Empty ->
                start
            | Node (_, left, (k,v), right) ->
                fld left (f k v (fld right start))
        in
        fld tree start



    let fold (f: Key.t -> 'a -> 'b -> 'b) (tree: 'a t) (start: 'b): 'b =
        let rec fld start tree =
            match tree with
            | Empty ->
                start
            | Node (_, left, (k,v), right) ->
                fld (f k v (fld start left)) right
        in
        fld start tree


    let cardinal (tree: 'a t): int =
        fold
            (fun _ _ sum -> sum + 1)
            tree
            0

    let bindings (tree: 'a t): (Key.t * 'a) list =
        fold_right
            (fun k v lst -> (k,v) :: lst)
            tree
            []


    let maybe_find (k: Key.t) (tree: 'a t): 'a option =
        let rec find tree =
            match tree with
            | Empty ->
                None
            | Node (_, left, x, right) ->
                let cmp = compare k x
                in
                if cmp < 0 then
                    find left
                else if cmp = 0 then
                    Some (snd x)
                else
                    find right
        in
        find tree


    let find (k: Key.t) (tree: 'a t): 'a =
        match maybe_find k tree with
        | None ->
            assert false (* Illegal call! *)
        | Some value ->
            value


    let mem (k: Key.t) (tree: 'a t): bool =
        maybe_find k tree <> None




    let rec check_invariant (t: 'a t): (int * color) option =
        match t with
        | Empty ->
            Some (0, Black)
        | Node (color, left, _, right) ->
            Option.(
                check_invariant left  >>= fun (h1, c1) ->
                check_invariant right >>= fun (h2, c2) ->
                if h1 = h2 then
                    match color, c1, c2 with
                    | Red, Black, Black ->
                        Some (h1, Red)

                    | Red, _, _ ->
                        (* Violation: A red parent has at least one red child.
                         *)
                        None

                    | Black, _, _ ->
                        Some (h1 + 1, Black)

                else
                    (* Violation: The black height of the children is different.
                     *)
                    None
            )



    let is_balanced (t: 'a t): bool =
        match check_invariant t with
        | None ->
            false
        | Some _ ->
            true





    (* Insertion *)
    (*************)



    type 'a almost =
      | AOk of (* Root color unchanged, no violation. *)
            'a t

      | BROk of (* Root color changed from black to red, no violation *)
            'a t * 'a pair * 'a t

      | RVio of
            (* Root color has been red originally. [a x b y c] is an orderd
             * sequence. All three subtrees [a], [b] and [c] are black rooted
             * and have the original black height. *)
            'a t * 'a pair * 'a t * 'a pair * 'a t



    let use_almost
            (ok1: 'a t -> 'b)
            (ok2: 'a t -> 'a pair -> 'a t -> 'b)
            (vio: 'a t -> 'a pair -> 'a t -> 'a pair -> 'a t -> 'b)
            : 'a almost -> 'b =
        function
        | AOk tree ->
            ok1 tree
        | BROk (left, x, right) ->
            ok2 left x right
        | RVio (a, x, b, y, c) ->
            vio a x b y c


    let rbt_of_almost (almost: 'a almost): 'a t =
        use_almost
            (fun x -> x)
            (node Red)
            (fun a x b y c ->
                 node Black (node Red a x b) y c)
            almost


    let balance_left color left z d =
        use_almost
            (fun left ->
                 AOk (node color left z d))
            (fun left p right ->
                 (* Root color of (left,p,right) changed from black to red, no
                  * violation. *)
                 match color with
                 | Black ->
                     AOk (node Black (node Red left p right) z d)

                 | Red ->
                     (* red violation *)
                     RVio (left, p, right, z, d))
            (fun a x b y c ->
                 assert (color = Black);
                 BROk (node Black a x b, y, (node Black c z d)))
            left


    let balance_right color a x right =
        use_almost
            (fun right ->
                 AOk (node color a x right))
            (fun left p right ->
                 (* Root color of (left,p,right) changed from black to red, no
                  * violation. *)
                 match color with
                 | Black ->
                     AOk (node color a x (node Red left p right))

                 | Red ->
                     (* red violation *)
                     RVio (a, x, left, p, right))
            (fun b y c z d ->
                 assert (color = Black);
                 BROk (node Black a x b, y, (node Black c z d)))
            right





    let rec add (k: Key.t) (v: 'a) (rbt: 'a t): 'a t =
        rbt_of_almost (ins k v rbt)

    and ins (k: Key.t) (v: 'a): 'a t -> 'a almost =
        function
        | Empty ->
            BROk (Empty, (k, v), Empty)

        | Node (color, left, x, right) ->
            let cmp = compare k x
            in
            if cmp < 0 then
                balance_left color (ins k v left) x right

            else if cmp = 0 then
                AOk (node color left (k,v) right)

            else
                balance_right color left x (ins k v right)




    (* Deletion  *)
    (*************)

    type 'a removed =
      | ROk of
            (* The tree has the same height as before the removal and the same
             * color or its color has been changed to black. *)
            'a t * 'a pair

      | RMinus of
            (* The tree has a reduced black height. Its color is black and has
             * not been changed. *)
            'a t * 'a pair


    let swap_pair
            (pnew: 'a pair) (rtree: 'a removed)
            : 'a removed * 'a pair =
        match rtree with
        | ROk (r, p) ->
            ROk (r, pnew), p

        | RMinus (r, p) ->
            RMinus (r, pnew), p


    let use_left_sibling
            (black1: 'a t -> 'a pair -> 'a t -> 'b)
            (black2: 'a t -> 'a pair -> 'a t -> 'a pair -> 'a t -> 'b)
            (red1:  'a t -> 'a pair -> 'a t -> 'a pair -> 'a t -> 'b)
            (red2:  'a t -> 'a pair -> 'a t -> 'a pair -> 'a t ->
                    'a pair -> 'a t -> 'b)
            (tree: 'a t)
        : 'b =
        (* [tree] is the left sibling of a height reduced right node. The right
         * node has black height [h] and therefore the sibling must have black
         * height [h + 1]. Otherwise the tree had not been a valid red black
         * tree before the removal of a node.
         *
         * The function splits up the sibling into the ordered sequence
         *
         *      a x b y c z d
         *
         *  where at least [a x b] is present. All subtrees have black height
         *  [h] except in the red case where the subtree [a] which has black
         *  height [h + 1].
         *
         * [tree] is black:
         *
         *          Bx                          Bx
         *      a       Bb                  a        Ry
         *                                        Bb    Bc
         *
         *  [tree] is red:
         *
         *          Rx                          Rx
         *      Ba+     By                  Ba+     By
         *            b    Bc                     b     Rz
         *                                            Bc   Bd
         *)
        match tree with
        | Empty ->
            (* Cannot happen. [tree] has black height [h+1]. An empty node is
             * not possible. *)
            assert false

        | Node (Black, a, x, right) ->
            (
                match right with
                | Node (Red, b, y, c) ->
                    black2 a x b y c
                | _ ->
                    black1 a x right
            )

        | Node (Red, a, x, right) ->
            (
                match right with
                | Node (Black, b, y, Node (Red, c, z, d)) ->
                    red2 a x b y c z d
                | Node (Black, b, y, c) ->
                    red1 a x b y c
                | _ ->
                    (* Cannot happen. [right] must be black, because its parent
                     * is red. Since the black height is [h+1], [right] cannot
                     * be empty either. *)
                    assert false
            )


    let use_right_sibling
            (black1: 'a t -> 'a pair -> 'a t -> 'b)
            (black2: 'a t -> 'a pair -> 'a t -> 'a pair -> 'a t -> 'b)
            (red1:  'a t -> 'a pair -> 'a t -> 'a pair -> 'a t -> 'b)
            (red2:  'a t -> 'a pair -> 'a t -> 'a pair -> 'a t ->
                    'a pair -> 'a t -> 'b)
            (tree: 'a t)
        : 'b =
        (* Mirror image of [use_left_sibling].
         * [tree] is black:
         *
         *          Bz                          Bz
         *      Bc      d                   Ry      d
         *                               Bb   Bc
         *
         * [tree] is red:
         *
         *          Rz                          Rz
         *      By      d+                  By      d+
         *   Bb    c                    Rx     c
         *                           Ba   Bb
         *)
        match tree with
        | Empty ->
            (* Cannot happen. [tree] has black height [h+1]. An empty node is
             * not possible. *)
            assert false

        | Node (Black, left, z, d) ->
            (
                match left with
                | Node (Red, b, y, c) ->
                    black2 b y c z d
                | _ ->
                    black1 left z d
            )

        | Node (Red, left, z, d) ->
            (
                match left with
                | Node (Black, Node (Red, a, x, b), y, c) ->
                    red2 a x b y c z d
                | Node (Black, b, y, c) ->
                    red1 b y c z d
                | _ ->
                    (* Cannot happen. [left] must be black, because its parent
                     * is red. Since the black height is [h+1], [left] cannot
                     * be empty either. *)
                    assert false
            )


    let use_left_sibling_black_parent
        (left: 'a t) (p: 'a pair) (reduced: 'a t) (deleted: 'a pair)
        : 'a removed
        =
        (* black_height(left):      h + 1
         * black_height(reduced):   h
         * black height goal:       h + 2 *)
        use_left_sibling
            (fun a x b_black ->
                 RMinus ( (* black height h + 1 *)
                     Node (Black, a, x,
                           Node (Red, b_black, p, reduced))
                     , deleted)
            )
            (fun a x b y cblack ->
                 ROk ( (* black height: h + 2 *)
                     Node (
                         Black,
                         Node (Black, a, x, b),
                         y,
                         Node (Black, cblack, p, reduced)),
                     deleted))
            (fun aplus x b y c_black ->
                 ROk ( (* black height: h + 2 *)
                     Node (Black, aplus, x,
                           Node (Black, b, y,
                                 Node (Red, c_black, p, reduced))),
                     deleted
                 ))
            (fun aplus x b y c z dblack ->
                 ROk (  (* black height: h + 2 *)
                     Node (Black, aplus, x,
                           Node (Red,
                                 Node (Black, b, y, c),
                                 z,
                                 Node (Black, dblack, p, reduced))),
                     deleted))
            left



    let use_left_sibling_red_parent
        (left: 'a t) (p: 'a pair) (reduced: 'a t) (deleted: 'a pair)
        : 'a removed
        =
        (* black_height(left):      h + 1
         * black_height(reduced):   h
         * black height goal:       h + 1 *)
        use_left_sibling
            (fun a x bblack ->
                 ROk ( (* black height: h + 1 *)
                    Node (Black, a, x,
                          Node (Red, bblack, p, reduced)),
                    deleted))
            (fun a x b y c ->
                 ROk ( (* black height: h + 1 *)
                     Node (Red,
                           Node (Black, a, x, b),
                           y,
                           Node (Black, c, p, reduced)),
                     deleted))
            (fun _ _ _ _ ->
                 (* [left] cannot be red. *)
                 assert false)
            (fun _ _ _ _ _ ->
                 (* [left] cannot be red. *)
                 assert false)
            left


    let use_right_sibling_black_parent
        (reduced: 'a t) (deleted: 'a pair) (p: 'a pair) (right: 'a t)
        : 'a removed
        =
        (* black_height(reduced):   h
         * black_height(right):     h + 1
         * black height goal:       h + 2 *)
        use_right_sibling
            (fun cblack z d ->
                 RMinus (
                     Node (Black,
                           Node (Red, reduced, p, cblack),
                           z, d)
                     , deleted)
            )
            (fun bblack y c z d ->
                 ROk (Node (Black,
                            Node (Black, reduced, p, bblack),
                            y,
                            Node (Black, c, z, d))
                     , deleted)
            )
            (fun bblack y c z dplus ->
                 ROk (Node (Black,
                            Node (Black,
                                  Node (Red, reduced, p, bblack),
                                  y, c),
                            z, dplus),
                      deleted)
            )
            (fun ablack x b y c z dplus ->
                 ROk (Node (Black,
                            Node (Red,
                                  Node (Black, reduced, p, ablack),
                                  x,
                                  Node (Black, b, y, c)),
                            z, dplus),
                      deleted)
            )
            right



    let use_right_sibling_red_parent
        (reduced: 'a t) (deleted: 'a pair) (p: 'a pair) (right: 'a t)
        : 'a removed
        =
        (* black_height(reduced):   h
         * black_height(right):     h + 1
         * black height goal:       h + 1 *)
        use_right_sibling
            (fun cblack z d ->
                 ROk (
                     Node (Black,
                           Node (Red, reduced, p, cblack),
                           z, d),
                     deleted
                 )
            )
            (fun b y c z d ->
                 ROk (
                     Node (Red,
                           Node (Black, reduced, p, b),
                           y,
                           Node (Black, c, z, d)),
                     deleted
                 )
            )
            (fun _ _ _ _ ->
                 (* [right] cannot be red. *)
                 assert false)
            (fun _ _ _ _ _ ->
                 (* [right] cannot be red. *)
                 assert false)
            right



    let removed_left
            (color: color) (reduced: 'a removed) (p: 'a pair) (right: 'a t)
            : 'a removed =
        (* The left child has a potentially reduced black height compared to
         * its right sibling. *)
        match reduced with
        | ROk (left, x) ->
            ROk (Node (color, left, p, right), x)

        | RMinus (reduced, deleted) ->
            match color with
            | Black ->
                use_right_sibling_black_parent
                    reduced deleted p right

            | Red ->
                use_right_sibling_red_parent
                    reduced deleted p right





    let removed_right
            (color: color) (left: 'a t) (p: 'a pair) (reduced: 'a removed)
            : 'a removed =
        (* The right child has a potentially reduced black height compared to
         * its left sibling. *)
        match reduced with
        | ROk (right, x) ->
            ROk (Node (color, left, p, right), x)

        | RMinus (reduced, deleted) ->
            match color with
            | Black ->
                use_left_sibling_black_parent
                    left p reduced deleted

            | Red ->
                use_left_sibling_red_parent
                    left p reduced deleted




    let remove_bottom (color: color) (x: 'a pair) (child: 'a t): 'a removed =
        (* Remove a bottom node with [color] and [x] which has at most one
         * [child]. *)
        match color, child with
        | Black, Empty ->
            RMinus (Empty, x)

        | Black, Node (Red, Empty, p, Empty) ->
            ROk (Node (Black, Empty, p, Empty), x)

        | Black, _ ->
            (* Cannot happen. If a black node has one child, then the child must
             * be red. *)
            assert false

        | Red, Empty ->
            ROk (Empty, x)

        | Red, _ ->
            (* Cannot happen. A red node has either no children or two black
             * children. *)
            assert false




    let rec remove (key: Key.t) (tree: 'a t): 'a t =
        match rem key tree with
        | None ->
            tree
        | Some (ROk (tree, _))
        | Some (RMinus (tree, _)) ->
            tree

    and rem (k: Key.t) (tree: 'a t): 'a removed option =
        match tree with
        | Empty ->
            None

        | Node (color, left, x, right) ->
            let cmp = compare k x
            in
            if cmp < 0 then
                Option.map
                    (fun left -> removed_left color left x right)
                    (rem k left)

            else if cmp = 0 then
                match remove_leftmost right with
                | None ->
                    Some (remove_bottom color x left)
                | Some rtree ->
                    let tree, p = swap_pair x rtree in
                    Some (removed_right color left p tree)

            else
                Option.map
                    (fun right -> removed_right color left x right)
                    (rem k right)

    and remove_leftmost (tree: 'a t): 'a removed option =
        match tree with
        | Empty ->
            None

        | Node (color, Empty, x, right) ->
            Some (remove_bottom color x right)

        | Node (color, left, x, right) ->
            Option.map
                (fun left -> removed_left color left x right)
                (remove_leftmost left)
end (* Map *)










module Set (Element: SORTABLE) =
struct
    module Map = Map (Element)

    type t = unit Map.t

    let is_empty (set: t): bool =
        Map.is_empty set

    let cardinal (set: t): int =
        Map.cardinal set

    let empty: t =
        Map.empty

    let add (e: Element.t) (set: t): t =
        Map.add e () set

    let mem (e: Element.t) (set: t): bool =
        Map.mem e set

    let remove (e: Element.t) (set: t): t =
        Map.remove e set
end







(* ------------------------------------------------------------------*)
(* Unit Tests *)
(* ------------------------------------------------------------------*)

module M = Map (Int)

let ins (n: int) (map: int M.t): int M.t =
    let rec ins i map =
        if i = n then
            map
        else
            ins (i + 1) (M.add i i map)
    in
    ins 0 map


let insert (n: int): int M.t =
    ins n M.empty


let check (start: int) (beyond: int) (map: int M.t): bool =
    let rec check_from i =
        if i = beyond then
            true
        else
            let res = M.maybe_find i map in
            match res with
            | None ->
                Printf.printf "nothing found for %d\n" i;
                false
            | Some k ->
                if i <> k then
                    Printf.printf "found wrong pair %d %d\n" i k;
                check_from (i + 1)
    in
    check_from start



let string_of_bindings (tree: int M.t): string =
    let open Printf
    in
    sprintf "[%s]"
        (String.concat ","
             (List.map
                  (fun (k,v) -> sprintf "(%d,%d)" k v)
                  (M.bindings tree)))

let _ = string_of_bindings



let%test _ =
    let n = 100
    in
    let rec test_from i map =
        if i = n then
            true
        else
            let map = M.add i i map
            in
            M.is_balanced map &&
            test_from (i + 1) map
    in
    test_from 0 M.empty



let%test _ =
    let n = 100
    in
    check 0 n (insert n)



let%test _ =
    let n = 10
    in
    check 0 n (ins n (insert n))


let%test _ =
    let n = 20
    in
    let rec test_from i map =
        let check_not_in map =
            match M.maybe_find i map with
            | None ->
                true
            | Some k ->
                Printf.printf "deleted pair (%d,%d) still in\n" i k;
                false
        and check_rest map =
            let res = check (i + 1) n map
            in
            if not res then
                Printf.printf "some values below %d not available\n" i;
            res
        and  remove map =
            let map = M.remove i map in
            (*Printf.printf "bindings %s\n" (string_of_bindings map);
            Printf.printf "cardinal %d\n" (M.cardinal map);*)
            map
        and check_invariant map =
            let res = M.is_balanced map in
            if not res then
                Printf.printf "invariant violated after deleting %d\n" i;
            res
        in
        if i = n then
            true
        else
            let map = remove map
            in
            check_not_in map
            &&
            check_rest map
            &&
            check_invariant map
            &&
            test_from (i + 1) map
    in
    test_from 0 (insert n)
