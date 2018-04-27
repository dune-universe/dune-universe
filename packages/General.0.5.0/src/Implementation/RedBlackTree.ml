type 'a node = {
  l: 'a t;
  v: 'a;
  r: 'a t
}
and 'a t =
  | Empty
  | EmptyPlus (* Temporary marker for Empty nodes that carry an extra unit of black-height during node removal. *)
  | Black of 'a node
  | BlackPlus of 'a node (* Same kind of temporary marker for black nodes. *)
  | Red of 'a node
  (* Red nodes don't need such marker because if the extra black-height reaches them, they just absorb it by turning black. *)

let repr ~repr_a =
  let rec aux = function
    | Empty -> "Empty"
    | EmptyPlus -> "EmptyPlus"
    | Black {l; v; r} -> Format.apply "Black {l=%s; v=%s; r=%s}" (aux l) (repr_a v) (aux r)
    | BlackPlus {l; v; r} -> Format.apply "BlackPlus {l=%s; v=%s; r=%s}" (aux l) (repr_a v) (aux r)
    | Red {l; v; r} -> Format.apply "Red {l=%s; v=%s; r=%s}" (aux l) (repr_a v) (aux r)
  in
  aux

module Invariants = struct
  type t =
    | HasBlackRoot
    | IsRedBlack
    | IsBlackBalanced
    | IsBinarySearchTree

  exception BrokenInvariants of t list

  (*BISECT-IGNORE-BEGIN*)
  let repr = function
    | HasBlackRoot -> "HasBlackRoot"
    | IsRedBlack -> "IsRedBlack"
    | IsBlackBalanced -> "IsBlackBalanced"
    | IsBinarySearchTree -> "IsBinarySearchTree"

  let _ = Exception.register_printer (function
    | BrokenInvariants broken_invariants ->
      Some (Format.apply "Broken red-black tree invariants: %s" (broken_invariants |> List.map ~f:repr |> List.join_string_list ~sep:", "))
    | _ ->
      None
  )
  (*BISECT-IGNORE-END*)

  let has_black_root = function
    | Empty
    | Black _ ->
      true
    | EmptyPlus
    | BlackPlus _
    | Red _ ->
      false

  let is_red_black t =
    let rec aux = function
      | Empty ->
        true
      | Red {l=Red _; _}
      | Red {r=Red _; _}
      | EmptyPlus
      | BlackPlus _ ->
        false
      | Red {l; r; _}
      | Black {l; r; _} ->
        (aux l) && (aux r)
    in
    aux t

  let is_binary_search_tree t ~cmp =
    let check_min ~min ~v =
      min |> Option.value_map ~def:true ~f:(fun min ->
        match cmp v min with
          | Compare.GT -> true
          | _ -> false
      )
    and check_max ~max ~v =
      max |> Option.value_map ~def:true ~f:(fun max ->
        match cmp v max with
          | Compare.LT -> true
          | _ -> false
        )
    in
    let check_value ~min ~max ~v =
      (check_min ~min ~v) && (check_max ~max ~v)
    in
    let rec aux ~min ~max = function
      | Empty
      | EmptyPlus ->
        true
      | Red {l; v; r}
      | Black {l; v; r}
      | BlackPlus {l; v; r} ->
        (aux ~min ~max:(Some v) l) && (check_value ~min ~max ~v) && (aux ~min:(Some v) ~max r)
    in
    aux ~min:None ~max:None t

  let rec black_height =
    let common_height l r =
      match (black_height l, black_height r) with
        | (Some hl, Some hr) when hl = hr ->
          Some hl
        | _ ->
          None
    in
    function
      | Empty ->
        Some 0
      | EmptyPlus ->
        Some 1
      | Red {l; r; _} ->
        common_height l r
      | Black {l; r; _} ->
        common_height l r |> Option.map ~f:((+) 1)
      | BlackPlus {l; r; _} ->
        common_height l r |> Option.map ~f:((+) 2)

  let is_black_balanced t =
    t
    |> black_height
    |> Option.is_some

  let validate t ~cmp =
    match
      [
        (has_black_root, HasBlackRoot);
        (is_red_black, IsRedBlack);
        (is_black_balanced, IsBlackBalanced);
        (is_binary_search_tree ~cmp, IsBinarySearchTree);
      ]
      |> List.filter_map ~f:(fun (predicate, invariant) ->
        Option.some_if' (not (predicate t)) invariant
      )
    with
      | [] -> t
      | broken_invariants -> Exception.raise (BrokenInvariants broken_invariants)
end

module Restore = struct
  let fix_root_color = function
    | Empty
    | EmptyPlus ->
      Empty
    | Red node
    | BlackPlus node ->
      Black node
    | Black _ as t ->
      t

  let balance_reds_left = function
    | Black {l=Red {l=Red {l=a; v=x; r=b}; v=y; r=c}; v=z; r=d} ->
      Red {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | Black {l=Red {l=a; v=x; r=Red {l=b; v=y; r=c}}; v=z; r=d} ->
      Red {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | Black _ as t -> t
    | BlackPlus {l=Red {l=Red {l=a; v=x; r=b}; v=y; r=c}; v=z; r=d} ->
      Black {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | BlackPlus {l=Red {l=a; v=x; r=Red {l=b; v=y; r=c}}; v=z; r=d} ->
      Black {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | BlackPlus _ as t -> t
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)

  let balance_reds_right = function
    | Black {l=a; v=x; r=Red {l=Red {l=b; v=y; r=c}; v=z; r=d}} ->
      Red {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | Black {l=a; v=x; r=Red {l=b; v=y; r=Red {l=c; v=z; r=d}}} ->
      Red {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | Black _ as t -> t
    | BlackPlus {l=a; v=x; r=Red {l=Red {l=b; v=y; r=c}; v=z; r=d}} ->
      Black {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | BlackPlus {l=a; v=x; r=Red {l=b; v=y; r=Red {l=c; v=z; r=d}}} ->
      Black {l=Black {l=a; v=x; r=b}; v=y; r=Black {l=c; v=z; r=d}}
    | BlackPlus _ as t -> t
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)

  let balance_blacks_left = function
    | Red {l=Black {l=a; v=x; r=b}; v=y; r=EmptyPlus} ->
      balance_reds_left (Black {l=Red {l=a; v=x; r=b}; v=y; r=Empty})
    | Black {l=Black {l=a; v=x; r=b}; v=y; r=EmptyPlus} ->
      balance_reds_left (BlackPlus {l=Red {l=a; v=x; r=b}; v=y; r=Empty})
    | Black {l=Red {l=a; v=x; r=Black {l=b; v=y; r=c}}; v=z; r=EmptyPlus} ->
      Black {l=a; v=x; r=balance_reds_left (Black {l=Red {l=b; v=y; r=c}; v=z; r=Empty})}
    | t ->
      t

  let balance_blacks_right = function
    | Red {l=EmptyPlus; v=x; r=Black {l=b; v=y; r=c}} ->
      balance_reds_right (Black {l=Empty; v=x; r=Red {l=b; v=y; r=c}})
    | Black {l=EmptyPlus; v=x; r=Black {l=b; v=y; r=c}} ->
      balance_reds_right (BlackPlus {l=Empty; v=x; r=Red {l=b; v=y; r=c}})
    | Black {l=EmptyPlus; v=x; r=Red {l=Black {l=b; v=y; r=c}; v=z; r=d}} ->
      Black {l=balance_reds_right (Black {l=Empty; v=x; r=Red {l=b; v=y; r=c}}); v=z; r=d}
    | t ->
      t
end

let empty = Empty

let add xs ~cmp x =
  let rec aux = function
    | Empty ->
      (true, Red {l=Empty; v=x; r=Empty})
    | Red {l; v; r} as t -> (
      match cmp x v with
        | Compare.LT ->
          let (modified, l') = aux l in
          if modified then
            (true, Red {l=l'; v; r})
          else
            (false, t)
        | Compare.EQ ->
          (false, t)
        | Compare.GT ->
          let (modified, r') = aux r in
          if modified then
            (true, Red {l; v; r=r'})
          else
            (false, t)
    )
    | Black {l; v; r} as t -> (
      match cmp x v with
        | Compare.LT ->
          let (modified, l') = aux l in
          if modified then
            (true, Restore.balance_reds_left (Black {l=l'; v; r}))
          else
            (false, t)
        | Compare.EQ ->
          (false, t)
        | Compare.GT ->
          let (modified, r') = aux r in
          if modified then
            (true, Restore.balance_reds_right (Black {l; v; r=r'}))
          else
            (false, t)
    )
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
  in
  let (modified, t) =
    xs
    (* @todo Define DEBUG when compiling with jbuilder --dev *)
    #ifdef DEBUG
    |> Invariants.validate ~cmp
    #endif
    |> aux
  in
  let t =
    t
    |> Restore.fix_root_color
    #ifdef DEBUG
    |> Invariants.validate ~cmp
    #endif
  in
  (modified, t)

let replace xs ~cmp x =
  let rec aux = function
    | Empty ->
      Red {l=Empty; v=x; r=Empty}
    | Red {l; v; r} -> (
      match cmp x v with
        | Compare.LT ->
          Red {l=aux l; v; r}
        | Compare.EQ ->
          Red {l; v=x; r}
        | Compare.GT ->
          Red {l; v; r=aux r}
    )
    | Black {l; v; r} -> (
      match cmp x v with
        | Compare.LT ->
          Restore.balance_reds_left (Black {l=aux l; v; r})
        | Compare.EQ ->
          Black {l; v=x; r}
        | Compare.GT ->
          Restore.balance_reds_right (Black {l; v; r=aux r})
    )
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
  in
  xs
  #ifdef DEBUG
  |> Invariants.validate ~cmp
  #endif
  |> aux
  |> Restore.fix_root_color
  #ifdef DEBUG
  |> Invariants.validate ~cmp
  #endif

let remove xs ~cmp ~cmp_k x =
  #ifndef DEBUG
  ignore cmp;
  #endif
  let rec remove_min = function
    | Black {l=Empty; v; r=Empty} ->
      (EmptyPlus, v)
    | Black {l=Empty; v=x; r=Red {l; v=y; r}} ->
      (Black {l; v=y; r}, x)
    | Red {l=Empty; v; r} ->
      (r, v)
    | Black {l; v; r} ->
        let (l', min) = remove_min l in
        let t = Black {l=l'; v; r} in
        (Restore.balance_blacks_right t, min)
    | Red {l; v; r} ->
        let (l', min) = remove_min l in
        let t = Red {l=l'; v; r} in
        (Restore.balance_blacks_right t, min)
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
  in
  let rec aux = function
    | Empty ->
      (false, Empty)
    | Black {l; v; r} as t -> (
      match cmp_k x v with
        | Compare.LT ->
          let (modified, l') = aux l in
          if modified then
            (true, Restore.balance_blacks_right (Black {l=l'; v; r}))
          else
            (false, t)
        | Compare.GT ->
          let (modified, r') = aux r in
          if modified then
            (true, Restore.balance_blacks_left (Black {l; v; r=r'}))
          else
            (false, t)
        | Compare.EQ -> (
          match r with
            | Empty -> (
              match l with
                | Red node ->
                  (true, Black node)
                | Empty ->
                  (true, EmptyPlus)
                | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
            )
            | Red _
            | Black _ ->
              let (r', min) = remove_min r in
              (true, Restore.balance_blacks_left (Black {l; v=min; r=r'}))
            | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
        )
    )
    | Red {l; v; r} as t -> (
      match cmp_k x v with
        | Compare.LT ->
          let (modified, l') = aux l in
          if modified then
            (true, Restore.balance_blacks_right (Red {l=l'; v; r}))
          else
            (false, t)
        | Compare.GT ->
          let (modified, r') = aux r in
          if modified then
            (true, Restore.balance_blacks_left (Red {l; v; r=r'}))
          else
            (false, t)
        | Compare.EQ -> (
          match r with
            | Empty ->
              (true, l)
            | Black _ ->
              let (r', min) = remove_min r in
              (true, Restore.balance_blacks_left (Red {l; v=min; r=r'}))
            | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
        )
    )
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
  in
  let (modified, t) =
    xs
    #ifdef DEBUG
    |> Invariants.validate ~cmp
    #endif
    |> aux
  in
  let t =
    t
    |> Restore.fix_root_color
    #ifdef DEBUG
    |> Invariants.validate ~cmp
    #endif
  in
  (modified, t)

let is_empty = function
  | Empty -> true
  | _ -> false

let try_get xs ~cmp ~cmp_k x =
  #ifndef DEBUG
  ignore cmp;
  #endif
  let rec aux = function
    | Empty ->
      None
    | Black {l; v; r}
    | Red {l; v; r} -> (
      match cmp_k x v with
        | Compare.LT ->
          aux l
        | Compare.EQ ->
          Some v
        | Compare.GT ->
          aux r
    )
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
  in
  xs
  #ifdef DEBUG
  |> Invariants.validate ~cmp
  #endif
  |> aux

let fold xs ~cmp ~init ~f =
  #ifndef DEBUG
  ignore cmp;
  #endif
  let rec aux acc = function
    | Empty ->
      acc
    | Red {l; v; r}
    | Black {l; v; r} ->
      aux (f (aux acc l) v) r
    | _ -> Exception.failure "broken invariants" (*BISECT-IGNORE*) (* Unreachable *)
  in
  xs
  #ifdef DEBUG
  |> Invariants.validate ~cmp
  #endif
  |> aux init

let to_list xs ~cmp =
  fold xs ~cmp ~init:[] ~f:(Functions.Function2.flip List.prepend)
  |> List.reverse

let size xs ~cmp =
  fold xs ~cmp ~init:0 ~f:(fun n _ -> Int.succ n)

module Tests = struct
  open Testing

  let repr = repr ~repr_a:Int.repr
  let cmp a b =
    Int.(compare (abs a) (abs b))

  let b1 = Black {l=Empty; v=1; r=Empty}

  let b1r3 = Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}
  let br13 = Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}

  let bb13b5 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}
  let br13r5 = Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}

  let bb13b5r7 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Red {l=Empty; v=7; r=Empty}}}
  let bb13br57 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}}
  let bb1r35b7 = Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}}
  let bbr135b7 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}

  let bb13br57r9 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Red {l=Empty; v=9; r=Empty}}}
  let bb13rb57b9 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}}
  let bb1r35b7r9 = Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}}
  let bb1r35br79 = Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}}
  let bbr135b7r9 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}}
  let bbr135br79 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}}
  let bbr13r57b9 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}}
  let brb13b57b9 = Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}}

  let bb13rb57b9r11 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}}}
  let bb13rb57br911 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}}}
  let bb13rb5r79b11 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Red {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}}}
  let bb13rbr579b11 = Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}}
  let bb1r35br79r11 = Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=11; r=Empty}}}
  let bb1r35rb79b11 = Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}}
  let bbr135br79r11 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=11; r=Empty}}}
  let bbr135rb79b11 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}}
  let bbr13r57b9r11 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}}
  let bbr13r57br911 = Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}}
  let brb13b57b9r11 = Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}}
  let brb13b57br911 = Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}}
  let brb13b5r79b11 = Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Red {l=Empty; v=7; r=Empty}}}; v=9; r=Black {l=Empty; v=11; r=Empty}}
  let brb13br579b11 = Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}}
  let brb1r35b79b11 = Black {l=Red {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}}
  let brbr135b79b11 = Black {l=Red {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}}

  let test = "RedBlackTree" >:: [
    "Invariants" >:: [
      "validate" >:: (
        let make t expected =
          (repr t) >: (
            match expected with
              | [] ->
                lazy (Invariants.validate t ~cmp |> ignore)
              | _ ->
                lazy (expect_exception ~expected:(Invariants.BrokenInvariants expected) (lazy (Invariants.validate t ~cmp)))
          )
        in Invariants.[
          make Empty [];
          make EmptyPlus [HasBlackRoot; IsRedBlack];
          make (Black     {l=Empty; v=0; r=Empty}) [];
          make (BlackPlus {l=Empty; v=0; r=Empty}) [HasBlackRoot; IsRedBlack];
          make (Red       {l=Empty; v=0; r=Empty}) [HasBlackRoot];
          make (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}) [];
          make (Red   {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}) [HasBlackRoot; IsRedBlack];
          make (Red   {l=Empty; v=0; r=Red {l=Empty; v=1; r=Empty}}) [HasBlackRoot; IsRedBlack];
          make (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=2; r=Empty}}) [];
          make (Black {l=Red {l=Empty; v=1; r=Empty}; v=1; r=Red {l=Empty; v=2; r=Empty}}) [IsBinarySearchTree];
          make (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=1; r=Empty}}) [IsBinarySearchTree];
          make (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=2; r=Empty}}) [IsBlackBalanced];
          make (Black {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=2; r=Empty}}) [IsBlackBalanced];
        ]
      )
    ];
    "add" >:: (
      let make t x expected =
        ~: "add %s %i" (repr t) x (lazy (
          check_poly ~repr:(Tuples.Tuple2.repr ~repr_a:Bool.repr ~repr_b:repr) ~expected (add t ~cmp x)
        ))
      in 
      let make_t t x expected =
        make t x (true, expected)
      and make_f t x =
        make t x (false, t)
      in
      [
        make_t Empty 0 (Black {l=Empty; v=0; r=Empty});

        make_t b1 0 (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty});
        make_f b1 1;
        make_t b1 2 (Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}});

        make_t b1r3 0 (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=3; r=Empty}});
        make_f b1r3 1;
        make_t b1r3 2 (Black {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}});
        make_f b1r3 3;
        make_t b1r3 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=4; r=Empty}});

        make_t br13 0 (Black {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=3; r=Empty}});
        make_f br13 1;
        make_t br13 2 (Black {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}});
        make_f br13 3;
        make_t br13 4 (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=4; r=Empty}});

        make_t bb13b5 0 (Black {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}});
        make_f bb13b5 1;
        make_t bb13b5 2 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}}; v=3; r=Black {l=Empty; v=5; r=Empty}});
        make_f bb13b5 3;
        make_t bb13b5 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=4; r=Empty}; v=5; r=Empty}});
        make_f bb13b5 5;
        make_t bb13b5 6 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Red {l=Empty; v=6; r=Empty}}});

        make_t br13r5 0 (Black {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}});
        make_f br13r5 1;
        make_t br13r5 2 (Black {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}});
        make_f br13r5 3;
        make_t br13r5 4 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=4; r=Black {l=Empty; v=5; r=Empty}});
        make_f br13r5 5;
        make_t br13r5 6 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=6; r=Empty}});

        make_t bb13b5r7 0 (Black {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Red {l=Empty; v=7; r=Empty}}});
        make_f bb13b5r7 1;
        make_t bb13b5r7 2 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}}; v=3; r=Black {l=Empty; v=5; r=Red {l=Empty; v=7; r=Empty}}});
        make_f bb13b5r7 3;
        make_t bb13b5r7 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=4; r=Empty}; v=5; r=Red {l=Empty; v=7; r=Empty}}});
        make_f bb13b5r7 5;
        make_t bb13b5r7 6 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=6; r=Black {l=Empty; v=7; r=Empty}}});
        make_f bb13b5r7 7;
        make_t bb13b5r7 8 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=8; r=Empty}}});

        make_f bb13br57 1;
        make_t bb13br57 2 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}});
        make_f bb13br57 3;
        make_t bb13br57 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=4; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}});
        make_f bb13br57 5;
        make_t bb13br57 6 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=6; r=Black {l=Empty; v=7; r=Empty}}});
        make_f bb13br57 7;
        make_t bb13br57 8 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Red {l=Empty; v=8; r=Empty}}});

        make_f bb1r35b7 1;
        make_t bb1r35b7 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35b7 3;
        make_t bb1r35b7 4 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=4; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35b7 5;
        make_t bb1r35b7 6 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=6; r=Empty}; v=7; r=Empty}});
        make_f bb1r35b7 7;
        make_t bb1r35b7 8 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=8; r=Empty}}});

        make_f bbr135b7 1;
        make_t bbr135b7 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135b7 3;
        make_t bbr135b7 4 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=4; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135b7 5;
        make_t bbr135b7 6 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=6; r=Empty}; v=7; r=Empty}});
        make_f bbr135b7 7;
        make_t bbr135b7 8 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=8; r=Empty}}});

        make_t bb13br57r9 0 (Black {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13br57r9 1;
        make_t bb13br57r9 2 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13br57r9 3;
        make_t bb13br57r9 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=4; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}}});
        make_f bb13br57r9 5;
        make_t bb13br57r9 6 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=6; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}}});
        make_f bb13br57r9 7;
        make_t bb13br57r9 8 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}; v=8; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13br57r9 9;
        make_t bb13br57r9 10 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}; v=9; r=Black {l=Empty; v=10; r=Empty}}});

        make_t bb13rb57b9 0 (Black {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9 1;
        make_t bb13rb57b9 2 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9 3;
        make_t bb13rb57b9 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Red {l=Empty; v=4; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9 5;
        make_t bb13rb57b9 6 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Red {l=Empty; v=6; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9 7;
        make_t bb13rb57b9 8 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Red {l=Empty; v=8; r=Empty}; v=9; r=Empty}}});
        make_f bb13rb57b9 9;
        make_t bb13rb57b9 10 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=10; r=Empty}}}});

        make_t bb1r35b7r9 0 (Black {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 1;
        make_t bb1r35b7r9 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 3;
        make_t bb1r35b7r9 4 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=4; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 5;
        make_t bb1r35b7r9 6 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=6; r=Empty}; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 7;
        make_t bb1r35b7r9 8 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=8; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 9;
        make_t bb1r35b7r9 10 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=10; r=Empty}}});

        make_t bb1r35br79 0 (Black {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35br79 1;
        make_t bb1r35br79 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35br79 3;
        make_t bb1r35br79 4 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=4; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35br79 5;
        make_t bb1r35br79 6 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Red {l=Black {l=Empty; v=6; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb1r35br79 7;
        make_t bb1r35br79 8 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=8; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb1r35br79 9;
        make_t bb1r35br79 10 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=10; r=Empty}}});

        make_t bbr135b7r9 0 (Black {l=Red {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 1;
        make_t bbr135b7r9 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 3;
        make_t bbr135b7r9 4 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=4; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 5;
        make_t bbr135b7r9 6 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=6; r=Empty}; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 7;
        make_t bbr135b7r9 8 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=8; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 9;
        make_t bbr135b7r9 10 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=10; r=Empty}}});

        make_t bbr135br79 0 (Black {l=Red {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135br79 1;
        make_t bbr135br79 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135br79 3;
        make_t bbr135br79 4 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=4; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135br79 5;
        make_t bbr135br79 6 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=6; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bbr135br79 7;
        make_t bbr135br79 8 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=8; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bbr135br79 9;
        make_t bbr135br79 10 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=10; r=Empty}}});

        make_t bbr13r57b9 0 (Black {l=Red {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 1;
        make_t bbr13r57b9 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 3;
        make_t bbr13r57b9 4 (Black {l=Red {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=4; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 5;
        make_t bbr13r57b9 6 (Black {l=Red {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=6; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 7;
        make_t bbr13r57b9 8 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=8; r=Empty}; v=9; r=Empty}});
        make_f bbr13r57b9 9;
        make_t bbr13r57b9 10 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=10; r=Empty}}});

        make_t brb13b57b9 0 (Black {l=Red {l=Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 1;
        make_t brb13b57b9 2 (Black {l=Red {l=Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 3;
        make_t brb13b57b9 4 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=4; r=Empty}; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 5;
        make_t brb13b57b9 6 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Red {l=Empty; v=6; r=Empty}}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 7;
        make_t brb13b57b9 8 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=8; r=Empty}; v=9; r=Empty}});
        make_f brb13b57b9 9;
        make_t brb13b57b9 10 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=10; r=Empty}}});
      ]
    );
    "replace" >:: (
      let make t x expected =
        ~: "replace %s %i" (repr t) x (lazy (
          check_poly ~repr ~expected (replace t ~cmp x)
        ))
      in
      [
        make Empty 0 (Black {l=Empty; v=0; r=Empty});

        make b1 (-2) (Black {l=Empty; v=1; r=Red {l=Empty; v=(-2); r=Empty}});
        make b1 (-1) (Black {l=Empty; v=(-1); r=Empty});
        make b1 0 (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Empty});
        make b1 1 (Black {l=Empty; v=1; r=Empty});
        make b1 2 (Black {l=Empty; v=1; r=Red {l=Empty; v=2; r=Empty}});

        make b1r3 (-4) (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=(-4); r=Empty}});
        make b1r3 (-3) (Black {l=Empty; v=1; r=Red {l=Empty; v=(-3); r=Empty}});
        make b1r3 (-2) (Black {l=Black {l=Empty; v=1; r=Empty}; v=(-2); r=Black {l=Empty; v=3; r=Empty}});
        make b1r3 (-1) (Black {l=Empty; v=(-1); r=Red {l=Empty; v=3; r=Empty}});
        make b1r3 0 (Black {l=Red {l=Empty; v=0; r=Empty}; v=1; r=Red {l=Empty; v=3; r=Empty}});
        make b1r3 1 (Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}});
        make b1r3 2 (Black {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}});
        make b1r3 3 (Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}});
        make b1r3 4 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=4; r=Empty}});

        make br13 (-4) (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=(-4); r=Empty}});
        make br13 (-3) (Black {l=Red {l=Empty; v=1; r=Empty}; v=(-3); r=Empty});
        make br13 (-2) (Black {l=Black {l=Empty; v=1; r=Empty}; v=(-2); r=Black {l=Empty; v=3; r=Empty}});
        make br13 (-1) (Black {l=Red {l=Empty; v=(-1); r=Empty}; v=3; r=Empty});
        make br13 0 (Black {l=Black {l=Empty; v=0; r=Empty}; v=1; r=Black {l=Empty; v=3; r=Empty}});
        make br13 1 (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty});
        make br13 2 (Black {l=Black {l=Empty; v=1; r=Empty}; v=2; r=Black {l=Empty; v=3; r=Empty}});
        make br13 3 (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty});
        make br13 4 (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=4; r=Empty}});
      ]
    );
    "try_get" >:: (
      let make t x expected =
        ~: "try_get %s %i" (repr t) x (lazy (
          check_int_option ~expected (try_get t ~cmp ~cmp_k:cmp x)
        ))
      in
      let make_t t x =
        make t x (Some (Int.abs x))
      and make_f t x =
        make t x None
      in
      [
        make_f br13r5 (-6);
        make_t br13r5 (-5);
        make_f br13r5 (-4);
        make_t br13r5 (-3);
        make_f br13r5 (-2);
        make_t br13r5 (-1);
        make_f br13r5 0;
        make_t br13r5 1;
        make_f br13r5 2;
        make_t br13r5 3;
        make_f br13r5 4;
        make_t br13r5 5;
        make_f br13r5 6;
      ]
    );
    "is_empty" >:: [
      "empty" >: (lazy (check_true (is_empty Empty)));
      "not empty" >: (lazy (check_false (is_empty bb13b5r7)));
    ];
    "remove" >:: (
      let make t x expected =
        ~: "remove %s %i" (repr t) x (lazy (
          check_poly ~repr:(Tuples.Tuple2.repr ~repr_a:Bool.repr ~repr_b:repr) ~expected (remove t ~cmp ~cmp_k:cmp x)
        ))
      in
      let make_t t x expected =
        make t x (true, expected)
      and make_f t x =
        make t x (false, t)
      in
      [
        make_f Empty 0;

        make_f b1 0;
        make_t b1 1 Empty;
        make_f b1 2;

        make_f b1r3 0;
        make_t b1r3 1 (Black {l=Empty; v=3; r=Empty});
        make_f b1r3 2;
        make_t b1r3 3 (Black {l=Empty; v=1; r=Empty});
        make_f b1r3 4;

        make_f br13 0;
        make_t br13 1 (Black {l=Empty; v=3; r=Empty});
        make_f br13 2;
        make_t br13 3 (Black {l=Empty; v=1; r=Empty});
        make_f br13 4;

        make_f bb13b5 0;
        make_t bb13b5 1 (Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}});
        make_f bb13b5 2;
        make_t bb13b5 3 (Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty});
        make_f bb13b5 4;
        make_t bb13b5 5 (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty});
        make_f bb13b5 6;

        make_f br13r5 0;
        make_t br13r5 1 (Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}});
        make_f br13r5 2;
        make_t br13r5 3 (Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty});
        make_f br13r5 4;
        make_t br13r5 5 (Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty});
        make_f br13r5 6;

        make_f bb13b5r7 0;
        make_t bb13b5r7 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb13b5r7 2;
        make_t bb13b5r7 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb13b5r7 4;
        make_t bb13b5r7 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb13b5r7 6;
        make_t bb13b5r7 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}});
        make_f bb13b5r7 8;

        make_f bb13br57 0;
        make_t bb13br57 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb13br57 2;
        make_t bb13br57 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb13br57 4;
        make_t bb13br57 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb13br57 6;
        make_t bb13br57 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}});
        make_f bb13br57 8;

        make_f bb1r35b7 0;
        make_t bb1r35b7 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35b7 2;
        make_t bb1r35b7 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35b7 4;
        make_t bb1r35b7 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35b7 6;
        make_t bb1r35b7 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}});
        make_f bb1r35b7 8;

        make_f bbr135b7 0;
        make_t bbr135b7 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135b7 2;
        make_t bbr135b7 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135b7 4;
        make_t bbr135b7 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135b7 6;
        make_t bbr135b7 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}});
        make_f bbr135b7 8;

        make_f bb13br57r9 0;
        make_t bb13br57r9 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13br57r9 2;
        make_t bb13br57r9 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13br57r9 4;
        make_t bb13br57r9 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13br57r9 6;
        make_t bb13br57r9 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=9; r=Empty}});
        make_f bb13br57r9 8;
        make_t bb13br57r9 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}});
        make_f bb13br57r9 10;

        make_f bb13rb57b9 0;
        make_t bb13rb57b9 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bb13rb57b9 2;
        make_t bb13rb57b9 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9 4;
        make_t bb13rb57b9 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9 6;
        make_t bb13rb57b9 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=9; r=Empty}});
        make_f bb13rb57b9 8;
        make_t bb13rb57b9 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}});
        make_f bb13rb57b9 10;

        make_f bb1r35b7r9 0;
        make_t bb1r35b7r9 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 2;
        make_t bb1r35b7r9 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bb1r35b7r9 4;
        make_t bb1r35b7r9 5 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bb1r35b7r9 6;
        make_t bb1r35b7r9 7 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=9; r=Empty}});
        make_f bb1r35b7r9 8;
        make_t bb1r35b7r9 9 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35b7r9 10;

        make_f bb1r35br79 0;
        make_t bb1r35br79 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35br79 2;
        make_t bb1r35br79 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35br79 4;
        make_t bb1r35br79 5 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bb1r35br79 6;
        make_t bb1r35br79 7 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=9; r=Empty}});
        make_f bb1r35br79 8;
        make_t bb1r35br79 9 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bb1r35br79 10;

        make_f bbr135b7r9 0;
        make_t bbr135b7r9 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 2;
        make_t bbr135b7r9 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Red {l=Empty; v=9; r=Empty}}});
        make_f bbr135b7r9 4;
        make_t bbr135b7r9 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr135b7r9 6;
        make_t bbr135b7r9 7 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr135b7r9 8;
        make_t bbr135b7r9 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135b7r9 10;

        make_f bbr135br79 0;
        make_t bbr135br79 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135br79 2;
        make_t bbr135br79 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135br79 4;
        make_t bbr135br79 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr135br79 6;
        make_t bbr135br79 7 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr135br79 8;
        make_t bbr135br79 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}});
        make_f bbr135br79 10;

        make_f bbr13r57b9 0;
        make_t bbr13r57b9 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 2;
        make_t bbr13r57b9 3 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 4;
        make_t bbr13r57b9 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9 6;
        make_t bbr13r57b9 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=9; r=Empty}});
        make_f bbr13r57b9 8;
        make_t bbr13r57b9 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}});
        make_f bbr13r57b9 10;

        make_f brb13b57b9 0;
        make_t brb13b57b9 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 2;
        make_t brb13b57b9 3 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 4;
        make_t brb13b57b9 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9 6;
        make_t brb13b57b9 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=9; r=Empty}});
        make_f brb13b57b9 8;
        make_t brb13b57b9 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Red {l=Empty; v=5; r=Empty}; v=7; r=Empty}});
        make_f brb13b57b9 10;

        make_f bb13rb57b9r11 0;
        make_t bb13rb57b9r11 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57b9r11 2;
        make_t bb13rb57b9r11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57b9r11 4;
        make_t bb13rb57b9r11 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57b9r11 6;
        make_t bb13rb57b9r11 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57b9r11 8;
        make_t bb13rb57b9r11 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57b9r11 10;
        make_t bb13rb57b9r11 11 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57b9r11 12;

        make_f bb13rb57br911 0;
        make_t bb13rb57br911 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f bb13rb57br911 2;
        make_t bb13rb57br911 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57br911 4;
        make_t bb13rb57br911 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57br911 6;
        make_t bb13rb57br911 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57br911 8;
        make_t bb13rb57br911 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb57br911 10;
        make_t bb13rb57br911 11 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb57br911 12;

        make_f bb13rb5r79b11 0;
        make_t bb13rb5r79b11 1 (Black {l=Red {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f bb13rb5r79b11 2;
        make_t bb13rb5r79b11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb5r79b11 4;
        make_t bb13rb5r79b11 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb5r79b11 6;
        make_t bb13rb5r79b11 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb5r79b11 8;
        make_t bb13rb5r79b11 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rb5r79b11 10;
        make_t bb13rb5r79b11 11 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rb5r79b11 12;

        make_f bb13rbr579b11 0;
        make_t bb13rbr579b11 1 (Black {l=Red {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f bb13rbr579b11 2;
        make_t bb13rbr579b11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rbr579b11 4;
        make_t bb13rbr579b11 5 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rbr579b11 6;
        make_t bb13rbr579b11 7 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rbr579b11 8;
        make_t bb13rbr579b11 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb13rbr579b11 10;
        make_t bb13rbr579b11 11 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f bb13rbr579b11 12;

        make_f bb1r35br79r11 0;
        make_t bb1r35br79r11 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb1r35br79r11 2;
        make_t bb1r35br79r11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb1r35br79r11 4;
        make_t bb1r35br79r11 5 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb1r35br79r11 6;
        make_t bb1r35br79r11 7 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb1r35br79r11 8;
        make_t bb1r35br79r11 9 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=11; r=Empty}});
        make_f bb1r35br79r11 10;
        make_t bb1r35br79r11 11 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35br79r11 12;

        make_f bb1r35rb79b11 0;
        make_t bb1r35rb79b11 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb1r35rb79b11 2;
        make_t bb1r35rb79b11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bb1r35rb79b11 4;
        make_t bb1r35rb79b11 5 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb1r35rb79b11 6;
        make_t bb1r35rb79b11 7 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bb1r35rb79b11 8;
        make_t bb1r35rb79b11 9 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=11; r=Empty}});
        make_f bb1r35rb79b11 10;
        make_t bb1r35rb79b11 11 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bb1r35rb79b11 12;

        make_f bbr135br79r11 0;
        make_t bbr135br79r11 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr135br79r11 2;
        make_t bbr135br79r11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr135br79r11 4;
        make_t bbr135br79r11 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr135br79r11 6;
        make_t bbr135br79r11 7 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr135br79r11 8;
        make_t bbr135br79r11 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=11; r=Empty}});
        make_f bbr135br79r11 10;
        make_t bbr135br79r11 11 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135br79r11 12;

        make_f bbr135rb79b11 0;
        make_t bbr135rb79b11 1 (Black {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bbr135rb79b11 2;
        make_t bbr135rb79b11 3 (Black {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Red {l=Black {l=Empty; v=7; r=Empty}; v=9; r=Black {l=Empty; v=11; r=Empty}}});
        make_f bbr135rb79b11 4;
        make_t bbr135rb79b11 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr135rb79b11 6;
        make_t bbr135rb79b11 7 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr135rb79b11 8;
        make_t bbr135rb79b11 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=11; r=Empty}});
        make_f bbr135rb79b11 10;
        make_t bbr135rb79b11 11 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f bbr135rb79b11 12;

        make_f bbr13r57b9r11 0;
        make_t bbr13r57b9r11 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr13r57b9r11 2;
        make_t bbr13r57b9r11 3 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr13r57b9r11 4;
        make_t bbr13r57b9r11 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f bbr13r57b9r11 6;
        make_t bbr13r57b9r11 7 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f bbr13r57b9r11 8;
        make_t bbr13r57b9r11 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=11; r=Empty}});
        make_f bbr13r57b9r11 10;
        make_t bbr13r57b9r11 11 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57b9r11 12;

        make_f bbr13r57br911 0;
        make_t bbr13r57br911 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f bbr13r57br911 2;
        make_t bbr13r57br911 3 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f bbr13r57br911 4;
        make_t bbr13r57br911 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f bbr13r57br911 6;
        make_t bbr13r57br911 7 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f bbr13r57br911 8;
        make_t bbr13r57br911 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=11; r=Empty}});
        make_f bbr13r57br911 10;
        make_t bbr13r57br911 11 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f bbr13r57br911 12;

        make_f brb13b57b9r11 0;
        make_t brb13b57b9r11 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f brb13b57b9r11 2;
        make_t brb13b57b9r11 3 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f brb13b57b9r11 4;
        make_t brb13b57b9r11 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Red {l=Empty; v=11; r=Empty}}});
        make_f brb13b57b9r11 6;
        make_t brb13b57b9r11 7 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b57b9r11 8;
        make_t brb13b57b9r11 9 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b57b9r11 10;
        make_t brb13b57b9r11 11 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57b9r11 12;

        make_f brb13b57br911 0;
        make_t brb13b57br911 1 (Black {l=Black {l=Empty; v=3; r=Red {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f brb13b57br911 2;
        make_t brb13b57br911 3 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=5; r=Empty}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f brb13b57br911 4;
        make_t brb13b57br911 5 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=7; r=Black {l=Red {l=Empty; v=9; r=Empty}; v=11; r=Empty}});
        make_f brb13b57br911 6;
        make_t brb13b57br911 7 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b57br911 8;
        make_t brb13b57br911 9 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b57br911 10;
        make_t brb13b57br911 11 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=7; r=Black {l=Empty; v=9; r=Empty}});
        make_f brb13b57br911 12;

        make_f brb13b5r79b11 0;
        make_t brb13b5r79b11 1 (Black {l=Red {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b5r79b11 2;
        make_t brb13b5r79b11 3 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b5r79b11 4;
        make_t brb13b5r79b11 5 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b5r79b11 6;
        make_t brb13b5r79b11 7 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13b5r79b11 8;
        make_t brb13b5r79b11 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=11; r=Empty}}});
        make_f brb13b5r79b11 10;
        make_t brb13b5r79b11 11 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f brb13b5r79b11 12;

        make_f brb13br579b11 0;
        make_t brb13br579b11 1 (Black {l=Red {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13br579b11 2;
        make_t brb13br579b11 3 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13br579b11 4;
        make_t brb13br579b11 5 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13br579b11 6;
        make_t brb13br579b11 7 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb13br579b11 8;
        make_t brb13br579b11 9 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=11; r=Empty}}});
        make_f brb13br579b11 10;
        make_t brb13br579b11 11 (Black {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Red {l=Black {l=Empty; v=5; r=Empty}; v=7; r=Black {l=Empty; v=9; r=Empty}}});
        make_f brb13br579b11 12;

        make_f brb1r35b79b11 0;
        make_t brb1r35b79b11 1 (Black {l=Red {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb1r35b79b11 2;
        make_t brb1r35b79b11 3 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb1r35b79b11 4;
        make_t brb1r35b79b11 5 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb1r35b79b11 6;
        make_t brb1r35b79b11 7 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brb1r35b79b11 8;
        make_t brb1r35b79b11 9 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=11; r=Empty}});
        make_f brb1r35b79b11 10;
        make_t brb1r35b79b11 11 (Black {l=Black {l=Empty; v=1; r=Red {l=Empty; v=3; r=Empty}}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f brb1r35b79b11 12;

        make_f brbr135b79b11 0;
        make_t brbr135b79b11 1 (Black {l=Red {l=Black {l=Empty; v=3; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brbr135b79b11 2;
        make_t brbr135b79b11 3 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=5; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brbr135b79b11 4;
        make_t brbr135b79b11 5 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=7; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brbr135b79b11 6;
        make_t brbr135b79b11 7 (Black {l=Red {l=Black {l=Empty; v=1; r=Empty}; v=3; r=Black {l=Empty; v=5; r=Empty}}; v=9; r=Black {l=Empty; v=11; r=Empty}});
        make_f brbr135b79b11 8;
        make_t brbr135b79b11 9 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=11; r=Empty}});
        make_f brbr135b79b11 10;
        make_t brbr135b79b11 11 (Black {l=Black {l=Red {l=Empty; v=1; r=Empty}; v=3; r=Empty}; v=5; r=Black {l=Red {l=Empty; v=7; r=Empty}; v=9; r=Empty}});
        make_f brbr135b79b11 12;
      ]
    );
    "heterogeneous" >:: (
      let cmp_k (`Int (x:int)) (y:int) = Int.compare x y in
      [
        "try_get" >: (lazy (
          check_some_42 (try_get (Black {l=Empty; v=42; r=Empty}) ~cmp ~cmp_k (`Int 42))
        ));
        "remove" >: (lazy (
          check_poly
            ~repr:(Tuples.Tuple2.repr ~repr_a:Bool.repr ~repr_b:repr)
            ~expected:(true, Empty)
            (remove (Black {l=Empty; v=42; r=Empty}) ~cmp ~cmp_k (`Int 42))
        ));
      ]
    );
    "to_list" >:: (
      let make t expected =
        (repr t) >: (lazy (
          check_int_list ~expected (to_list ~cmp t)
        ))
      in
      [
        make empty [];
        make bb13br57r9 [1; 3; 5; 7; 9];
      ]
    );
    "size" >:: (
      let make t expected =
        (repr t) >: (lazy (
          check_int ~expected (size ~cmp t)
        ))
      in
      [
        make empty 0;
        make bb13br57r9 5;
      ]
    );
  ]
end
