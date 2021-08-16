@type expr =
  Add of expr * expr
| Mul of expr * expr
| Int of GT.int
| Var of GT.string with show, foldl, gmap


class show' fself =
  object inherit [_] @expr[show] fself
    method! c_Var _ _ s = s
  end

class show'' fself =
  object inherit  show' fself
    method! c_Int _ _ i = string_of_int i
  end

class show''' fself =
  object inherit  show'' fself
    method! c_Add i _ x y = (fself i x) ^ " + " ^ (fself i y)
    method! c_Mul i _ x y = (fself i x) ^ " * " ^ (fself i y)
  end

class show'''' fself =
  let enclose op p x y =
    let prio = function
      | Add (_, _) -> 1
      | Mul (_, _) -> 2
      | _ -> 3
    in
    let bracket f x = if f then "(" ^ x ^ ")" else x in
    bracket (p >  prio x) (fself () x) ^ op ^
    bracket (p >= prio y) (fself () y)
  in
  object
    inherit show''' fself
    method! c_Mul _ _ x y = enclose "*" 2 x y
    method! c_Add _ _ x y = enclose "+" 1 x y
  end

let vars =
  let module S = Set.Make (String) in
  let module M =
    struct

      class vars fself =
        object inherit [S.t, _] @expr[foldl] fself
          method! c_Var s _ x = S.add x s
        end

     let vars e = S.elements (GT.transform(expr) (new vars) S.empty e)

    end
  in M.vars

let eval =
  let module M =
    struct

      class eval fself =
        object inherit [string -> int, expr, int] @expr
          method c_Var s _ x = s x
          method c_Int _ _ i = i
          method c_Add s _ x y = (fself s x) + (fself s y)
          method c_Mul s _ x y = (fself s x) * (fself s y)
        end

      let eval s = GT.transform(expr) new eval s

    end
  in M.eval

let simplify =
  let module M =
    struct

      class simplify_add =
        let (+) a b =
          match a, b with
          | Int a, Int b -> Int (a+b)
          | Int a, Add (Int b, c)
          | Add (Int a, c), Int b -> Add (Int (a+b), c)
          | Add (Int a, c), Add (Int b, d) -> Add (Int (a+b), Add (c, d))
          | _, Int _ -> Add (b, a)
          | _ -> Add (a, b)
        in
        fun fself -> object inherit [_,_] @expr[gmap] fself
          method! c_Add _ _ x y = (fself () x) + (fself () y)
        end

      class simplify_mul =
        let ( * ) a b =
          match a, b with
          | Int a, Int b -> Int (a*b)
          | Int a, Mul (Int b, c)
          | Mul (Int a, c), Int b -> Mul (Int (a*b), c)
          | Mul (Int a, c), Mul (Int b, d) -> Mul (Int (a*b), Add (c, d))
          | _, Int _ -> Mul (b, a)
          | _ -> Mul (a, b)
        in
        fun fself -> object inherit simplify_add fself
          method! c_Mul _ _ x y = (fself () x) * (fself () y)
        end

      class simplify_all fself =
        object inherit simplify_mul fself as super
          method! c_Add i it x y =
            match super#c_Add i it x y with
            | Add (Int 0, a) -> a
            | x -> x
          method! c_Mul i it x y =
            match super#c_Mul i it x y with
            | Mul (Int 1, a) -> a
            | Mul (Int 0, _) -> Int 0
            | x -> x
        end

      let simplify = GT.transform(expr) new simplify_all ()

    end
  in M.simplify

let _ =
  let x = Mul (Var "a", Add (Int 1, Var "b")) in
  let y = Add (Int 1, Add (Var "a", Int 3)) in
  let z = Add (Int 0, Mul (Add (Int 2, Int 3), Mul (Var "a", Int 4))) in
  let t = Mul (Mul (Int 1, Var "x"), Var "y") in
  Printf.printf "%s\n" (GT.transform(expr) new @expr[show] () x);
  Printf.printf "%s\n" (GT.transform(expr) new show' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show'' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show'''' () x);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () y);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () z);
  Printf.printf "%s\n" (GT.transform(expr) new show'''' () z);
  Printf.printf "%d\n" (eval (function "a" -> 1 | "b" -> 2) x);
  Printf.printf "%s\n" (GT.transform(expr) new show''' () (simplify y));
  Printf.printf "%s\n" (GT.transform(expr) new show''' () (simplify z));
  Printf.printf "%s\n" (GT.transform(expr) new show'''' () (simplify t));
  Printf.printf "[%s]\n"
    (GT.transform(GT.list)
       (new @GT.list[show] (fun _ s -> s))
       ()
       (vars x)
    )
