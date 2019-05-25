open Idd_
open Base

module Make(A : Boolean.Algebra) = struct

  module Base = struct
    let constants = [(true, A.tru); (false, A.fls)]

    (* basic *)
    let%test "tru != fls" = not A.(tru == fls)
    let%test "of_bool true = tru" = A.(of_bool true == tru)
    let%test "of_bool false = fls" = A.(of_bool false == fls)

    (* negation *)
    let%test "negation of constants correct" =
      List.for_all constants ~f:(fun (b, t) ->
        A.(of_bool (not b) == !t) &&
        A.(of_bool b == !(!t))
      )

    (* binary operators *)
    let%test "binary operations on constants correct" =
      List.for_all [((&&), A.(&&)); ((||), A.(||))] ~f:(fun (op_b, op_t) ->
        List.(for_all (cartesian_product constants constants)) ~f:(
          fun ((b1, t1), (b2, t2)) ->
            A.(op_t t1 t2 == of_bool (op_b b1 b2))
        )
      )
  end


  module WithVars = struct
    let vars = [|"x"; "y";|]
    let n = Array.length vars

    let%test "declare_var succeeds" =
      Array.map vars ~f:A.declare_var
      |> Array.for_all ~f:(function `Ok -> true | `Duplicate -> false)

    let%test "double declaration fails" =
      Array.for_all vars ~f:(fun v ->
        A.declare_var v
        |> function `Duplicate -> true | `Ok -> false
      )

    let rec all_formulas (n : int) : A.t list =
      if n <= 0 then
        [A.tru; A.fls]
      else
        let fs = all_formulas (n-1) in
        let v = vars.(n-1) in
        List.cartesian_product fs fs
        |> List.map ~f:(fun (t1, t2) ->
          A.((var v && t1) || (!(var v) && t2))
        )

    let formulas : A.t list = all_formulas n
    let two_formulas : (A.t*A.t) list =
      List.cartesian_product formulas formulas
    let three_formulas : (A.t*A.t*A.t) list =
      List.cartesian_product two_formulas formulas
      |> List.map ~f:(fun ((a,b),c) -> (a,b,c))

    (* equivalence *)
    let%test "== discriminates" = List.for_all formulas ~f:(fun a ->
      A.(a == a) &&
      List.for_all formulas ~f:(fun b -> phys_equal a b || not A.(a == b))
    )

    (* boolean axioms *)
    let%test "&& commutative" = List.for_all two_formulas ~f:(fun (a,b) ->
      A.((a && b) == (b && a))
    )
    let%test "|| commutative" = List.for_all two_formulas ~f:(fun (a,b) ->
      A.((a || b) == (b || a))
    )
    let%test "&& associative" = List.for_all three_formulas ~f:(fun (a,b,c) ->
      A.(((a && b) && c) == (a && (b && c)))
    )
    let%test "|| associative" = List.for_all three_formulas ~f:(fun (a,b,c) ->
      A.(((a || b) || c) == (a || (b || c)))
    )
    let%test "true is && identity" = List.for_all formulas ~f:(fun a ->
      A.(a == (a && tru))
    )
    let%test "false is || identity" = List.for_all formulas ~f:(fun a ->
      A.(a == (a || fls))
    )
    let%test "&& distributes over ||" = List.for_all three_formulas ~f:(
      fun (a,b,c) ->
        A.((a && (b||c)) == (a&&b || a&&c))
    )
    let%test "|| distributes over &&" = List.for_all three_formulas ~f:(
      fun (a,b,c) ->
        A.((a || b&&c) == ((a||b) && (a||c)))
    )
    let%test "a && !a == 0" = List.for_all formulas ~f:(fun a ->
      A.((a && !a) == fls)
    )
    let%test "a || !a == 1" = List.for_all formulas ~f:(fun a ->
      A.((a || !a) == tru)
    )

    (* theorems *)
    let%test "a && a == a" = List.for_all formulas ~f:(fun a ->
      A.((a && a) == a)
    )
    let%test "a || a == a" = List.for_all formulas ~f:(fun a ->
      A.((a || a) == a)
    )
    let%test "a && 0 == 0" = List.for_all formulas ~f:(fun a ->
      A.((a && fls) == fls)
    )
    let%test "a || 1 == 1" = List.for_all formulas ~f:(fun a ->
      A.((a || tru) == tru)
    )
    let%test "a && b || b == b" = List.for_all two_formulas ~f:(fun (a,b) ->
      A.((a&&b || b) == b)
    )
    let%test "(a || b) && b == b" = List.for_all two_formulas ~f:(fun (a,b) ->
      A.(((a||b) && b) == b)
    )
    let%test "DeMorgan I" = List.for_all two_formulas ~f:(fun (a,b) ->
      A.(!(a && b) == (!a || !b))
    )
    let%test "DeMorgan II" = List.for_all two_formulas ~f:(fun (a,b) ->
      A.(!(a || b) == (!a && !b))
    )

  end


end
