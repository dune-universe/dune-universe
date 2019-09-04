module Basic = struct
  #include "Ringoid.signatures.Basic.ml"
end

module Operators = struct
  #include "Ringoid.signatures.Operators.ml"

  module Make0(M: sig
    type t

    val negate: t -> t
    val add: t -> t -> t
    val substract: t -> t -> t
    val multiply: t -> t -> t
    val divide: t -> t -> t

    val exponentiate: t -> int -> t
  end) = struct
    open M

    let (~+) x =
      identity x

    let (~-) x =
      negate x

    let (+) x y =
      add x y

    let (-) x y =
      substract x y

    let ( * ) x y =
      multiply x y

    let (/) x y =
      divide x y

    let ( ** ) x n =
      exponentiate x n
  end
end

#include "Ringoid.signatures.ml"

(* @todo Fix spelling of 'subtract' *)
module Substract = struct
  module Make0(M: sig
    type t

    val negate: t -> t
    val add: t -> t -> t
  end) = struct
    open M

    let substract x y =
      add x (negate y)
  end
end

module Square = struct
  module Make0(M: sig
    type t

    val multiply: t -> t -> t
  end) = struct
    open M

    let square x =
      multiply x x
  end
end

module Exponentiate = struct
  module Make0(M: sig
    type t

    val one: t

    val square: t -> t
    val multiply: t -> t -> t

    val exponentiate_negative_exponent: exponentiate:(t -> int -> t) -> t -> int -> t
  end) = struct
    open M
    open Int.O

    let exponentiate x n =
      let rec aux y x n =
        if n < 0 then
          exponentiate_negative_exponent ~exponentiate:(aux one) x n
        else if n = 0 then
          y
        else if n = 1 then
          multiply x y
        else if n mod 2 = 0 then
          aux y (square x) (n / 2)
        else
          aux (multiply x y) (square x) ((n - 1) / 2)
      in
      aux one x n
  end
end

module Tests = struct
  open Testing

  module Examples = struct
    module type S0 = sig
      type t

      val add_substract: (t * t * t) list
      val negate: (t * t) list
      val multiply: (t * t * t) list
      val divide: (t * t * t) list
      val exponentiate: (t * int * t) list
    end
  end

  module Make0(M: sig
    include S0
    include Representable.S0 with type t := t
    include Equatable.Basic.S0 with type t := t
  end)(E: Examples.S0 with type t := M.t): sig val test: Test.t end = struct
    open M
    open M.O

    module E = struct
      include E

      let add_substract = add_substract @ [
        (zero, zero, zero);
        (one, zero, one);
      ]

      let negate = negate @ [
        (zero, zero);
      ]

      let multiply = multiply @ [
        (zero, zero, zero);
        (one, zero, zero);
      ]

      let divide = divide @ [
        (zero, one, zero);
        (one, one, one);
      ]

      let exponentiate = exponentiate @ [
        (zero, 0, one);
        (zero, 1, zero);
        (zero, 7, zero);
        (one, 0, one);
        (one, 1, one);
        (one, 7, one);
      ]
    end

    let check = check ~repr ~equal

    let test = "Ringoid" >:: (
      E.add_substract
      |> List.flat_map ~f:(fun (x, y, z) ->
        let rx = repr x and ry = repr y and rz = repr z in
        [
          ~: "add %s %s" rx ry (lazy (check ~expected:z (add x y)));
          ~: "add %s %s" ry rx (lazy (check ~expected:z (add y x)));
          ~: "%s + %s" rx ry (lazy (check ~expected:z (x + y)));
          ~: "%s + %s" ry rx (lazy (check ~expected:z (y + x)));
          ~: "sub %s %s" rz ry (lazy (check ~expected:x (substract z y)));
          ~: "%s - %s" rz ry (lazy (check ~expected:x (z - y)));
          ~: "sub %s %s" rz rx (lazy (check ~expected:y (substract z x)));
          ~: "%s - %s" rz rx (lazy (check ~expected:y (z - x)));
        ]
      )
    ) @ (
      E.negate
      |> List.flat_map ~f:(fun (x, y) ->
        let rx = repr x and ry = repr y in
        [
          ~: "negate %s" rx (lazy (check ~expected:y (negate x)));
          ~: "negate %s" ry (lazy (check ~expected:x (negate y)));
          ~: "-%s" rx (lazy (check ~expected:y (-x)));
          ~: "-%s" ry (lazy (check ~expected:x (-y)));
          ~: "substract zero %s" rx (lazy (check ~expected:y (substract zero x)));
          ~: "substract zero %s" ry (lazy (check ~expected:x (substract zero y)));
          ~: "zero - %s" rx (lazy (check ~expected:y (zero - x)));
          ~: "zero - %s" ry (lazy (check ~expected:x (zero - y)));
          ~: "add %s %s" rx ry (lazy (check ~expected:zero (add x y)));
          ~: "add %s %s" ry rx (lazy (check ~expected:zero (add y x)));
          ~: "%s + %s" rx ry (lazy (check ~expected:zero (x + y)));
          ~: "%s + %s" ry rx (lazy (check ~expected:zero (y + x)));
          ~: "square %s" rx (lazy (check ~expected:(negate (multiply x y)) (square x)));
          ~: "square %s" ry (lazy (check ~expected:(negate (multiply x y)) (square y)));
        ]
      )
    ) @ (
      E.multiply
      |> List.flat_map ~f:(fun (x, y, expected) ->
        let rx = repr x and ry = repr y in
        [
          ~: "multiply %s %s" rx ry (lazy (check ~expected (multiply x y)));
          ~: "%s * %s" rx ry (lazy (check ~expected (x * y)));
        ]
      )
    ) @ (
      E.divide
      |> List.flat_map ~f:(fun (x, y, expected) ->
        let rx = repr x and ry = repr y in
        [
          ~: "divide %s %s" rx ry (lazy (check ~expected (divide x y)));
          ~: "%s / %s" rx ry (lazy (check ~expected (x / y)));
        ]
      )
    ) @ (
      E.exponentiate
      |> List.flat_map ~f:(fun (x, n, expected) ->
        let rx = repr x in
        [
          ~: "exponentiate %s %n" rx n (lazy (check ~expected (exponentiate x n)));
          ~: "%s ** %n" rx n (lazy (check ~expected (x ** n)));
        ]
      )
    )
  end
end
