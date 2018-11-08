module Basic = struct
  #include "Equatable.signatures.Basic.ml"

  module Specialize1(M: S1)(A: S0): S0 with type t = A.t M.t = struct
    type t = A.t M.t

    let equal x y =
      M.equal x y ~equal_a:A.equal
  end

  module Specialize2(M: S2)(A: S0)(B: S0): S0 with type t = (A.t, B.t) M.t = struct
    type t = (A.t, B.t) M.t

    let equal x y =
      M.equal x y ~equal_a:A.equal ~equal_b:B.equal
  end

  module Specialize3(M: S3)(A: S0)(B: S0)(C: S0): S0 with type t = (A.t, B.t, C.t) M.t = struct
    type t = (A.t, B.t, C.t) M.t

    let equal x y =
      M.equal x y ~equal_a:A.equal ~equal_b:B.equal ~equal_c:C.equal
  end

  module Specialize4(M: S4)(A: S0)(B: S0)(C: S0)(D: S0): S0 with type t = (A.t, B.t, C.t, D.t) M.t = struct
    type t = (A.t, B.t, C.t, D.t) M.t

    let equal x y =
      M.equal x y ~equal_a:A.equal ~equal_b:B.equal ~equal_c:C.equal ~equal_d:D.equal
  end

  module Specialize5(M: S5)(A: S0)(B: S0)(C: S0)(D: S0)(E: S0): S0 with type t = (A.t, B.t, C.t, D.t, E.t) M.t = struct
    type t = (A.t, B.t, C.t, D.t, E.t) M.t

    let equal x y =
      M.equal x y ~equal_a:A.equal ~equal_b:B.equal ~equal_c:C.equal ~equal_d:D.equal ~equal_e:E.equal
  end
end

module Operators = struct
  #include "Equatable.signatures.Operators.ml"

  module Make0(M: sig
    type t

    val equal: t -> t
      -> bool
    val different: t -> t
      -> bool
  end) = struct
    open M

    let (=) x y =
      equal x y

    let (<>) x y =
      different x y
  end
end

#include "Equatable.signatures.ml"

module Different = struct
  module Make0(M: sig
    type t

    val equal: t -> t
      -> bool
  end) = struct
    open M

    let different x y =
      not (equal x y)
  end

  module Make1(M: sig
    type 'a t

    val equal: 'a t -> 'a t
      -> equal_a:('a -> 'a -> bool)
      -> bool
  end) = struct
    open M

    let different x y ~equal_a =
      not (equal x y ~equal_a)
  end

  module Make2(M: sig
    type ('a, 'b) t

    val equal: ('a, 'b) t -> ('a, 'b) t
      -> equal_a:('a -> 'a -> bool)
      -> equal_b:('b -> 'b -> bool)
      -> bool
  end) = struct
    open M

    let different x y ~equal_a ~equal_b =
      not (equal x y ~equal_a ~equal_b)
  end

  module Make3(M: sig
    type ('a, 'b, 'c) t

    val equal: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
      -> equal_a:('a -> 'a -> bool)
      -> equal_b:('b -> 'b -> bool)
      -> equal_c:('c -> 'c -> bool)
      -> bool
  end) = struct
    open M

    let different x y ~equal_a ~equal_b ~equal_c =
      not (equal x y ~equal_a ~equal_b ~equal_c)
  end

  module Make4(M: sig
    type ('a, 'b, 'c, 'd) t

    val equal: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
      -> equal_a:('a -> 'a -> bool)
      -> equal_b:('b -> 'b -> bool)
      -> equal_c:('c -> 'c -> bool)
      -> equal_d:('d -> 'd -> bool)
      -> bool
  end) = struct
    open M

    let different x y ~equal_a ~equal_b ~equal_c ~equal_d =
      not (equal x y ~equal_a ~equal_b ~equal_c ~equal_d)
  end

  module Make5(M: sig
    type ('a, 'b, 'c, 'd, 'e) t

    val equal: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
      -> equal_a:('a -> 'a -> bool)
      -> equal_b:('b -> 'b -> bool)
      -> equal_c:('c -> 'c -> bool)
      -> equal_d:('d -> 'd -> bool)
      -> equal_e:('e -> 'e -> bool)
      -> bool
  end) = struct
    open M

    let different x y ~equal_a ~equal_b ~equal_c ~equal_d ~equal_e =
      not (equal x y ~equal_a ~equal_b ~equal_c ~equal_d ~equal_e)
  end
end

module Specialize1(M: S1)(A: Basic.S0): S0 with type t = A.t M.t = struct
  module Self = struct
    include Basic.Specialize1(M)(A)

    let different x y =
      M.different x y ~equal_a:A.equal
  end

  module O = Operators.Make0(Self)

  include Self
end

module Specialize2(M: S2)(A: Basic.S0)(B: Basic.S0): S0 with type t = (A.t, B.t) M.t = struct
  module Self = struct
    include Basic.Specialize2(M)(A)(B)

    let different x y =
      M.different x y ~equal_a:A.equal ~equal_b:B.equal
  end

  module O = Operators.Make0(Self)

  include Self
end

module Specialize3(M: S3)(A: Basic.S0)(B: Basic.S0)(C: Basic.S0): S0 with type t = (A.t, B.t, C.t) M.t = struct
  module Self = struct
    include Basic.Specialize3(M)(A)(B)(C)

    let different x y =
      M.different x y ~equal_a:A.equal ~equal_b:B.equal ~equal_c:C.equal
  end

  module O = Operators.Make0(Self)

  include Self
end

module Specialize4(M: S4)(A: Basic.S0)(B: Basic.S0)(C: Basic.S0)(D: Basic.S0): S0 with type t = (A.t, B.t, C.t, D.t) M.t = struct
  module Self = struct
    include Basic.Specialize4(M)(A)(B)(C)(D)

    let different x y =
      M.different x y ~equal_a:A.equal ~equal_b:B.equal ~equal_c:C.equal ~equal_d:D.equal
  end

  module O = Operators.Make0(Self)

  include Self
end

module Specialize5(M: S5)(A: Basic.S0)(B: Basic.S0)(C: Basic.S0)(D: Basic.S0)(E: Basic.S0): S0 with type t = (A.t, B.t, C.t, D.t, E.t) M.t = struct
  module Self = struct
    include Basic.Specialize5(M)(A)(B)(C)(D)(E)

    let different x y =
      M.different x y ~equal_a:A.equal ~equal_b:B.equal ~equal_c:C.equal ~equal_d:D.equal ~equal_e:E.equal
  end

  module O = Operators.Make0(Self)

  include Self
end

module Tests = struct
  open Testing

  module Examples = struct
    module type S0 = sig
      type t

      val equal: t list list
      val different: (t * t) list
    end

    module type Element = sig
      include Basic.S0
      include Representable.S0 with type t := t
    end

    module type S1 = sig
      module A: Element

      type 'a t

      val equal: A.t t list list
      val different: (A.t t * A.t t) list
    end

    module type S2 = sig
      module A: Element
      module B: Element

      type ('a, 'b) t

      val equal: (A.t, B.t) t list list
      val different: ((A.t, B.t) t * (A.t, B.t) t) list
    end

    module type S3 = sig
      module A: Element
      module B: Element
      module C: Element

      type ('a, 'b, 'c) t

      val equal: (A.t, B.t, C.t) t list list
      val different: ((A.t, B.t, C.t) t * (A.t, B.t, C.t) t) list
    end

    module type S4 = sig
      module A: Element
      module B: Element
      module C: Element
      module D: Element

      type ('a, 'b, 'c, 'd) t

      val equal: (A.t, B.t, C.t, D.t) t list list
      val different: ((A.t, B.t, C.t, D.t) t * (A.t, B.t, C.t, D.t) t) list
    end

    module type S5 = sig
      module A: Element
      module B: Element
      module C: Element
      module D: Element
      module E: Element

      type ('a, 'b, 'c, 'd, 'e) t

      val equal: (A.t, B.t, C.t, D.t, E.t) t list list
      val different: ((A.t, B.t, C.t, D.t, E.t) t * (A.t, B.t, C.t, D.t, E.t) t) list
    end
  end

  module Make0(M: sig
    include S0
    include Representable.S0 with type t := t
  end)(E: Examples.S0 with type t := M.t) = struct
    open M
    open M.O

    let test = "Equatable" >:: (
      E.equal
      |> List.flat_map ~f:(fun xs ->
        List.cartesian_product xs xs
        |> List.flat_map ~f:(fun (x, y) ->
          let rx = repr x and ry = repr y in
          [
            ~: "equal %s %s" rx ry (lazy (check_true (equal x y)));
            ~: "different %s %s" rx ry (lazy (check_false (different x y)));
            ~: "%s = %s" rx ry (lazy (check_true (x = y)));
            ~: "%s <> %s" rx ry (lazy (check_false (x <> y)));

            ~: "equal %s %s" ry rx (lazy (check_true (equal y x)));
            ~: "different %s %s" ry rx (lazy (check_false (different y x)));
            ~: "%s = %s" ry rx (lazy (check_true (y = x)));
            ~: "%s <> %s" ry rx (lazy (check_false (y <> x)));
          ]
        )
      )
    ) @ (
      E.different
      |> List.flat_map ~f:(fun (x, y) ->
        let rx = repr x and ry = repr y in
        [
          ~: "equal %s %s" rx ry (lazy (check_false (equal x y)));
          ~: "different %s %s" rx ry (lazy (check_true (different x y)));
          ~: "%s = %s" rx ry (lazy (check_false (x = y)));
          ~: "%s <> %s" rx ry (lazy (check_true (x <> y)));

          ~: "equal %s %s" ry rx (lazy (check_false (equal y x)));
          ~: "different %s %s" ry rx (lazy (check_true (different y x)));
          ~: "%s = %s" ry rx (lazy (check_false (y = x)));
          ~: "%s <> %s" ry rx (lazy (check_true (y <> x)));
        ]
      )
    )
  end

  module Make1(M: sig
    include S1
    include Representable.S1 with type 'a t := 'a t
  end)(E: Examples.S1 with type 'a t := 'a M.t) = Make0(struct
    include Specialize1(M)(E.A)
    include (Representable.Specialize1(M)(E.A): Representable.S0 with type t := t)
  end)(E)

  module Make2(M: sig
    include S2
    include Representable.S2 with type ('a, 'b) t := ('a, 'b) t
  end)(E: Examples.S2 with type ('a, 'b) t := ('a, 'b) M.t) = Make0(struct
    include Specialize2(M)(E.A)(E.B)
    include (Representable.Specialize2(M)(E.A)(E.B): Representable.S0 with type t := t)
  end)(E)

  module Make3(M: sig
    include S3
    include Representable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  end)(E: Examples.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t) = Make0(struct
    include Specialize3(M)(E.A)(E.B)(E.C)
    include (Representable.Specialize3(M)(E.A)(E.B)(E.C): Representable.S0 with type t := t)
  end)(E)

  module Make4(M: sig
    include S4
    include Representable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
  end)(E: Examples.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) M.t) = Make0(struct
    include Specialize4(M)(E.A)(E.B)(E.C)(E.D)
    include (Representable.Specialize4(M)(E.A)(E.B)(E.C)(E.D): Representable.S0 with type t := t)
  end)(E)

  module Make5(M: sig
    include S5
    include Representable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
  end)(E: Examples.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) M.t) = Make0(struct
    include Specialize5(M)(E.A)(E.B)(E.C)(E.D)(E.E)
    include (Representable.Specialize5(M)(E.A)(E.B)(E.C)(E.D)(E.E): Representable.S0 with type t := t)
  end)(E)
end
