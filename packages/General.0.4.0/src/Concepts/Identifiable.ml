#include "Identifiable.signatures.ml"

module Tests = struct
  open Testing

  module Examples = struct
    module type S0 = sig
      type t

      include Traits.Equatable.Tests.Examples.S0 with type t := t
      include Traits.Representable.Tests.Examples.S0 with type t := t
    end

    module type Element = sig
      type t

      include Traits.Equatable.Tests.Examples.Element with type t := t
      include Traits.Representable.Tests.Examples.Element with type t := t
    end

    module type S1 = sig
      type 'a t

      module A: Element

      include Traits.Equatable.Tests.Examples.S1 with type 'a t := 'a t and module A := A
      include Traits.Representable.Tests.Examples.S1 with type 'a t := 'a t and module A := A
    end

    module type S2 = sig
      type ('a, 'b) t

      module A: Element
      module B: Element

      include Traits.Equatable.Tests.Examples.S2 with type ('a, 'b) t := ('a, 'b) t and module A := A and module B := B
      include Traits.Representable.Tests.Examples.S2 with type ('a, 'b) t := ('a, 'b) t and module A := A and module B := B
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      module A: Element
      module B: Element
      module C: Element

      include Traits.Equatable.Tests.Examples.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t and module A := A and module B := B and module C := C
      include Traits.Representable.Tests.Examples.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t and module A := A and module B := B and module C := C
    end

    module type S4 = sig
      type ('a, 'b, 'c, 'd) t

      module A: Element
      module B: Element
      module C: Element
      module D: Element

      include Traits.Equatable.Tests.Examples.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t and module A := A and module B := B and module C := C and module D := D
      include Traits.Representable.Tests.Examples.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t and module A := A and module B := B and module C := C and module D := D
    end

    module type S5 = sig
      type ('a, 'b, 'c, 'd, 'e) t

      module A: Element
      module B: Element
      module C: Element
      module D: Element
      module E: Element

      include Traits.Equatable.Tests.Examples.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t and module A := A and module B := B and module C := C and module D := D and module E := E
      include Traits.Representable.Tests.Examples.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t and module A := A and module B := B and module C := C and module D := D and module E := E
    end
  end

  module Make0(M: S0)(E: Examples.S0 with type t := M.t) = struct
    let test = "Identifiable" >:: [
      (let module T = Traits.Equatable.Tests.Make0(M)(E) in T.test);
      (let module T = Traits.Representable.Tests.Make0(M)(E) in T.test);
    ]
  end

  module Make1(M: S1)(E: Examples.S1 with type 'a t := 'a M.t) = struct
    let test = "Identifiable" >:: [
      (let module T = Traits.Equatable.Tests.Make1(M)(E) in T.test);
      (let module T = Traits.Representable.Tests.Make1(M)(E) in T.test);
    ]
  end

  module Make2(M: S2)(E: Examples.S2 with type ('a, 'b) t := ('a, 'b) M.t) = struct
    let test = "Identifiable" >:: [
      (let module T = Traits.Equatable.Tests.Make2(M)(E) in T.test);
      (let module T = Traits.Representable.Tests.Make2(M)(E) in T.test);
    ]
  end

  module Make3(M: S3)(E: Examples.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t) = struct
    let test = "Identifiable" >:: [
      (let module T = Traits.Equatable.Tests.Make3(M)(E) in T.test);
      (let module T = Traits.Representable.Tests.Make3(M)(E) in T.test);
    ]
  end

  module Make4(M: S4)(E: Examples.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) M.t) = struct
    let test = "Identifiable" >:: [
      (let module T = Traits.Equatable.Tests.Make4(M)(E) in T.test);
      (let module T = Traits.Representable.Tests.Make4(M)(E) in T.test);
    ]
  end

  module Make5(M: S5)(E: Examples.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) M.t) = struct
    let test = "Identifiable" >:: [
      (let module T = Traits.Equatable.Tests.Make5(M)(E) in T.test);
      (let module T = Traits.Representable.Tests.Make5(M)(E) in T.test);
    ]
  end
end
