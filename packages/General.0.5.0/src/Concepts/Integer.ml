#include "Integer.signatures.ml"

module Tests = struct
  open Testing

  module Make0(M: S0)(E: sig
    include RealNumber.Tests.Examples.S0 with type t := M.t
    include Traits.PredSucc.Tests.Examples.S0 with type t := M.t
  end): sig val test: Test.t end = struct
    open M

    module E = struct
      include E

      let succ = succ @ [
        (zero, one);
      ]
    end

    let test = "Integer" >:: [
      (let module T = RealNumber.Tests.Make0(M)(E) in T.test);
      (let module T = Traits.PredSucc.Tests.Make0(M)(E) in T.test);
    ]
  end
end
