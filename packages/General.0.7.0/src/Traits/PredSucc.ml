#include "PredSucc.signatures.ml"

module Make0(M: sig
  type t

  val one: t

  val add: t -> t -> t
  val substract: t -> t -> t
end) = struct
  open M

  let pred x =
    substract x one

  let succ x =
    add x one
end

module Tests = struct
  open Testing

  module Examples = struct
    module type S0 = sig
      type t
      
      val succ: (t * t) list
    end
  end

  module Make0(M: sig
    include S0
    include Equatable.Basic.S0 with type t := t
    include Representable.S0 with type t := t
  end)(E: Examples.S0 with type t := M.t) = struct
    open M

    let test = "PredSucc" >:: (
      E.succ
      |> List.flat_map ~f:(fun (p, s) ->
        let rp = repr p and rs = repr s in
        [
          ~: "succ %s" rp (lazy (check ~repr ~equal ~expected:s (succ p)));
          ~: "pred %s" rs (lazy (check ~repr ~equal ~expected:p (pred s)));
        ]
      )
    )
  end
end
