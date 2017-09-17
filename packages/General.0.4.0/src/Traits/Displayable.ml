#include "Displayable.signatures.ml"

module Tests = struct
  open Testing

  module Examples = struct
    module type S0 = sig
      type t

      val to_string: (t * string) list
    end
  end

  module Make0(M: S0)(E: Examples.S0 with type t := M.t) = struct
    open M

    let test = "Displayable" >:: (
      E.to_string
      |> List.map ~f:(fun (v, expected) ->
        ~: "to_string %s" expected (lazy (check_string ~expected (to_string v)))
      )
    )
  end
end
