(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

module Make(C: JsOfOCairo.S) = struct
  let draw ctx =
    C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
    C.stroke ctx
end
