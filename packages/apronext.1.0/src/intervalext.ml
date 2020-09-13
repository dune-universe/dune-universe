open Apron
module S = Scalarext

include Interval

let join a b = {
    inf = if Scalar.cmp a.inf b.inf < 0 then a.inf else b.inf;
    sup = if Scalar.cmp a.sup b.sup > 0 then a.sup else b.sup;
  }

let to_float a = (S.to_float a.inf),(S.to_float a.sup)

let to_mpqf a = (S.to_mpqf a.inf),(S.to_mpqf a.sup)

(** scalar range of an interval *)
let range a = S.sub a.sup a.inf

(** same as range but result as an mpqf *)
let range_mpqf a = S.sub a.sup a.inf |> S.to_mpqf

(** midpoint of an interval *)
let mid a = S.add a.inf (S.div (S.sub a.sup a.inf) (S.of_int 2))

(** Random uniform value within an interval, according to the type *)
let spawn ({inf;sup}:t) =
  let inf = S.to_mpqf inf and sup = S.to_mpqf sup in
  let r = Mpqf.of_float (Random.float 1.) in
  Mpqf.add inf (Mpqf.mul (Mpqf.sub sup inf) r)
