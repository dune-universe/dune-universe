let rec fact (x : Z.t) =
  if Z.equal x Z.zero then Z.one
  else if Z.equal x Z.one then Z.one
  else Z.mul x (fact (Z.pred x))
